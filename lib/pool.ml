open Lwt.Infix

module Metrics = struct
  open Prometheus

  let namespace = "ocurrent"
  let subsystem = "pool"

  let qlen =
    let help = "Number of users waiting for a resource" in
    Gauge.v_label ~help ~label_name:"name" ~namespace ~subsystem "qlen"

  let wait_time =
    let help = "Time spent waiting for a resource" in
    Summary.v_label ~help ~label_name:"name" ~namespace ~subsystem
      "wait_time_seconds"

  let use_time =
    let help = "Time spent using a resource" in
    Summary.v_label ~help ~label_name:"name" ~namespace ~subsystem
      "use_time_seconds"

  let resources_in_use =
    let help = "Number of resources currently being used" in
    Gauge.v_label ~help ~label_name:"name" ~namespace ~subsystem
      "resources_in_use"

  let capacity =
    let help = "Total pool capacity" in
    Gauge.v_label ~help ~label_name:"name" ~namespace ~subsystem "capacity"
end

class type ['a] offer = object
  method accept : Switch.t -> 'a Lwt.t
  method decline : unit
end

class type ['a] t = object
  method get : ('a offer, [`Busy of string * unit Lwt.t]) result Lwt.t
  method pp : Format.formatter -> unit
end

let create ~label capacity =
  let () = Prometheus.Gauge.set (Metrics.capacity label) (float_of_int capacity) in
  object (_ : unit #t)
    val mutable used = 0
    val queue : (float * unit Lwt.u) Lwt_dllist.t = Lwt_dllist.create ();

    method get =
      if used < capacity then (
        used <- used + 1;
        Prometheus.Gauge.inc_one (Metrics.resources_in_use label);
        Lwt_result.return @@ object (_ : unit #offer)
          method accept switch =
            let start_use_time = Unix.gettimeofday () in
            Switch.add_hook_or_exec switch (fun () ->
                Prometheus.Gauge.dec_one (Metrics.resources_in_use label);
                used <- used - 1;
                let release_time = Unix.gettimeofday () in
                Prometheus.Summary.observe (Metrics.use_time label) (release_time -. start_use_time);
                begin match Lwt_dllist.take_opt_l queue with
                  | None -> ()
                  | Some (start_wait_time, waiter) ->
                    Prometheus.Summary.observe (Metrics.wait_time label) (start_use_time -. start_wait_time);
                    Prometheus.Gauge.dec_one (Metrics.qlen label);
                    Lwt.wakeup_later waiter ()
                end;
                Lwt.return_unit
              )

          method decline = ()
        end
      ) else (
        let start_wait_time = Unix.gettimeofday () in
        let ready, set_ready = Lwt.task () in
        let node = Lwt_dllist.add_r (start_wait_time, set_ready) queue in
        Prometheus.Gauge.inc_one (Metrics.qlen label);
        Lwt.on_cancel ready (fun () ->
            if Lwt.is_sleeping ready then (
              Prometheus.Gauge.dec_one (Metrics.qlen label);
              Lwt_dllist.remove node;
              Lwt.wakeup_later set_ready ()
            )
          );
        let msg = Fmt.strf "Waiting for resource from pool %S" label in
        Lwt_result.fail (`Busy (msg, ready))
      )

    method pp f = Fmt.string f label
  end

let ( >>!= ) = Lwt_result.bind

let pair (a : 'a t) (b : 'b t) : ('a * 'b) t =
  object
    method get =
      a#get >>!= fun a_offer ->
      b#get >>= function
      | Error _ as e ->
        a_offer#decline;
        Lwt.return e
      | Ok b_offer ->
        Lwt_result.return @@ object
          method decline =
            a_offer#decline;
            b_offer#decline

          method accept switch =
            a_offer#accept switch >>= fun av ->
            b_offer#accept switch >>= fun bv ->
            Lwt.return (av, bv)
        end

    method pp f = Fmt.pf f "(%t,%t)" a#pp b#pp
  end

let both (a : unit t) (b : unit t) : unit t =
  let p = pair a b in
  object
    method get =
      p#get >>!= fun offer ->
      Lwt_result.return @@ object
        method accept switch =
          offer#accept switch >|= fun ((), ()) -> ()

        method decline = offer#decline
      end

    method pp f = Fmt.pf f "(%t & %t)" a#pp b#pp
  end

let either (a : 'a t) (b : 'a t) : 'a t =
  object
    method get =
      a#get >>= function
      | Ok a_offer -> Lwt_result.return a_offer
      | Error (`Busy (a_msg, a_ready)) ->
        b#get >>= function
        | Ok b_offer ->
          Lwt.cancel a_ready;
          Lwt_result.return b_offer
        | Error (`Busy (b_msg, b_ready)) ->
          let ready = Lwt.pick [a_ready; b_ready] in
          let msg = Fmt.strf "(%s | %s)" a_msg b_msg in
          Lwt_result.fail (`Busy (msg, ready))

    method pp f = Fmt.pf f "(%t | %t)" a#pp b#pp
  end

let pp f (t:_ t) = t#pp f
