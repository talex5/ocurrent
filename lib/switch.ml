(** Like [Lwt_switch], but the cleanup functions are called in sequence, not
    in parallel, and a reason for the shutdown may be given. *)

open Lwt.Infix

type reason = string

type cancel = string -> unit Lwt.t
type release = unit -> unit Lwt.t

type t = {
  label : string;
  mutable state : [
    | `On of cancel Stack.t * release Stack.t
    | `Cancelling of reason * unit Lwt.t * release Stack.t       (* Resolves once cancelled *)
    | `Cancelled of reason * release Stack.t
    | `Turned_off of [ `Releasing of unit Lwt.t | `Shutdown ]
  ];
}

let pp_reason = Fmt.(option ~none:(unit "Success") string)

let require_on t f =
  match t.state with
  | `Turned_off _ -> Fmt.failwith "Switch %S has been turned off!" t.label
  | `Cancelling _ | `Cancelled _ | `On _ as x -> f x

let rec turn_off t =
  require_on t @@ function
  | `Cancelling (_, thread, _) ->
    (* Wait for cancel to finish first. *)
    thread >>= fun () ->
    turn_off t
  | `On (_, on_release) | `Cancelled (_, on_release) ->
    let th, set_th = Lwt.wait () in
    t.state <- `Turned_off (`Releasing th);
    let rec aux () =
      match Stack.pop on_release with
      | fn -> fn () >>= aux
      | exception Stack.Empty ->
        t.state <- `Turned_off `Shutdown;
        Lwt.wakeup set_th ();
        Lwt.return_unit
    in
    aux ()

let cancel t reason =
  require_on t @@ function
  | `Cancelling (orig, thread, _) ->
    Log.info (fun f -> f "Switch.cancel(%s, %S): already cancelled: %S" t.label reason orig);
    thread
  | `Cancelled (orig, _) ->
    Log.info (fun f -> f "Switch.cancel(%s, %S): already cancelled: %S" t.label reason orig);
    Lwt.return_unit
  | `On (on_cancel, on_release) ->
    let th, set_th = Lwt.wait () in
    t.state <- `Cancelling (reason, th, on_release);
    let rec aux () =
      match Stack.pop on_cancel with
      | fn -> fn reason >>= aux
      | exception Stack.Empty ->
        t.state <- `Cancelled (reason, on_release);
        Lwt.wakeup set_th ();
        Lwt.return_unit
    in
    aux ()

(* Once the first callback is added, attach a GC finaliser so we can detect if the user
   forgets to turn it off. *)
let gc t =
  match t.state with
  | `Turned_off _ -> ()
  | `On _ | `Cancelling _ | `Cancelled _ ->
    Log.err (fun f -> f "Switch %S GC'd without being turned off!" t.label);
    Lwt.async (fun () -> turn_off t)

let add_cancel_hook_or_fail t fn =
  require_on t @@ function
  | `On (on_cancel, _) -> Stack.push fn on_cancel
  | `Cancelling (reason, _, _) | `Cancelled (reason, _) -> Fmt.failwith "Switch was cancelled (%s)" reason

let add_cancel_hook_or_exec t fn =
  match t.state with
  | `On (on_cancel, _) ->
    Stack.push fn on_cancel;
    Lwt.return_unit
  | `Cancelled (reason, _) ->
    fn reason
  | `Cancelling (reason, thread, _) ->
    thread >>= fun () ->
    fn reason
  | `Turned_off `Shutdown -> fn (Fmt.strf "Switch %S has been turned off!" t.label)
  | `Turned_off (`Releasing th) -> th >>= fun () -> fn (Fmt.strf "Switch %S has been turned off!" t.label)

let add_release_hook_or_exec t fn =
  match t.state with
  | `On (_, on_release)
  | `Cancelling (_, _, on_release)
  | `Cancelled (_, on_release) ->
    if Stack.is_empty on_release then Gc.finalise gc t;
    Stack.push fn on_release;
    Lwt.return_unit
  | `Turned_off `Shutdown -> fn ()
  | `Turned_off (`Releasing th) -> th >>= fn

let add_release_hook_or_fail t fn =
  require_on t @@ function
  | `On (_, on_release)
  | `Cancelling (_, _, on_release)
  | `Cancelled (_, on_release) ->
    if Stack.is_empty on_release then Gc.finalise gc t;
    Stack.push fn on_release

let add_cancel_hook_or_exec_opt t fn =
  match t with
  | None -> Lwt.return_unit
  | Some t -> add_cancel_hook_or_exec t fn

let create ~label () = {
  label;
  state = `On (Stack.create (), Stack.create ());
}

let create_off label = {
  label;
  state = `Turned_off `Shutdown;
}

let is_on t =
  match t.state with
  | `On _ -> true
  | `Turned_off _ | `Cancelling _ | `Cancelled _ -> false

let pp f t =
  match t.state with
  | `On _ -> Fmt.pf f "on(%s)" t.label
  | `Cancelling (r, _, _) -> Fmt.pf f "cancelling(%s, %s)" t.label r
  | `Cancelled (r, _) -> Fmt.pf f "cancelled(%s, %s)" t.label r
  | `Turned_off (`Releasing _) -> Fmt.pf f "turned-off(%s, releasing)" t.label
  | `Turned_off `Shutdown -> Fmt.pf f "turning-off(%s, shutdown)" t.label

let pp_duration f d =
  let d = Duration.to_f d in
  if d > 120.0 then Fmt.pf f "%.1f minutes" (d /. 60.)
  else if d > 2.0 then Fmt.pf f "%.1f seconds" d
  else Fmt.pf f "%f seconds" d

let add_timeout t duration =
  (* We could be smarter about this. e.g. cancel the timeout when the switch is turned off or
     only keep the nearest timeout. *)
  Lwt.async (fun () ->
      Lwt_unix.sleep (Duration.to_f duration) >>= fun () ->
      if is_on t then cancel t @@ Fmt.strf "Timeout (%a)" pp_duration duration
      else Lwt.return_unit
    )

let ensure_on t =
  require_on t ignore
