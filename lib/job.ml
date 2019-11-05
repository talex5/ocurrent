open Lwt.Infix

module Map = Map.Make(String)

(* For unit-tests: *)
let timestamp = ref Unix.gettimeofday
let sleep = ref Lwt_unix.sleep

type t = {
  switch : Switch.t;
  config : Config.t;
  id : string;
  set_start_time : float Lwt.u;
  start_time : float Lwt.t;
  mutable ch : out_channel option;
  log_cond : unit Lwt_condition.t;  (* Fires whenever log data is written, or log is closed. *)
  explicit_confirm : unit Lwt.t;
  set_explicit_confirm : unit Lwt.u; (* Resolve this to override the global confirmation threshold. *)
  mutable waiting_for_confirmation : bool;  (* Is calling [approve_early_start] useful? *)
}

let jobs = ref Map.empty

let open_temp_file ~dir ~prefix ~suffix =
  let path, ch = Filename.open_temp_file ~temp_dir:(Fpath.to_string dir) prefix suffix in
  Fpath.v path, ch

let write t msg =
  match t.ch with
  | None -> Log.err (fun f -> f "Job.write(%s, %S) called on closed job" t.id msg)
  | Some ch ->
    output_string ch msg;
    flush ch;
    Lwt_condition.broadcast t.log_cond ()

let log t fmt =
  let { Unix.tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec; _ } =
    !timestamp () |> Unix.gmtime in
  let fmt = "%04d-%02d-%02d %02d:%02d.%02d: @[" ^^ fmt ^^ "@]@." in
  Fmt.kstrf (write t) fmt
    (tm_year + 1900) (tm_mon + 1) tm_mday
    tm_hour tm_min tm_sec

let id t = t.id

let jobs_dir = lazy (Disk_store.state_dir "job")

let log_path job_id =
  let open Astring in
  let jobs_dir = Lazy.force jobs_dir in
  match String.cuts ~sep:"/" job_id with
  | [date; file] when
      not (String.is_prefix ~affix:"." date) &&
      not (String.is_prefix ~affix:"." file) ->
    let path = Fpath.(jobs_dir / date / (file ^ ".log")) in
    begin match Bos.OS.File.exists path with
      | Ok true -> Ok path
      | Ok false -> Error (`Msg (Fmt.strf "Job log %a does not exist" Fpath.pp path))
      | Error _ as e -> e
    end
  | _ -> Error (`Msg (Fmt.strf "Invalid job ID %S" job_id))

let id_of_path path =
  match Fpath.split_base path with
  | parent_dir, leaf ->
    Fpath.(base parent_dir // leaf) |> Fpath.to_string |> Filename.chop_extension

let create ~switch ~label ~config () =
  if not (Switch.is_on switch) then Fmt.failwith "Switch %a is not on! (%s)" Switch.pp switch label;
  let jobs_dir = Lazy.force jobs_dir in
  let time = !timestamp () |> Unix.gmtime in
  let date =
    let { Unix.tm_year; tm_mon; tm_mday; _ } = time in
    Fmt.strf "%04d-%02d-%02d" (tm_year + 1900) (tm_mon + 1) tm_mday
  in
  let date_dir = Fpath.(jobs_dir / date) in
  match Bos.OS.Dir.create date_dir with
  | Error (`Msg m) -> failwith m
  | Ok (_ : bool) ->
    let prefix =
      let { Unix.tm_hour; tm_min; tm_sec; _ } = time in
      Fmt.strf "%02d%02d%02d-%s-" tm_hour tm_min tm_sec label
    in
    let path, ch = open_temp_file ~dir:date_dir ~prefix ~suffix:".log" in
    Log.info (fun f -> f "Created new log file at@ %a" Fpath.pp path);
    let id = id_of_path path in
    let start_time, set_start_time = Lwt.wait () in
    let log_cond = Lwt_condition.create () in
    let explicit_confirm, set_explicit_confirm = Lwt.wait () in
    let t = { switch; id; ch = Some ch; start_time; set_start_time; config; log_cond;
              explicit_confirm; set_explicit_confirm; waiting_for_confirmation = false } in
    jobs := Map.add id t !jobs;
    Switch.add_cancel_hook_or_fail switch (fun reason ->
        log t "Cancelled: %s" reason;
        Lwt.return_unit
      );
    Switch.add_release_hook_or_fail switch (fun () ->
        close_out ch;
        t.ch <- None;
        jobs := Map.remove id !jobs;
        Lwt_condition.broadcast t.log_cond ();
        Lwt.return_unit
      );
    t

let pp_id = Fmt.string

let is_running t = Lwt.state t.start_time <> Lwt.Sleep

let confirm t ?pool level =
  let confirmed =
    let confirmed = Config.confirmed level t.config in
    Switch.add_cancel_hook_or_fail t.switch (fun _ -> Lwt.cancel confirmed; Lwt.return_unit);
    match Lwt.state confirmed with
    | Lwt.Return () -> Lwt.return_unit
    | _ ->
      log t "Waiting for confirm-threshold > %a" Level.pp level;
      Log.info (fun f -> f "Waiting for confirm-threshold > %a" Level.pp level);
      t.waiting_for_confirmation <- true;
      Lwt.choose [confirmed; t.explicit_confirm] >>= fun () ->
      t.waiting_for_confirmation <- false;
      if Lwt.state confirmed <> Lwt.Sleep then (
        log t "Confirm-threshold now > %a" Level.pp level;
        Log.info (fun f -> f "Confirm-threshold now > %a" Level.pp level)
      );
      if Lwt.state t.explicit_confirm <> Lwt.Sleep then (
        log t "Explicit approval received for this job"
      );
      Lwt.return_unit
  in
  confirmed >>= fun () ->
  match pool with
  | None -> Lwt.return_unit
  | Some pool ->
    let res = Pool.get ~switch:t.switch pool in
    if Lwt.is_sleeping res then (
      log t "Waiting for resource in pool %a" Pool.pp pool;
      res >|= fun () ->
      log t "Got resource from pool %a" Pool.pp pool
    ) else res

let start ?timeout ?pool ~level t =
  confirm t ?pool level >|= fun () ->
  if is_running t then (
    Log.warn (fun f -> f "start called, but job %s is already running!" t.id);
    Fmt.failwith "Job.start called twice!"
  );
  Lwt.wakeup t.set_start_time (!timestamp ());
  Option.iter (Switch.add_timeout t.switch) timeout

let start_time t = t.start_time

let wait_for_log_data t = Lwt_condition.wait t.log_cond

let lookup_running id = Map.find_opt id !jobs

let is_waiting_for_confirmation t = t.waiting_for_confirmation

let approve_early_start t =
  match Lwt.state t.explicit_confirm with
  | Lwt.Sleep -> Lwt.wakeup t.set_explicit_confirm ()
  | Lwt.Return () -> ()
  | Lwt.Fail ex -> raise ex
