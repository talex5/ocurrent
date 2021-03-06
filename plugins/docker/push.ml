open Lwt.Infix

type auth = string * string

type t = auth option

let ( >>!= ) = Lwt_result.bind

let id = "docker-push"

module Key = struct
  type t = {
    tag : string;
    docker_context : string option;
  } [@@deriving to_yojson]

  let digest t = Yojson.Safe.to_string (to_yojson t)
end

module Value = struct
  type t = {
    image : Image.t;
  }

  let digest { image } =
    Yojson.Safe.to_string @@ `Assoc [
      "image", `String (Image.hash image);
    ]
end

module Outcome = Current.String (* [S.repo_id] *)

let tag_cmd { Key.tag; docker_context } { Value.image } =
  Cmd.docker ~docker_context ["tag"; Image.hash image; tag]

let publish ~switch auth job key value =
  Current.Process.exec ~switch ~job (tag_cmd key value) >>= function
  | Error _ as e -> Lwt.return e
  | Ok () ->
    let { Key.tag; docker_context } = key in
    begin match auth with
      | None -> Lwt.return (Ok ())
      | Some (user, password) ->
        let cmd = Cmd.login ~docker_context user in
        Current.Process.exec ~switch ~job ~stdin:password cmd
    end >>!= fun () ->
    let cmd = Cmd.docker ~docker_context ["push"; tag] in
    Current.Process.exec ~switch ~job cmd >>!= fun () ->
    let cmd = Cmd.docker ~docker_context ["image"; "inspect"; tag; "-f"; "{{index .RepoDigests 0}}"] in
    Current.Process.check_output ~job cmd >|= Stdlib.Result.map @@ fun id ->
    let repo_id = String.trim id in
    Current.Job.log job "Pushed %S -> %S" tag repo_id;
    repo_id

let pp f (key, value) =
  Fmt.pf f "%a; docker push %S"
    Cmd.pp (tag_cmd key value)
    key.Key.tag

let auto_cancel = false

let level _auth _tag _value = Current.Level.Dangerous
