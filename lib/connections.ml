open Unix;;
open Printer;;
open Settings;;
open Frameutils;;

let core_server_fd = ref None
let core_client_fd = ref None
let core_client_connected = ref false

let attempt_unlink file = try unlink file with _ -> ()

let connect_core_client () =
  try
    let fd = (socket PF_UNIX SOCK_STREAM 0) in
    core_client_fd := Some fd;
    attempt_unlink core_sock_client;
    bind fd (ADDR_UNIX core_sock_client);
    connect fd (ADDR_UNIX core_sock_server);
    core_client_connected := true
  with _ -> ()
  (*with e -> printerr "" e*)

let setup_core_server () =
  let fd = (socket PF_UNIX SOCK_STREAM 0) in
  core_server_fd := Some fd;
  attempt_unlink core_sock_server;
  bind fd (ADDR_UNIX core_sock_server);
  listen fd 1

(* DEPRECATE to be deleted; max 256 characters *)
let send_core_client str =
  match !core_client_fd with
  | Some fd -> 
  let bytes_str = Bytes.of_string str in
  let str_len = Bytes.length bytes_str in
  let bytes_len = Bytes.create 1 in
  Bytes.set bytes_len 0 (Char.chr str_len);
  send fd bytes_len 0 1 [] |> ignore;
  send fd bytes_str 0 str_len [] |> ignore;
  | None -> ()

let send_bytes_to_core b =
  match !core_client_fd with
  | Some fd ->
    let frames = generate_frames b in
    List.fold_left (fun () frame ->
      send fd frame 0 (Bytes.length frame) [] |> ignore
    ) () frames
  | None -> print "unable to communicate, core_process not started.\n"

let close_socket ofd =
  try
    match ofd with
    | Some fd -> close fd
    | None    -> ()
  with _ -> ()