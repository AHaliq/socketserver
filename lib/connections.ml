open Unix;;
open Printer;;
open Settings;;
open Numutils;;
open Frameutils;;

(* TCP CONNECTIONS --------------------------- *)

let core_server_fd = ref None
let core_client_fd = ref None
let core_client_connected = ref false

(* CORE CONNECTIONS -------------------------- *)

let attempt_unlink file = try unlink file with _ -> ()

let send_bytes_as_frames_to_fd fd b =
  let frames = generate_frames b in
  List.fold_left (fun () frame ->
    send fd frame 0 (Bytes.length frame) [] |> ignore
  ) () frames

let recv_frames_as_bytes fd =
  let moreframes = ref true in
  let payloads = ref [] in
  (while !moreframes do
    let bytes_cont = Bytes.create 1 in
    recv fd bytes_cont 0 1 [] |> ignore;
    let cont = Bytes.get bytes_cont 0 == Char.chr 1 in
    let bytes_len = Bytes.create frame_len_bytes in
    recv fd bytes_len 0 frame_len_bytes [] |> ignore;
    let len = int_of_bytes bytes_len in
    let payload = Bytes.create len in
    recv fd payload 0 len [] |> ignore;
    payloads := List.cons payload !payloads;
    moreframes := cont
  done);
  payloads := List.rev !payloads;
  Bytes.concat (Bytes.create 0) !payloads

let close_socket ofd =
  try
    match ofd with
    | Some fd -> close fd
    | None    -> ()
  with _ -> ()

(* GENERAL FUNCTIONS ------------------------- *)

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

let close_core_client () =
  if !core_client_connected
  then close_socket !core_client_fd
  else ()

let setup_core_server () =
  let fd = (socket PF_UNIX SOCK_STREAM 0) in
  core_server_fd := Some fd;
  attempt_unlink core_sock_server;
  bind fd (ADDR_UNIX core_sock_server);
  listen fd 1

let send_bytes_to_core b =
  match !core_client_fd with
  | Some fd when !core_client_connected -> send_bytes_as_frames_to_fd fd b
  | _ -> print "core_process not started.\nrun `ocs core` first.\n"

let recv_bytes_from_core () = 
  match !core_client_fd with
  | Some fd when !core_client_connected -> recv_frames_as_bytes fd
  | _ -> Bytes.create 0

(* CORE FUNCTIONS ---------------------------- *)