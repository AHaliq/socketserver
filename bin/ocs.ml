open Unix;;
open Settings;;
open Printer;;
open Frameutils;;

let core_client_fd = ref None
let core_server_fd = ref None
let core_client_connected = ref false

(* VARIABLES --------------------------------- *)

type command =
  {
    (* command name to invoke as cli argument *)
    name        : string;
    (* argument hints for help menu *)
    arghelp     : string;
    (* description for help menu *)
    description : string;
    (* function to execute when cli argument is command *)
    cmd_func    : unit -> unit;
    (* check if payload matches and execute response; used by core_process *)
    core_func   : bytes -> unit
  }

(* TYPES ------------------------------------- *)

let connect_core_client () =
  try
    let fd = (socket PF_UNIX SOCK_STREAM 0) in
    core_client_fd := Some fd;
    (try unlink core_sock_client with _ -> ());
    bind fd (ADDR_UNIX core_sock_client);
    connect fd (ADDR_UNIX core_sock_server);
    core_client_connected := true
  with _ -> ()
  (*with e -> printerr "" e*)

(* max 256 characters *)
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

let setup_core_server () =
  let fd = (socket PF_UNIX SOCK_STREAM 0) in
  core_server_fd := Some fd;
  (try unlink core_sock_server with _ -> ());
  bind fd (ADDR_UNIX core_sock_server);
  listen fd 1

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

(* SOCKET FUNCTIONS -------------------------- *)

let service_cmd () =
  (match !core_server_fd with
  | None -> false
  | Some fd ->
    let (rs, _, _) = select [fd] [] [] poll_time in
    (match rs with
    | [] -> ()
    | _ -> 
      let (cfd, _) = accept fd in
      (*
      let btslen = Bytes.create 1 in
      recv cfd btslen 0 1 [] |> ignore;
      let len = Char.code (Bytes.get btslen 0) in
      let btsmsg = Bytes.create len in
      recv cfd btsmsg 0 len [] |> ignore;
      print (Bytes.to_string btsmsg);*)
      close cfd
    );
    true);;

let service_connections () = ()

let core_process () =
  if !core_client_connected
  then print "core is already running.\n"
  else
    (try
      setup_core_server ();
      print "core successfully started\n";
      (try
        while service_cmd () do
          (try service_connections ()
          with e -> printerr "connection response error\n" e)
        done
      with e -> printerr "cmd response error\n" e);
      close_socket !core_server_fd
    with e -> printerr "fail to create core server\n" e)

(* CORE PROCESSES ---------------------------- *)

let all_commands = [
  {
    name = "core";
    arghelp = "";
    description = "start core process";
    cmd_func = core_process;
    core_func = (fun _ -> ())
  };
  {
    name = "ls";
    arghelp = "";
    description = "list socket connections";
    cmd_func = (fun () -> ());
    core_func = (fun b ->
      if match_cmd_payload_type 0 b
      then print "TODO! print connections\n"
      else ()
    )
  };
  {
    name = "open";
    arghelp = "<port>";
    description = "starts a socket server at specified port";
    cmd_func = (fun () -> ());
    core_func = (fun b -> 
      if match_cmd_payload_type 1 b
      then print "TODO! open port\n"
      else ()
    )
  };
  {
    name = "connect";
    arghelp = "<ip>:<port>";
    description = "connect to a socket at specified address";
    cmd_func = (fun () -> ());
    core_func = (fun b -> 
      if match_cmd_payload_type 2 b
      then print "TODO! connect ip:port\n"
      else ()
    )
  };
  {
    name = "sendstring";
    arghelp = "<name> <string>";
    description = "sends a string to the name socket connection";
    cmd_func = (fun () -> ());
    core_func = (fun b -> 
      if match_cmd_payload_type 3 b
      then print "TODO! connect ip:port\n"
      else ()
    )
  };
  {
    name = "close";
    arghelp = "<name>";
    description = "closes a socket connection or soket server of the name";
    cmd_func = (fun () -> ());
    core_func = (fun b -> 
      if match_cmd_payload_type 4 b
      then print "TODO! connect ip:port\n"
      else ()
    )
  };
  {
    name = "exit";
    arghelp = "";
    description = "closes all sockets and kill core process";
    cmd_func = (fun () -> ());
    core_func = (fun b -> 
      if match_cmd_payload_type 5 b
      then print "TODO! connect ip:port\n"
      else ()
    )
  }]

let help_text =
  let indent = "        " in
  List.fold_left (
    fun a x ->
    a ^ indent ^ "ocs " ^ x.name ^ " " ^ x.arghelp ^ "\n" ^
    indent ^ indent ^ "- " ^ x.description ^ "\n\n"
  ) "usage:\n" all_commands

let ocs () =
  let arg_len = Array.length Sys.argv - 1 in
  (if arg_len == 0
    then print help_text
    else
      let arg_cmd = Sys.argv.(1) in
      match List.filter_map
        (fun x -> if (x.name = arg_cmd) then (Some x) else None)
        all_commands with
      | []    -> print ("unknown command '" ^ arg_cmd ^ "'\n"); print help_text
      | x::_ ->
        connect_core_client ();
        x.cmd_func ();
        if !core_client_connected
        then close_socket !core_client_fd
        else ();
  );;

handle_unix_error ocs ();;