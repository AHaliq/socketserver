open Unix;;
open Printer;;
open Frameutils;;
open Connections;;
open Settings;;

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
  try
    setup_core_server ();
    print "core successfully started\n";
    (try
      while service_cmd () do
        (try service_connections ()
        with e -> printerr "connection response error\n" e)
      done
    with e -> printerr "cmd response error\n" e);
    close_socket !core_server_fd
  with e -> printerr "fail to create core server\n" e

(* CORE PROCESSES ---------------------------- *)

let all_commands = [
  {
    name = "core";
    arghelp = "";
    description = "start core process";
    cmd_func = (fun () ->
      if !core_client_connected
      then print "core_process is already running.\n"
      else core_process ()
    );
    core_func = (fun _ -> ())
  };
  {
    name = "ls";
    arghelp = "";
    description = "list socket connections";
    cmd_func = (fun () -> print "hello im working.\n");
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