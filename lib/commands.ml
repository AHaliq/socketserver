open Unix;;
open Printer;;
open Frameutils;;
open Connections;;
open Settings;;
open Numutils;;

let wrong_args_text = "wrong number of arguments. see usages `ocs help`.\n"

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
    core_func   : file_descr -> bytes -> unit
  }

(* CORE SUB MODULE --------------------------- *)

let coreexit = ref false

let rec all_commands = [
  {
    name = "core";
    arghelp = "";
    description = "start core process";
    cmd_func = (
    let service_cmd () =
      (match !core_server_fd with
      | None -> ()
      | Some fd ->
        let (rs, _, _) = select [fd] [] [] poll_time in
        (match rs with
        | [] -> ()
        | _ -> 
          let (cfd, _) = accept fd in
          let payload = recv_frames_as_bytes cfd in
          List.fold_left (fun () x -> x.core_func cfd payload) () all_commands |> ignore;
          close cfd
        )) in
    let service_connections () = () in
    let core_process () = 
      (try
        setup_core_server ();
        print "core successfully started\n";
        (try
          while not !coreexit do
            (try service_connections ()
            with e -> printerr "connection response error\n" e);
            service_cmd ()
          done
        with e -> printerr "cmd response error\n" e);
        close_socket !core_server_fd
      with e -> printerr "fail to create core server\n" e) in
    fun () ->
      connect_core_client ();
      (if !core_client_connected
      then
        (send_bytes_to_core (bytes_of_int 0);
        print (Bytes.to_string (recv_bytes_from_core ())))
      else core_process ());
      close_core_client ()
    );
    core_func = (fun fd b -> 
      if match_cmd_payload_type 0 b
      then let returnmsg = "core_process is already running.\n" in
      send_bytes_as_frames_to_fd fd (Bytes.of_string returnmsg)
      else ()
    )
  };
  {
    name = "ls";
    arghelp = "";
    description = "list socket connections";
    cmd_func = (fun () ->
      connect_core_client ();
      send_bytes_to_core (bytes_of_int 1);
      print (Bytes.to_string (recv_bytes_from_core ()));
      close_core_client ()
    );
    core_func = (fun fd b ->
      if match_cmd_payload_type 1 b
      then let returnmsg = "TODO! print connections\n" in
      send_bytes_as_frames_to_fd fd (Bytes.of_string returnmsg)
      else ()
    )
  };
  {
    name = "open";
    arghelp = "<port>";
    description = "starts a socket server at specified port";
    cmd_func = (fun () ->
      if Array.length Sys.argv - 1 != 2 then
        print wrong_args_text
      else
        try
          let port = int_of_string Sys.argv.(2) in
          if port >= 0 && port <= 65536
          then
            let type_bytes = (bytes_of_int 2) in
            let port_bytes = pad_min_len 2 (bytes_of_int port) in
            connect_core_client ();
            send_bytes_to_core (Bytes.cat type_bytes port_bytes);
            print (Bytes.to_string (recv_bytes_from_core ()));
            close_core_client ()
          else print "port must be between 0 and 65536.\n"
        with _ -> print "port must be a number.\n"
    );
    core_func = (fun fd b -> 
      if match_cmd_payload_type 2 b
      then
        let port = int_of_bytes (Bytes.sub b 1 2) in
        let returnmsg = "TODO! make port " ^ (string_of_int port) ^ ".\n" in
        send_bytes_as_frames_to_fd fd (Bytes.of_string returnmsg)
      else ()
    )
  };
  {
    name = "connect";
    arghelp = "<ip>:<port>";
    description = "connect to a socket at specified address";
    cmd_func = (fun () -> 
      if Array.length Sys.argv - 1 != 2 then
        print wrong_args_text
      else
        match String.split_on_char ':' Sys.argv.(2) with
        | ip_str::port_str::[]  ->
          let port = int_of_string port_str in
          if port >= 0 && port <= 65536
          then (
            try
              inet_addr_of_string ip_str |> ignore;
              let type_bytes = (bytes_of_int 3) in
              let ipaddr_bytes = Bytes.of_string Sys.argv.(2) in
              connect_core_client ();
              send_bytes_to_core (Bytes.cat type_bytes ipaddr_bytes);
              print (Bytes.to_string (recv_bytes_from_core ()));
              close_core_client ()
            with _ -> print "not a valid ip address.\n")
          else print "port must be between 0 and 65536.\n"
        | _             -> print "argument must be <ip:port>.\n"
    );
    core_func = (fun fd b -> 
      if match_cmd_payload_type 3 b
      then
        let ipport_str = Bytes.to_string (Bytes.sub b 1 (Bytes.length b - 1)) in
        let returnmsg = "TODO! create connection.\n" in
        print ipport_str;
        send_bytes_as_frames_to_fd fd (Bytes.of_string returnmsg)
      else ()
    )
  };
  {
    name = "sendstring";
    arghelp = "<connection_id> <string>";
    description = "sends a string to the id socket connection";
    cmd_func = (fun () -> 
      if Array.length Sys.argv - 1 < 3 then
        print wrong_args_text
      else
        (try
        let wocmd = List.tl (List.tl (Array.to_list Sys.argv)) in
        let id = int_of_string (List.hd wocmd) in
        if id >= 0 && id <= 65536 then
          let str = String.trim (List.fold_left (fun a x -> a ^ " " ^ x) "" (List.tl wocmd)) in
          let type_bytes = (bytes_of_int 4) in
          let id_bytes = (pad_min_len 2 (bytes_of_int id)) in
          connect_core_client ();
          send_bytes_to_core (Bytes.cat type_bytes (Bytes.cat id_bytes (Bytes.of_string str)));
          print (Bytes.to_string (recv_bytes_from_core ()));
          close_core_client ()
        else
          print "id is not valid.\n"
        with _ -> print "id is not valid.\n")
    );
    core_func = (fun fd b -> 
      if match_cmd_payload_type 4 b
      then 
        let id = int_of_bytes (Bytes.sub b 1 2) in
        let str = Bytes.to_string (Bytes.sub b 3 (Bytes.length b - 3)) in
        let returnmsg = "TODO! send string.\n" in
        print ((string_of_int id) ^ ":" ^ str ^ "\n");
        send_bytes_as_frames_to_fd fd (Bytes.of_string returnmsg)
      else ()
    )
  };
  {
    name = "close";
    arghelp = "<connection_id>";
    description = "closes a socket connection or soket server of the id";
    cmd_func = (fun () ->
      if Array.length Sys.argv - 1 != 2 then
        print wrong_args_text
      else
        (try
        let id = int_of_string Sys.argv.(2) in
        if id >= 0 && id <= 65536 then
          let type_bytes = (bytes_of_int 5) in
          let id_bytes = (pad_min_len 2 (bytes_of_int id)) in
          connect_core_client ();
          send_bytes_to_core (Bytes.cat type_bytes id_bytes);
          print (Bytes.to_string (recv_bytes_from_core ()));
          close_core_client ()
        with _ -> print "id is not valid.\n")
    );
    core_func = (fun fd b -> 
      if match_cmd_payload_type 5 b
      then
        let id = int_of_bytes (Bytes.sub b 1 2) in
        let returnmsg = "TODO! close.\n" in
        print ((string_of_int id) ^ "\n");
        send_bytes_as_frames_to_fd fd (Bytes.of_string returnmsg)
      else ()
    )
  };
  {
    name = "exit";
    arghelp = "";
    description = "closes all sockets and kill core process";
    cmd_func = (fun () -> 
      connect_core_client ();
      send_bytes_to_core (bytes_of_int 6);
      close_core_client ()
    );
    core_func = (fun _ b -> 
      if match_cmd_payload_type 6 b
      then coreexit := true
      else ()
    )
  }]

let help_per_command x =
  let indent = "        " in
  indent ^ "ocs " ^ x.name ^ " " ^ x.arghelp ^ "\n" ^
  indent ^ indent ^ "- " ^ x.description ^ "\n\n"

let help_text =
  List.fold_left (fun a x -> a ^ help_per_command x) "usage:\n" all_commands