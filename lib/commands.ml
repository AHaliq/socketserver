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
          let payload = recv_frames_as_bytes_from_fd cfd in
          List.fold_left (fun () x -> x.core_func cfd payload) () all_commands |> ignore;
          close cfd
        )) in
    
    let service_connections () = List.map (fun c -> 
      let (rs, _, _) = select [c.fd] [] [] poll_time in
      match rs with
      | [] -> ()
      | _ ->
        if c.is_server 
        then
          let (cfd, _) = accept c.fd in
          add_new_peer cfd;
          let newCon = tcpconnection_of_fd cfd in
          print ("new client connected :\n" ^
          indent ^ (string_of_tcpconnection newCon) ^ "\nto:\n" ^
          indent ^ (string_of_tcpconnection c) ^ "\n")
          (* new connect to server *)
        else
          let payload = recv_frames_as_bytes_from_fd c.fd in
          let msg_type = int_of_bytes (Bytes.sub payload 0 1) in
          let msg_bytes = Bytes.sub payload 1 (Bytes.length payload - 1) in
          (match msg_type with
          | 0 -> print "TODO! print ACK timing\n"
          | 1 ->
            send_bytes_as_frames_to_fd c.fd (bytes_of_int 0);
            print ("new message from :\n" ^
            indent ^ (string_of_tcpconnection c) ^ "\nmessage :" ^
            (Bytes.to_string msg_bytes) ^ "\n")
          | _ -> print "UNKNOWN MSG FORMAT RECIEVED"
          )
          (* new send to client *)
    ) !all_conns |> ignore in
    
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
        print "core_process is already running.\n")
      else core_process ());
      close_core_client ()
    );
    core_func = (fun _ _ -> ())
  }; (* -------------------------------------- *)
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
      then let returnmsg = List.fold_left (fun a c -> a ^ indent ^ (string_of_tcpconnection c) ^ "\n") "connections:\n" !all_conns in
      send_bytes_as_frames_to_fd fd (Bytes.of_string returnmsg)
      else ()
    )
  }; (* -------------------------------------- *)
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
        let returnmsg = (try create_server (get_local_ip ()) port; "success.\n"
        with e -> "failed with:\n" ^ (Printexc.to_string e) ^ "\n") in
        send_bytes_as_frames_to_fd fd (Bytes.of_string returnmsg)
      else ()
    )
  }; (* -------------------------------------- *)
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
        let returnmsg = (match String.split_on_char ':' ipport_str with
        | ip_str::port_str::_ ->
          (try connect_to_server (inet_addr_of_string ip_str) (int_of_string port_str); "success"
          with e -> "failed connecting with:\n" ^ Printexc.to_string e)
        | _ -> "invalid format") ^ "\n" in
        send_bytes_as_frames_to_fd fd (Bytes.of_string returnmsg)
      else ()
    )
  }; (* -------------------------------------- *)
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
        let str_byte = Bytes.sub b 3 (Bytes.length b - 3) in
        let returnmsg =
          (try
            let c = tcpconnection_of_id id in
            if c.is_server then
              "you can't send to your own server, send to client connections"
            else
              let type_byte = bytes_of_int 1 in
              send_bytes_as_frames_to_fd c.fd (Bytes.cat type_byte str_byte);
              (* mark Sys.time () for ACK timing *)
              "success"
          with _ -> "id is not valid") ^ "\n" in
        send_bytes_as_frames_to_fd fd (Bytes.of_string returnmsg)
      else ()
    )
  }; (* -------------------------------------- *)
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
        let returnmsg = (try kill_conn id; "success" with e -> "failed with\n" ^ Printexc.to_string e) ^ "\n" in
        send_bytes_as_frames_to_fd fd (Bytes.of_string returnmsg)
      else ()
    )
  }; (* -------------------------------------- *)
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
      then (kill_all_conn (); coreexit := true)
      else ()
    )
  }] (* -------------------------------------- *)

let help_per_command x =
  indent ^ "ocs " ^ x.name ^ " " ^ x.arghelp ^ "\n" ^
  indent ^ indent ^ "- " ^ x.description ^ "\n\n"

let help_text =
  List.fold_left (fun a x -> a ^ help_per_command x) "usage:\n" all_commands