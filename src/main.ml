open Unix;;
open Sys;;
open List;;

module Pr = struct
  let print msg = let bytear = (Bytes.of_string msg) in write stdout bytear 0 (Bytes.length bytear) |> ignore;;
  let print_and_prompt msg = print (msg ^ "\n> ");;
  let print_pre_prompt msg = print_and_prompt ("\n" ^ msg);;
end

module Pa = struct
  let regex_match r s = Str.string_match (Str.regexp r) s 0;;
  let split r s = Str.split (Str.regexp r) s;;
end

module Ip = struct
  let get_my_addr () =
    (Unix.gethostbyname(Unix.gethostname())).Unix.h_addr_list.(0);;
end

let mux = Mutex.create ()

type connection_type =
  {
    name      : string;
    fd        : file_descr
  }

type state_type =
  { exit        : bool;
    connections : connection_type list;
  };;

let state = ref { exit = false; connections = [] }

let sync f =
  Mutex.lock mux;
  f ();
  Mutex.unlock mux;;

let open_command s =
  let port = match Pa.split "open " s with
    | x::xs -> int_of_string x
    | []    -> 80
  in
  let ip = Ip.get_my_addr () in
  let sockaddr = Unix.ADDR_INET (ip, port) in
  let con_name = (string_of_inet_addr ip) ^ ":" ^ (string_of_int port) in
  (* create socket and save fd *)
  state := { !state with connections = cons {
     name = con_name;
     fd =  stdin
  } !state.connections };
  Pr.print_and_prompt ("Starting socket : " ^ (string_of_inet_addr ip) ^ " at " ^ (string_of_int port));;

let connect_command s =
  match Pa.split "connect " s with
    | x::xs -> 
    let ipportstrs = Pa.split ":" x in
    let ipstr = hd ipportstrs in
    let portstr = hd (tl ipportstrs) in
    Pr.print_and_prompt ("Connecting to : " ^ ipstr ^ " at " ^ portstr)
    | []    -> ()

let list_command () =
  Pr.print_and_prompt (fold_left (fun a { name = n; _ } -> a ^ "\n  " ^ n) "Active connections:" !state.connections);;

let send_command s =
  let inp = hd (Pa.split "send " s) in
  let ipstr = hd (Pa.split ":" inp) in
  let msgstr = fold_left (fun a b -> a ^ b) "" (Pa.split "[0-9]+.[0-9]+.[0-9].+[0-9]+:[0-9]+ " inp) in
  let port = int_of_string (hd (tl (Pa.split ":" (hd (Pa.split (" " ^ msgstr) inp))))) in
  Pr.print_and_prompt (string_of_int port);;

let close_command s =
  match Pa.split "close " s with
    | x::xs ->
    let ipportstrs = Pa.split ":" x in
    let ipstr = hd ipportstrs in
    let portstr = hd (tl ipportstrs) in
    (* search connection object *)
    (* close socket and fd *)
    (* remove from state *)
    | []    -> ()

let help_command () = Pr.print_and_prompt (
  "Following are legal commands:\n" ^
  "  open <port>\n"^
  "    opens a socket server at specified port\n" ^
  "  connect <ip:port>\n" ^
  "    connects to a socket server at specified ipaddr\n" ^
  "  list\n" ^
  "    lists current connections\n" ^
  "  send <ip:port> <string>\n" ^
  "    sends a string as byte array through the connection\n" ^
  "  close <ip:port>\n" ^
  "    closes the connection\n" ^
  "  exit\n" ^
  "    gracefully closes all connections and threads and exits program");;

let listener () =
  Pr.print_pre_prompt "Thread started"

let client () =
  Pr.print_and_prompt "Welcome to multi socket communication app!";
  let p = Thread.create listener () in
  while not !state.exit do
    match read_line () with
      | s when Pa.regex_match "^open [0-9]+$" s ->
      open_command s
      | s when Pa.regex_match "^connect [0-9]+.[0-9]+.[0-9].+[0-9]+:[0-9]+$" s ->
      connect_command s
      | s when Pa.regex_match "^list$" s ->
      list_command ()
      | s when Pa.regex_match "^send [0-9]+.[0-9]+.[0-9].+[0-9]+:[0-9]+ .+$" s ->
      send_command s
      | s when Pa.regex_match "^close [0-9]+.[0-9]+.[0-9].+[0-9]+:[0-9]+$" s ->
      close_command s
      | s when Pa.regex_match "^exit$" s ->
      sync (fun x -> state := { !state with exit = true }; x);
      | _ -> help_command ()
  done;
  Pr.print "waiting for thread";
  Thread.join p;;

handle_unix_error client ();;