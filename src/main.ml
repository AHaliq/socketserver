open Unix;;
open Sys;;
open List;;

(* Printer module *)
module Pr = struct
  let print msg = let bytear = (Bytes.of_string msg) in write stdout bytear 0 (Bytes.length bytear) |> ignore;;
  let print_and_prompt msg = print (msg ^ "\n> ");;
  let print_pre_prompt msg = print_and_prompt ("\n" ^ msg);;
end

(* Parser module *)
module Pa = struct
  let regex_match r s = Str.string_match (Str.regexp r) s 0;;
  let split r s = Str.split (Str.regexp r) s;;
end

(* Ip address module *)
module Ip = struct
  let get_my_addr () =
    (gethostbyname(gethostname())).h_addr_list.(0);;
end

(* MODULES AND IMPORTS -------------------------- *)

let mux = Mutex.create ()

type seek_state_type =
  | SeekLength  (* currently reading length of payload for frame *)
  | SeekEnd     (* currently reading end flag of frame *)
  | SeekData    (* currently reading payload of frame *)

type connection_type =
  {
    name      : string;             (* ip and port string of connection *)
    is_server : bool;
    fd        : file_descr;         (* socket file descriptor *)
    s_buffer  : char list;          (* buffer immediately from socket *)
    s_buf_len : int;                (* length of s_buffer *)
    d_buffer  : char list;          (* buffer for collating payload *)
    frm_d_len : int;                (* expected length of payload in frame *)
    end_of_msg: bool;               (* current frame is end of message flag *)
    seek_state: seek_state_type     (* state as recieving data *)
  }

type state_type =
  { exit        : bool;                   (* exiting application, proceed to cleanup *)
    connections : connection_type list;   (* list of active connections *)
  };;

let state = ref { exit = false; connections = [] }

(* TYPES AND GLOBAL ----------------------------- *)

let sync f =
  Mutex.lock mux;
  f ();
  Mutex.unlock mux;;

let default_connection = {
  name = "";
  is_server = false;
  fd = stdin;
  s_buffer = [];
  s_buf_len = 0;
  d_buffer = [];
  frm_d_len = 0;
  end_of_msg = false;
  seek_state = SeekLength
}

let setSocketOptions s =
  setsockopt s SO_REUSEADDR true;
  setsockopt_optint s SO_LINGER None;;

let rec getActiveConnection conns con_name = match conns with
  | x::xs when x.name == con_name  -> Some x
  | x::xs                         -> getActiveConnection xs con_name
  | _                             -> None

let checkConnected fd =
  (try getpeername fd |> ignore; true
  with e -> false);;

let closeConnection conn = 
  Pr.print ("attempt close " ^ (if conn.is_server then "passive" else "active") ^ " socket " ^ conn.name ^ "\n");
  (try
  shutdown conn.fd SHUTDOWN_ALL;
  Pr.print " success\n"
  with e -> Pr.print (" " ^ Printexc.to_string e ^ "\n"));;
(* UTIL FUNCTIONS ------------------------------- *)

let open_command s =
  let port = match Pa.split "open " s with
    | x::xs -> int_of_string x
    | []    -> 1024
  in
  let ip = Ip.get_my_addr () in
  let sockaddr = ADDR_INET (ip, port) in
  let con_name = (string_of_inet_addr ip) ^ ":" ^ (string_of_int port) in
  let socket_result = socket PF_INET SOCK_STREAM 0 in
  setSocketOptions socket_result;
  bind socket_result sockaddr;
  listen socket_result 1;
  sync (fun x -> 
  state := { !state with connections = cons { default_connection with
    name = con_name;
    is_server = true;
    fd = socket_result
  } !state.connections };
  Pr.print_and_prompt ("Started socket : " ^ con_name ^ "\n");
  x);;

let connect_command s =
  match Pa.split "connect " s with
    | x::xs -> 
    let ipportstrs = Pa.split ":" x in
    let ipstr = hd ipportstrs in
    let portstr = hd (tl ipportstrs) in
    let ip = ADDR_INET (inet_addr_of_string ipstr, int_of_string portstr) in
    let con_name = ipstr ^ ":" ^ portstr in
    sync (fun x ->
    let socket_result = socket PF_INET SOCK_STREAM 0 in
    setSocketOptions socket_result;
    connect socket_result ip;
    state := { !state with connections = cons { default_connection with
      name = con_name;
      fd = socket_result
    } !state.connections };
    Pr.print_and_prompt ("Connected to : " ^ con_name);
    x)
    | []    -> ()

let list_command () = 
  sync (fun x -> Pr.print_and_prompt
  (if length !state.connections = 0
  then "No connections, use open or connect command.\n"
  else snd (fold_left (fun (i,a) { name = n; is_server = s; fd = f; _ } -> (i+1, a ^ "\n  " ^ string_of_int i ^ " " ^ (if s then "passive" else "active ") ^ " " ^ n)) (0, "Active connections:") !state.connections) ^ "\n");
  x);;

let send_command s =
  let inp = hd (Pa.split "send " s) in
  let ipstr = hd (Pa.split ":" inp) in
  let msgstr = fold_left (fun a b -> a ^ b) "" (Pa.split "[0-9]+.[0-9]+.[0-9].+[0-9]+:[0-9]+ " inp) in
  let port = int_of_string (hd (tl (Pa.split ":" (hd (Pa.split (" " ^ msgstr) inp))))) in
  let con_name = ipstr ^ ":" ^ (string_of_int port) in
  (match getActiveConnection !state.connections con_name with
    | Some x  ->
    (* convert to bytearray *)
    (* split into frames *)
    (* append header; MD5 hash and length *)
    (* send frames through socket *)
    Pr.print_and_prompt ("Send '" ^ msgstr ^ "' to " ^ con_name)
    | None    -> Pr.print_and_prompt "no such connection exist")

let close_command s =
  match Pa.split "close " s with
    | x::xs ->
    let ipportstrs = Pa.split ":" x in
    let ipstr = hd ipportstrs in
    let portstr = hd (tl ipportstrs) in
    let con_name = ipstr ^ ":" ^ portstr in
    sync (fun y -> (match getActiveConnection !state.connections con_name with
      | Some x  -> 
        closeConnection x;
        state := { !state with connections = filter_map (fun x -> if x.name = con_name then None else Some x) !state.connections };
        Pr.print_and_prompt "";
      | None    -> Pr.print_and_prompt "no such connection exist");
      y)
    | []    -> ()

let exit_command () =
  sync (fun x -> 
    map (fun x -> closeConnection x) !state.connections |> ignore;
    state := { !state with exit = true; connections = [] };
    Pr.print "\n";
  x);;

let help_command () = Pr.print_and_prompt (
  "Following are legal commands:\n" ^
  "  open <port>\n"^
  "    opens a socket server at specified port\n" ^
  "  connect <ip:port>\n" ^
  "    connects to a socket server at specified ipaddr\n" ^
  "  ls\n" ^
  "    lists active connections\n" ^
  "  send <ip:port> <string>\n" ^
  "    sends a string as byte array through the connection\n" ^
  "  close <ip:port>\n" ^
  "    closes the connection\n" ^
  "  exit\n" ^
  "    gracefully closes all connections and threads and exits program");;

(* COMMAND FUNCTIONS ---------------------------- *)

let listener () =
  (*Pr.print_pre_prompt "Thread started";*)
  let exit = ref false in
  while not !exit do
    sleep 1;
    sync (fun x -> exit := !state.exit;
    (if !exit then () else
      map (fun c ->
      let (rs, ws, es) = select [c.fd] [] [] 0.1 in
      (match rs with
      | [] -> ()
      | ers -> (match c.is_server with
      | false -> ()(*Pr.print_and_prompt ("Ready to message pass for " ^ string_of_bool x.is_server ^ x.name)*)
      | true -> (let (rfd, paddr) = accept c.fd in Pr.print_pre_prompt (match paddr with
      | ADDR_INET (addr,port) -> string_of_inet_addr addr ^ ":" ^ string_of_int port
      | ADDR_UNIX name -> "" ))
      ));
      (* recv non blocking into socket buffer *)
      (* interpret socket buffer / push to data buffer *)
      (* on data buffer flush interpret message *)
      (* if new message send ACK message *)
      (* if ACK message look through ack hashes to print roundtrip time *)
      c) !state.connections |> ignore);
      x)
  done;;

let client () =
  Pr.print_and_prompt "Welcome to multi socket communication app!";
  let p = Thread.create listener () in
  let exit = ref false in
  while not !exit do
    (match read_line () with
      | s when Pa.regex_match "^open [0-9]+$" s ->
      (try open_command s with e -> Pr.print_and_prompt (Printexc.to_string e))
      | s when Pa.regex_match "^connect [0-9]+.[0-9]+.[0-9].+[0-9]+:[0-9]+$" s ->
      (try connect_command s with e -> Pr.print_and_prompt (Printexc.to_string e))
      | s when Pa.regex_match "^ls$" s ->
      list_command ()
      | s when Pa.regex_match "^send [0-9]+.[0-9]+.[0-9].+[0-9]+:[0-9]+ .+$" s ->
      send_command s
      | s when Pa.regex_match "^close [0-9]+.[0-9]+.[0-9].+[0-9]+:[0-9]+$" s ->
      close_command s
      | s when Pa.regex_match "^exit$" s ->
      exit_command ()
      | _ -> help_command ());
      sync (fun x -> exit := !state.exit; x)
  done;
  Pr.print "waiting for thread\n";
  Thread.join p;
  Pr.print "Goodbye!\n";;

handle_unix_error client ();;