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

type seek_state_type =
  | SeekLength  (* currently reading length of payload for frame *)
  | SeekEnd     (* currently reading end flag of frame *)
  | SeekData    (* currently reading payload of frame *)

type connection_type =
  {
    name      : string;             (* ip and port string of connection *)
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

(* TYPES AND GLOBAL ------------------ *)

let sync f =
  Mutex.lock mux;
  f ();
  Mutex.unlock mux;;

let default_connection = {
  name = "";
  fd =  stdin;
  s_buffer = [];
  s_buf_len = 0;
  d_buffer = [];
  frm_d_len = 0;
  end_of_msg = false;
  seek_state = SeekLength
}

let rec getConnection conns con_name = match conns with
  | x::xs when x.name = con_name  -> Some x
  | x::xs                         -> getConnection xs con_name
  | _                             -> None

let closeConnection conn = Pr.print ("closed connection " ^ conn.name ^ "\n")
(* UTIL FUNCTIONS -------------------- *)

let open_command s =
  let port = match Pa.split "open " s with
    | x::xs -> int_of_string x
    | []    -> 80
  in
  let ip = Ip.get_my_addr () in
  let sockaddr = Unix.ADDR_INET (ip, port) in
  let con_name = (string_of_inet_addr ip) ^ ":" ^ (string_of_int port) in
  (* create socket and save fd *)
  state := { !state with connections = cons { default_connection with name = con_name } !state.connections };
  Pr.print_and_prompt ("Starting socket : " ^ (string_of_inet_addr ip) ^ " at " ^ (string_of_int port));;

let connect_command s =
  match Pa.split "connect " s with
    | x::xs -> 
    let ipportstrs = Pa.split ":" x in
    let ipstr = hd ipportstrs in
    let portstr = hd (tl ipportstrs) in
    let con_name = ipstr ^ ":" ^ portstr in
    (* create socket and save fd *)
    state := { !state with connections = cons { default_connection with name = con_name } !state.connections };
    Pr.print_and_prompt ("Connecting to : " ^ ipstr ^ " at " ^ portstr)
    | []    -> ()

let list_command () = Pr.print_and_prompt
  (if length !state.connections = 0
  then "No connections, use open or connect command."
  else (fold_left (fun a { name = n; _ } -> a ^ "\n  " ^ n) "Active connections:" !state.connections));;

let send_command s =
  let inp = hd (Pa.split "send " s) in
  let ipstr = hd (Pa.split ":" inp) in
  let msgstr = fold_left (fun a b -> a ^ b) "" (Pa.split "[0-9]+.[0-9]+.[0-9].+[0-9]+:[0-9]+ " inp) in
  let port = int_of_string (hd (tl (Pa.split ":" (hd (Pa.split (" " ^ msgstr) inp))))) in
  let con_name = ipstr ^ ":" ^ (string_of_int port) in
  (match getConnection !state.connections con_name with
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
    (match getConnection !state.connections con_name with
      | Some x  -> 
      closeConnection x;
      state := { !state with connections = filter_map (fun x -> if x.name = con_name then None else Some x) !state.connections }
      | None    -> Pr.print_and_prompt "no such connection exist")
    | []    -> ()

let exit_command () =
  map (fun x ->
    closeConnection x
  ) !state.connections |> ignore;
  sync (fun x -> state := { !state with exit = true }; x);;

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

(* COMMAND FUNCTIONS ------------------------ *)

let listener () =
  Pr.print_pre_prompt "Thread started";
  let exit = ref false in
  while not !exit do
    map (fun x -> x
      (* recv non blocking into socket buffer *)
      (* interpret socket buffer / push to data buffer *)
      (* on data buffer flush interpret message *)
      (* if new message send ACK message *)
      (* if ACK message look through ack hashes to print roundtrip time *)
    ) !state.connections |> ignore;
    sync (fun x -> exit := !state.exit; x)
  done;;

let client () =
  Pr.print_and_prompt "Welcome to multi socket communication app!";
  let p = Thread.create listener () in
  let exit = ref false in
  while not !exit do
    (match read_line () with
      | s when Pa.regex_match "^open [0-9]+$" s ->
      open_command s
      | s when Pa.regex_match "^connect [0-9]+.[0-9]+.[0-9].+[0-9]+:[0-9]+$" s ->
      connect_command s
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