open Unix;;
open Printer;;
open Settings;;
open Numutils;;
open Frameutils;;

type tcpconnection =
  {
    id        : int;
    is_server : bool;
    ip        : inet_addr;
    port      : int;
    fd        : file_descr;
    sent_time : float Queue.t
  }

let id_counter = ref 0

let all_conns : tcpconnection list ref  = ref []

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

let recv_frames_as_bytes_from_fd fd =
  let moreframes = ref true in
  let payloads = ref [] in
  (while !moreframes do
    let bytes_cont = Bytes.create 1 in
    recv fd bytes_cont 0 1 [] |> ignore;
    let chr_cont = Bytes.get bytes_cont 0 in
    let eof = chr_cont == Char.chr 0 in
    if eof then raise (Failure "eof")
    else
      let cont = chr_cont == Char.chr 2 in
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

let get_local_ip () =
  (Unix.gethostbyname(Unix.gethostname())).Unix.h_addr_list.(0)

(* GENERAL FUNCTIONS ------------------------- *)

let string_of_tcpconnection x =
  "id: " ^ (string_of_int x.id) ^ ", " ^
  "type: " ^ (if x.is_server then "server" else "client") ^ ", " ^
  "sockaddr: " ^ (string_of_inet_addr x.ip) ^ ":" ^ (string_of_int x.port)

let tcpconnection_of_fd fd =
  match List.filter_map (fun c -> if c.fd == fd then Some c else None) !all_conns with
  | c::_  -> c
  | _     -> raise (Failure "Id does not exist")

let tcpconnection_of_id id =
  match List.filter_map (fun c -> if c.id == id then Some c else None) !all_conns with
  | c::_  -> c
  | _     -> raise (Failure "Id does not exist")

let adv_counter () = id_counter := (
  match !id_counter with
  | 65536 -> 0
  | _     -> !id_counter + 1)


let create_server ip_arg port_arg =
  let sfd = socket PF_INET SOCK_STREAM 0 in
  (try
    bind sfd (ADDR_INET (ip_arg, port_arg));
    listen sfd 1;
    let newCon = {
      id = !id_counter;
      is_server = true;
      ip = (match getsockname sfd with
      | ADDR_INET (ip,_) -> ip
      | ADDR_UNIX _ -> raise (Failure "expecting tcp address"));
      port = (match getsockname sfd with
      | ADDR_INET (_,p) -> p
      | ADDR_UNIX _ -> raise (Failure "expecting tcp address"));
      fd = sfd;
      sent_time = Queue.create ()
    } in
    adv_counter ();
    all_conns := List.cons newCon !all_conns
  with e -> close sfd; raise e)

let connect_to_server ip_arg port_arg =
  let cfd = socket PF_INET SOCK_STREAM 0 in
  (try 
    bind cfd (ADDR_INET (get_local_ip (), 0));
    connect cfd (ADDR_INET (ip_arg, port_arg));
    let newCon = {
      id = !id_counter;
      is_server = false;
      ip = (match getpeername cfd with
      | ADDR_INET (ip,_) -> ip
      | ADDR_UNIX _ -> raise (Failure "expecting tcp address"));
      port = (match getpeername cfd with
      | ADDR_INET (_,p) -> p
      | ADDR_UNIX _ -> raise (Failure "expecting tcp address"));
      fd = cfd;
      sent_time = Queue.create ()
    } in
    adv_counter ();
    all_conns := List.cons newCon !all_conns
  with e -> close cfd; raise e)

let add_new_peer cfd =
  let (ip_arg, port_arg) = (
    match getpeername cfd with
    | ADDR_INET (x,y) -> (x,y)
    | ADDR_UNIX _ -> raise (Failure "must be tcp socket")
  ) in
  let newCon = {
    id = !id_counter;
    is_server = false;
    ip = ip_arg;
    port = port_arg;
    fd = cfd;
    sent_time = Queue.create ()
  } in
  adv_counter ();
  all_conns := List.cons newCon !all_conns

let kill_conn id =
  match (List.filter_map (fun x -> if x.id == id then Some x else None) !all_conns) with
  | con::_ ->
    close con.fd;
    all_conns := List.filter_map (fun x -> if x.id == id then None else Some x) !all_conns
  | _ -> raise (Failure "Id does not exist.")

let kill_all_conn () =
  List.map (fun x -> close x.fd) !all_conns |> ignore;
  all_conns := []

(* TCPCONN FUNCTIONS ------------------------- *)

let connect_core_client () =
  try
    let fd = socket PF_UNIX SOCK_STREAM 0 in
    (try
      core_client_fd := Some fd;
      attempt_unlink core_sock_client;
      bind fd (ADDR_UNIX core_sock_client);
      connect fd (ADDR_UNIX core_sock_server);
      core_client_connected := true
    with _ -> close fd)
  with _ -> ()
  (*with e -> printerr "" e*)

let close_core_client () =
  if !core_client_connected
  then close_socket !core_client_fd
  else ()

let setup_core_server () =
  let fd = (socket PF_UNIX SOCK_STREAM 0) in
  core_server_fd := Some fd;
  (try
    attempt_unlink core_sock_server;
    bind fd (ADDR_UNIX core_sock_server);
    listen fd 1
  with _ -> close fd)

let send_bytes_to_core b =
  match !core_client_fd with
  | Some fd when !core_client_connected -> send_bytes_as_frames_to_fd fd b
  | _ -> print "core_process not started.\nrun `ocs core` first.\n"

let recv_bytes_from_core () = 
  match !core_client_fd with
  | Some fd when !core_client_connected -> recv_frames_as_bytes_from_fd fd
  | _ -> Bytes.create 0

(* CORE FUNCTIONS ---------------------------- *)