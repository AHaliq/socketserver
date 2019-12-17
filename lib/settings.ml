open Numutils;;

let poll_time = 0.1
let frame_len_bytes = 2
let max_payload_len = int_exp 2 (frame_len_bytes * 8)

let indent = "        "

let core_sock_path = "ocs_unix_sock."
let core_sock_server = core_sock_path ^ "server"
let core_sock_client = core_sock_path ^ "client"