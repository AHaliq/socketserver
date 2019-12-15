open Unix;;
open Printer;;
open Commands;;
open Connections;;

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