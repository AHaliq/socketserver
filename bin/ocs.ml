open Unix;;
open Printer;;
open Commands;;

let ocs () =
  let arg_len = Array.length Sys.argv - 1 in
  (if arg_len == 0 || (arg_len >= 1 && Sys.argv.(1) = "help")
    then print help_text
    else
      let arg_cmd = Sys.argv.(1) in
      match List.filter_map
        (fun x -> if (x.name = arg_cmd) then (Some x) else None)
        all_commands with
      | []    -> print ("unknown command '" ^ arg_cmd ^ "'\n"); print help_text
      | x::_  -> x.cmd_func ()
  );;

handle_unix_error ocs ();;