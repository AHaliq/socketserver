open Unix;;
open Printer;;

let core_sock_path = "ocs_unix_sock.server"

let ocs () =
  print "hello, world!\n";;

handle_unix_error ocs ();;