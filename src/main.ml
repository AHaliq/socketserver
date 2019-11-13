open Unix;;
open Sys;;

module Printer = struct
  let print msg = let bytear = (Bytes.of_string msg) in write stdout bytear 0 (Bytes.length bytear) |> ignore;;
  let print_and_prompt msg = print (msg ^ "\n> ");;
  let print_pre_prompt msg = print_and_prompt ("\n" ^ msg);;
  let greet = print_and_prompt "Welcome to multi socket communication app!";;
end

let mux = Mutex.create ()

type state_type =
  { exit  : bool;
    num   : int;
  };;

let state = ref { exit = false; num = 0 }

let listener () =
  Printer.print_and_prompt "Thread started";
  while not !state.exit do
    Mutex.lock mux;
    state := { !state with num = !state.num + 1 };
    Mutex.unlock mux;
    sleep 1;
  done;;

let client () =
  Printer.greet;
  let p = Thread.create listener () in
  while not !state.exit do
    let s = read_line () in
    if s = "show" then 
      (Mutex.lock mux;
      Printer.print_and_prompt (string_of_int !state.num);
      Mutex.unlock mux)
    else if s = "exit" then
      (Mutex.lock mux;
      state := { !state with exit = true };
      Mutex.unlock mux);
  done;
  Printer.print "waiting for thread";
  Thread.join p;;

handle_unix_error client ();;