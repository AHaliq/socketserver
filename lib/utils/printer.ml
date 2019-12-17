open Unix;;

let print msg = let bytear = (Bytes.of_string msg) in write stdout bytear 0 (Bytes.length bytear) |> ignore;;
let printerr msg e = print (msg ^ (Printexc.to_string e))