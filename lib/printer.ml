open Unix;;

let print msg = let bytear = (Bytes.of_string msg) in write stdout bytear 0 (Bytes.length bytear) |> ignore;;