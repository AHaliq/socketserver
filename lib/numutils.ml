let ceil_int_div x y =
  let xf = Float.of_int x in
  let yf = Float.of_int y in
  int_of_float (Float.ceil (Float.div xf yf))

let int_exp x y =
  let xf = Float.of_int x in
  let yf = Float.of_int y in
  int_of_float (xf ** yf)

let bytes_of_int i =
  let make_int_bytes i = Bytes.make 1 (Char.chr i) in
  let c = ref i in
  let empty = ref true in
  let res = ref (make_int_bytes 0) in
  if i == 0 then !res else (
  (while !c > 0 do
    let b = make_int_bytes (!c land 255) in
    (if !empty
    then
      (res := b;
      empty := false)
    else
      res := Bytes.cat b !res);
    c := !c lsr 8
  done);
  !res)

let pad_min_len n b =
  let len = Bytes.length b in
  if len >= n then b
  else Bytes.cat (Bytes.make (n - len) (Char.chr 0)) b