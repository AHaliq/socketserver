open Numutils;;
open Settings;;

let match_cmd_payload_type i p = Bytes.get p 0 == Char.chr i

let generate_frames b =
  let len = Bytes.length b in
  let num_frames = ceil_int_div len max_payload_len in
  let last_frame = num_frames - 1 in
  let frames = ref [] in
  (for i = 0 to last_frame do
    let frame_start = i * max_payload_len in
    let frame_len = if i == last_frame then len - frame_start else max_payload_len in
    let frame_payload = Bytes.create frame_len in
    let frame_lendata = pad_min_len frame_len_bytes (bytes_of_int frame_len) in
    let frame_cont = bytes_of_int (if i == last_frame then 1 else 2) in
    Bytes.blit b frame_start frame_payload 0 frame_len;
    let frame = Bytes.cat frame_cont (Bytes.cat frame_lendata frame_payload) in
    frames := List.cons frame !frames 
  done);
  List.rev !frames;;
