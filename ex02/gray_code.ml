
let print_hex_uint32 (a : Int32.t) =
  Printf.printf "0x%08lx" a


let print_dec_uint32 (a : Int32.t) =
  print_int (Int32.to_int a)


let print_bit_at_index (a : Int32.t) (index : int) =
  let mask = Int32.shift_left 1l index in
  print_int (if Int32.logand a mask = Int32.zero then 0 else 1)


let print_bin_uint32 (a : Int32.t) =
  let rec print_bin_uint32_rec a index =
    if index < 32 then (
      print_bin_uint32_rec a (index + 1);
       print_bit_at_index a index; if (index mod 4 = 0 && index != 0)  then print_char ' '
    )
  in
  print_bin_uint32_rec a 0


let print_bin_uint32_ln a =
  print_bin_uint32 a;
  print_endline ""


let print_mem (a: Int32.t) = print_dec_uint32 a; print_char ' '; print_hex_uint32 a; print_char ' '; print_bin_uint32_ln a

let grey_code (a: Int32.t): Int32.t = Int32.logxor a (Int32.shift_right a 1)

let a = 64l

let () = print_endline "gray code"
let () = print_mem a
let () = print_mem (grey_code a)

