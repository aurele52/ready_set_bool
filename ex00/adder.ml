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
let print_couple a b = print_mem a; print_mem b; print_char '\n'

(* let () = print_hex_uint32 (Int32.of_int 5) *)
(* let () = print_bin_uint32_ln (Int32.of_int 7) *)
(* let () = print_bin_uint32_ln (Int32.of_int 6) *)

let print_couple a b = print_mem a; print_mem b; print_char '\n'

let false_adder (a : Int32.t) (b : Int32.t) =  Int32.add a b
let adder (a : Int32.t) (b : Int32.t): Int32.t = let rec adder_rec a b =  if b <> Int32.zero then adder_rec (Int32.logxor a b) (Int32.shift_left (Int32.logand a b) 1) else a in adder_rec a b

let a = 64l
let b = 14l

let () = print_endline "add"

let () = print_couple a b
let () = print_mem (false_adder a b)
let () = print_mem (adder a b)
