
let print_bin_uint32 (a : Int32.t) =
  let rec print_bin_uint32_rec a index =
    if index < 32 then (
      print_bin_uint32_rec a (index + 1);
       Print_bit_at_index.print_bit_at_index a index; if (index mod 4 = 0 && index != 0)  then print_char ' '
    )
  in
  print_bin_uint32_rec a 0
