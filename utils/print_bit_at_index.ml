
let print_bit_at_index (a : Int32.t) (index : int) =
  let mask = Int32.shift_left 1l index in
  print_int (if Int32.logand a mask = Int32.zero then 0 else 1)
