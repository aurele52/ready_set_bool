let int_to_bin_str (n : Int32.t) (size : int) : string =
  let rec int_to_bin_str_rec i ret =
    if i = size then ret
    else
      let bit_index = size - 1 - i in
      let mask = Int32.shift_left 1l bit_index in
      let bit =
        if Int32.logand n mask = Int32.zero then '0' else '1'
      in
      int_to_bin_str_rec (i + 1) (ret ^ String.make 1 bit)
  in
  int_to_bin_str_rec 0 ""
