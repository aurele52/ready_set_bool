
let multiplier (a : Int32.t) (b : Int32.t): Int32.t =
  let rec multiplier_rec (a: Int32.t) (b: Int32.t) (result: Int32.t) =
    if (a = Int32.zero)
    then result
    else (
      if ((Int32.logand a 1l) = 1l)
      then multiplier_rec (Int32.shift_right_logical a 1) (Int32.shift_left b 1) (Adder.adder result b)
      else multiplier_rec (Int32.shift_right_logical a 1) (Int32.shift_left b 1) (result)
    )
  in multiplier_rec a b 0l

