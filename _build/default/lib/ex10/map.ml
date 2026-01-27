let map (x: Int32.t) (y: Int32.t): float = 
  let mask16 = 0xFFFFl in
  let x = Int32.logand x mask16 in
  let y = Int32.logand y mask16 in
  let rec map_rec x y i ret = 
      if (i < 32)
      then
        if (i mod 2 = 0)
        then map_rec (Int32.shift_right_logical x 1) y (i + 1) (Int32.logor (Int32.logand x 1l) (Int32.shift_left ret 1))
      else map_rec x (Int32.shift_right_logical y 1) (i + 1) (Int32.logor (Int32.logand y 1l) (Int32.shift_left ret 1))
      else ret
  in let u32_to_float (u:int32) : float =
  let n =
    Int64.(logand (of_int32 u) 0xFFFF_FFFFL)  (* interprétation unsigned *)
  in
  Int64.to_float n /. 4294967296.0
  in u32_to_float ( map_rec (Int32.of_int (Int32.to_int x)) (Int32.of_int (Int32.to_int y)) 0 0l)
