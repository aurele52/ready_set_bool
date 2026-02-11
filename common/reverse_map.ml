let reverse_map (n: float): Int32.t * Int32.t = 
  let rec map_reverse_rec n i (ret: Int32.t * Int32.t): Int32.t * Int32.t = 
      if (i < 32)
      then
        if (i mod 2 = 0)
        then map_reverse_rec (Int32.shift_right_logical n 1) (i + 1) (let (x, y) = ret in (x, Int32.logor (Int32.logand n 1l) (Int32.shift_left y 1)))
        else map_reverse_rec (Int32.shift_right_logical n 1) (i + 1) (let (x, y) = ret in (Int32.logor (Int32.logand n 1l) (Int32.shift_left x 1), y))
      else ret
  in let float_to_u32 (f:float) : int32 =
  let n = Int64.of_float (Float.floor (f *. 4294967296.0)) in
  Int64.(to_int32 (logand n 0xFFFF_FFFFL))  (* modulo 2^32 *)
  in map_reverse_rec (float_to_u32 n) 0 (0l, 0l)


