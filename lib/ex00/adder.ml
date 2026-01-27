let adder (a : Int32.t) (b : Int32.t): Int32.t = let rec adder_rec a b =  if b <> Int32.zero then adder_rec (Int32.logxor a b) (Int32.shift_left (Int32.logand a b) 1) else a in adder_rec a b

