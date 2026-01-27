
let gray_code (a: Int32.t): Int32.t = Int32.logxor a (Int32.shift_right_logical a 1)
