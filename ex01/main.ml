let false_multiplier (a : Int32.t) (b : Int32.t) =  Int32.mul a b

let a = 64l
let b = 14l

let () = print_endline "mult"
let () = Utils.Print_couple.print_couple a b
let () = Utils.Print_mem.print_mem (Multiplier.multiplier a b)
let () = Utils.Print_mem.print_mem (false_multiplier a b)

