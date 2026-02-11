
let false_adder (a : Int32.t) (b : Int32.t) =  Int32.add a b
let a = 64l
let b = 14l

let () = print_endline "add"

let () = Utils.Print_couple.print_couple a b
let () = Utils.Print_mem.print_mem (false_adder a b)
let () = Utils.Print_mem.print_mem (Adder.adder a b)

