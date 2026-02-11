

let a = 64l

let () = print_endline "gray code"
let () = Utils.Print_mem.print_mem a
let () = Utils.Print_mem.print_mem (Gray_code.gray_code a)

