let () = print_endline "Powerset"
let test = [1l;2l;3l]

let () = Utils.Print_double_list.print_double_liste (Powerset.powerset test)
