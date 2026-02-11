let () = print_endline "SAT"

let test = "AB=CD=|"      (* (A=B) | (C=D) *)
let () = print_endline (test)
let () = Utils.Print_bool.print_bool (Sat.sat (test))

