let () = print_endline "conjunctive_normal_form"

let test = "AB=CD=|"      (* (A=B) | (C=D) *)
let () = Common.Print_truth_table.print_truth_table (test)
let () = Common.Print_truth_table.print_truth_table(Conjunctive_normal_form.conjunctive_normal_form test)
