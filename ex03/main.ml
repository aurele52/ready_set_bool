let () = print_endline "eval_formula"
let str = "011|&"
let () = print_endline str
let () = Utils.Print_bool.print_bool (Eval_formula.eval_formula str)

