


let () = print_endline "negation_normal_form"
let test = "AB|C&!"
let () = print_endline test
let () = Utils.Print_btree.print_btree Utils.Char_to_string.char_to_string (Utils.Btree_construct.btree_construct test)
let () = Utils.Print_btree.print_btree Utils.Char_to_string.char_to_string (Utils.Btree_construct.btree_construct (Negation_normal_form.negation_normal_form test))
let () = print_endline (Utils.Btree_to_RPN.btree_to_RPN (Utils.Btree_construct.btree_construct test))
let () = print_endline (Negation_normal_form.negation_normal_form(test))




let test = "ABC&&!"
let () = print_endline test
let () = print_endline (Negation_normal_form.negation_normal_form test)
let () = print_endline ""

let test = "AB^C>D|"
let () = print_endline test
let () = print_endline (Negation_normal_form.negation_normal_form test)
let () = print_endline ""

let test = "AB=CD|&"
let () = print_endline test
let () = print_endline (Negation_normal_form.negation_normal_form test)
