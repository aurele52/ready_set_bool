
open Ready_set_bool
let false_adder (a : Int32.t) (b : Int32.t) =  Int32.add a b
let a = 64l
let b = 14l

let () = print_endline "add"

let () = Utils.Print_couple.print_couple a b
let () = Utils.Print_mem.print_mem (false_adder a b)
let () = Utils.Print_mem.print_mem (Ex00.Adder.adder a b)

let false_multiplier (a : Int32.t) (b : Int32.t) =  Int32.mul a b

let a = 64l
let b = 14l

let () = print_endline "mult"
let () = Utils.Print_couple.print_couple a b
let () = Utils.Print_mem.print_mem (Ex01.Multiplier.multiplier a b)
let () = Utils.Print_mem.print_mem (false_multiplier a b)

let a = 64l

let () = print_endline "gray code"
let () = Utils.Print_mem.print_mem a
let () = Utils.Print_mem.print_mem (Ex02.Gray_code.gray_code a)


let () = print_endline "eval_formula"
let str = "011|&"
let () = print_endline str
let () = Utils.Print_bool.print_bool (Ex03.Eval_formula.eval_formula str)

let () = print_endline ""
let () = print_endline "truth table"

let toTest = "AB&C|"
let () = Ex04.Print_truth_table.print_truth_table toTest



let () = print_endline "negation_normal_form"
let test = "AB|C&!"
let () = print_endline test
let () = Utils.Print_btree.print_btree Utils.Char_to_string.char_to_string (Utils.Btree_construct.btree_construct test)
let () = Utils.Print_btree.print_btree Utils.Char_to_string.char_to_string (Utils.Btree_construct.btree_construct (Ex05.Negation_normal_form.negation_normal_form test))
let () = print_endline (Utils.Btree_to_RPN.btree_to_RPN (Utils.Btree_construct.btree_construct test))
let () = print_endline (Ex05.Negation_normal_form.negation_normal_form(test))




let test = "ABC&&!"
let () = print_endline test
let () = print_endline (Ex05.Negation_normal_form.negation_normal_form test)
let () = print_endline ""

let test = "AB^C>D|"
let () = print_endline test
let () = print_endline (Ex05.Negation_normal_form.negation_normal_form test)
let () = print_endline ""

let test = "AB=CD|&"
let () = print_endline test
let () = print_endline (Ex05.Negation_normal_form.negation_normal_form test)
let () = print_endline ""

let () = print_endline ""
let () = print_endline "conjunctive_normal_form"

let test = "AB=CD=|"      (* (A=B) | (C=D) *)
let () = Ex04.Print_truth_table.print_truth_table (test)
let () = Ex04.Print_truth_table.print_truth_table(Ex06.Conjunctive_normal_form.conjunctive_normal_form test)


let () = print_endline ""
let () = print_endline "SAT"

let test = "AB=CD=|"      (* (A=B) | (C=D) *)
let () = print_endline (test)
let () = Utils.Print_bool.print_bool (Ex07.Sat.sat (test))

let () = print_endline ""
let () = print_endline ""
let () = print_endline "Powerset"
let test = [1l;2l;3l]

let () = Utils.Print_double_list.print_double_liste (Ex08.Powerset.powerset test)


module Int32Set = Ex09.Eval_set.Int32Set

let () =
  (* ---- Test 1 ---- *)
  let sets1 = [
    Int32Set.of_list [0l; 1l; 2l];
    Int32Set.of_list [0l; 3l; 4l];
  ] in

  let result1 = Ex09.Eval_set.eval_set "AB&" sets1 in
  (* attendu : [0] *)

  (* ---- Test 2 ---- *)
  let sets2 = [
    Int32Set.of_list [0l; 1l; 2l];
    Int32Set.of_list [3l; 4l; 5l];
  ] in

  let result2 = Ex09.Eval_set.eval_set "AB|" sets2 in
  (* attendu : [0; 1; 2; 3; 4; 5] *)

  (* ---- Test 3 ---- *)
  let sets3 = [
    Int32Set.of_list [0l; 1l; 2l];
  ] in

  let result3 = Ex09.Eval_set.eval_set "A!" sets3 in
  (* attendu : [] *)

  let print_set s =
    Int32Set.iter (fun x ->
      Printf.printf "%ld " x
    ) s;
    print_newline ()
  in

  print_endline "Test 1:";
  print_set result1;

  print_endline "Test 2:";
  print_set result2;

  print_endline "Test 3:";
  print_set result3

let () = print_endline ""
let () = print_endline "Map"
let () = print_float (Ex10.Map.map 3l 2l)
let () = print_endline ""
let () = print_float (Ex10.Map.map 300l 2l)

let () = print_endline ""
let () = print_endline "Map Reverse"
let () = let (x, y) = Ex11.Reverse_map.reverse_map (Ex10.Map.map 3l 2l) in (Utils.Print_dec_uint32.print_dec_uint32 x; print_endline " "; Utils.Print_dec_uint32.print_dec_uint32 y)
let () = print_endline ""
let () = let (x, y) = Ex11.Reverse_map.reverse_map (Ex10.Map.map 10220l 12334l) in (Utils.Print_dec_uint32.print_dec_uint32 x; print_endline " "; Utils.Print_dec_uint32.print_dec_uint32 y)




