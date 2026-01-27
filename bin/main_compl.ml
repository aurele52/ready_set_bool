
open Ready_set_bool

(* “fausses” versions (autorisé dans le main de test) *)
let false_adder (a : Int32.t) (b : Int32.t) : Int32.t = Int32.add a b
let false_multiplier (a : Int32.t) (b : Int32.t) : Int32.t = Int32.mul a b

let expected (v : string) =
  print_string "Expected: ";
  print_string v;
  print_endline ""

let () =
  (* -------------------- Ex00 - Adder (exemples du sujet) -------------------- *)
  print_endline "add";
  let a = 64l in
  let b = 14l in
  Utils.Print_couple.print_couple a b;
  expected (Int32.to_string (false_adder a b));
  Utils.Print_mem.print_mem (false_adder a b);
  Utils.Print_mem.print_mem (Ex00.Adder.adder a b);

  (* -------------------- Ex01 - Multiplier (exemples du sujet) -------------------- *)
  print_endline "mult";
  let a = 64l in
  let b = 14l in
  Utils.Print_couple.print_couple a b;
  expected (Int32.to_string (false_multiplier a b));
  Utils.Print_mem.print_mem (false_multiplier a b);
  Utils.Print_mem.print_mem (Ex01.Multiplier.multiplier a b);

  (* -------------------- Ex02 - Gray code (exemples du sujet) -------------------- *)
  print_endline "gray code";
  let n = 0l in
  Utils.Print_mem.print_mem n;
  expected "0";
  Utils.Print_mem.print_mem (Ex02.Gray_code.gray_code n);

  let n = 1l in
  Utils.Print_mem.print_mem n;
  expected "1";
  Utils.Print_mem.print_mem (Ex02.Gray_code.gray_code n);

  let n = 2l in
  Utils.Print_mem.print_mem n;
  expected "3";
  Utils.Print_mem.print_mem (Ex02.Gray_code.gray_code n);

  let n = 3l in
  Utils.Print_mem.print_mem n;
  expected "2";
  Utils.Print_mem.print_mem (Ex02.Gray_code.gray_code n);

  let n = 4l in
  Utils.Print_mem.print_mem n;
  expected "6";
  Utils.Print_mem.print_mem (Ex02.Gray_code.gray_code n);

  let n = 5l in
  Utils.Print_mem.print_mem n;
  expected "7";
  Utils.Print_mem.print_mem (Ex02.Gray_code.gray_code n);

  let n = 6l in
  Utils.Print_mem.print_mem n;
  expected "5";
  Utils.Print_mem.print_mem (Ex02.Gray_code.gray_code n);

  let n = 7l in
  Utils.Print_mem.print_mem n;
  expected "4";
  Utils.Print_mem.print_mem (Ex02.Gray_code.gray_code n);

  let n = 8l in
  Utils.Print_mem.print_mem n;
  expected "12";
  Utils.Print_mem.print_mem (Ex02.Gray_code.gray_code n);

  (* -------------------- Ex03 - Eval formula (exemples du sujet) -------------------- *)
  print_endline "eval_formula";
  let str = "10&" in
  print_endline str;
  expected "false";
  Utils.Print_bool.print_bool (Ex03.Eval_formula.eval_formula str);

  let str = "10|" in
  print_endline str;
  expected "true";
  Utils.Print_bool.print_bool (Ex03.Eval_formula.eval_formula str);

  let str = "11>" in
  print_endline str;
  expected "true";
  Utils.Print_bool.print_bool (Ex03.Eval_formula.eval_formula str);

  let str = "10=" in
  print_endline str;
  expected "false";
  Utils.Print_bool.print_bool (Ex03.Eval_formula.eval_formula str);

  let str = "1011||=" in
  print_endline str;
  expected "true";
  Utils.Print_bool.print_bool (Ex03.Eval_formula.eval_formula str);

  (* -------------------- Ex04 - Truth table (exemple du sujet) -------------------- *)
  print_endline "";
  print_endline "truth table";
  let to_test = "AB&C|" in
  print_endline to_test;
  expected "table for (A ∧ B) ∨ C";
  Ex04.Print_truth_table.print_truth_table to_test;

  (* -------------------- Ex05 - NNF (exemples du sujet) -------------------- *)
  print_endline "negation_normal_form";

  let test = "AB&!" in
  print_endline test;
  expected "A!B!|";
  print_endline (Ex05.Negation_normal_form.negation_normal_form test);

  let test = "AB|!" in
  print_endline test;
  expected "A!B!&";
  print_endline (Ex05.Negation_normal_form.negation_normal_form test);

  let test = "AB>" in
  print_endline test;
  expected "A!B|";
  print_endline (Ex05.Negation_normal_form.negation_normal_form test);

  let test = "AB=" in
  print_endline test;
  expected "AB&A!B!&|";
  print_endline (Ex05.Negation_normal_form.negation_normal_form test);

  let test = "AB|C&!" in
  print_endline test;
  expected "A!B!&C!|";
  print_endline (Ex05.Negation_normal_form.negation_normal_form test);

  (* -------------------- Ex06 - CNF (exemples du sujet) -------------------- *)
  print_endline "";
  print_endline "conjunctive_normal_form";

  let test = "AB&!" in
  print_endline test;
  expected "A!B!|";
  print_endline (Ex06.Conjunctive_normal_form.conjunctive_normal_form test);

  let test = "AB|!" in
  print_endline test;
  expected "A!B!&";
  print_endline (Ex06.Conjunctive_normal_form.conjunctive_normal_form test);

  let test = "AB|C&" in
  print_endline test;
  expected "AB|C&";
  print_endline (Ex06.Conjunctive_normal_form.conjunctive_normal_form test);

  let test = "AB|C|D|" in
  print_endline test;
  expected "ABCD|||";
  print_endline (Ex06.Conjunctive_normal_form.conjunctive_normal_form test);

  let test = "AB&C&D&" in
  print_endline test;
  expected "ABCD&&&";
  print_endline (Ex06.Conjunctive_normal_form.conjunctive_normal_form test);

  let test = "AB&!C!|" in
  print_endline test;
  expected "A!B!C!||";
  print_endline (Ex06.Conjunctive_normal_form.conjunctive_normal_form test);

  let test = "AB|!C!&" in
  print_endline test;
  expected "A!B!C!&&";
  print_endline (Ex06.Conjunctive_normal_form.conjunctive_normal_form test);

  (* -------------------- Ex07 - SAT (exemples du sujet) -------------------- *)
  print_endline "";
  print_endline "SAT";

  let test = "AB|" in
  print_endline test;
  expected "true";
  Utils.Print_bool.print_bool (Ex07.Sat.sat test);

  let test = "AB&" in
  print_endline test;
  expected "true";
  Utils.Print_bool.print_bool (Ex07.Sat.sat test);

  let test = "AA!&" in
  print_endline test;
  expected "false";
  Utils.Print_bool.print_bool (Ex07.Sat.sat test);

  let test = "AA^" in
  print_endline test;
  expected "false";
  Utils.Print_bool.print_bool (Ex07.Sat.sat test);

  (* -------------------- Ex08 - Powerset (exemple du sujet: type i32) -------------------- *)
  print_endline "";
  print_endline "Powerset";
  let set = [1l; 2l; 3l] in
  expected "all subsets (order doesn't matter)";
  (* adapte si ton Ex08 prend Int32.t list / int32 list / etc. *)
  let ps = Ex08.Powerset.powerset set in
  Utils.Print_double_list.print_double_liste ps


(* ------------- helpers (ajoute ça en haut du main ou juste avant Ex09) ------------- *)
let print_u16_pair (x : Int32.t) (y : Int32.t) =
  print_string "(";
  print_string (Int32.to_string x);
  print_string ", ";
  print_string (Int32.to_string y);
  print_endline ")"

let expected (v : string) =
  print_string "Expected: ";
  print_string v;
  print_endline ""
;;

(* -------------------- Ex09 - Set evaluation -------------------- *)
let () =
  print_endline "";
  print_endline "Set evaluation (Ex09)";

  let open Ex09.Eval_set.Int32Set in

  (* sets = { {0,1,2}, {0,3,4} } *)
  let s1 = empty |> add 0l |> add 1l |> add 2l in
  let s2 = empty |> add 0l |> add 3l |> add 4l in
  let sets1 = [s1; s2] in

  let formula1 = "AB&" in
  print_endline formula1;
  print_string "Expected: ";
  print_endline "[0]";
  let r1 = Ex09.Eval_set.eval_set formula1 sets1 in
  Utils.Print_list_int.print_liste
    (List.map Int32.to_int (elements r1));

  (* sets = { {0,1,2}, {3,4,5} } *)
  let s3 = empty |> add 0l |> add 1l |> add 2l in
  let s4 = empty |> add 3l |> add 4l |> add 5l in
  let sets2 = [s3; s4] in

  let formula2 = "AB|" in
  print_endline formula2;
  print_string "Expected: ";
  print_endline "[0;1;2;3;4;5]";
  let r2 = Ex09.Eval_set.eval_set formula2 sets2 in
  Utils.Print_list_int.print_liste
    (List.map Int32.to_int (elements r2));

  (* sets = { {0,1,2} } *)
  let s5 = empty |> add 0l |> add 1l |> add 2l in
  let sets3 = [s5] in

  let formula3 = "A!" in
  print_endline formula3;
  print_string "Expected: ";
  print_endline "[]";
  let r3 = Ex09.Eval_set.eval_set formula3 sets3 in
  Utils.Print_list_int.print_liste
    (List.map Int32.to_int (elements r3))

(* -------------------- Ex10 - Curve / map (sanity checks cohérents u16) -------------------- *)
let () =
  print_endline "";
  print_endline "Curve (Ex10)";

  let x = 6l and y = 12l in
  print_u16_pair x y;
  expected "0.0";
  let v = Ex10.Map.map x y in
  print_endline (string_of_float v);

  let x = 6l and y = 12l in
  print_u16_pair x y;
  expected "Entre [0,1]";
  let v = Ex10.Map.map x y in
  print_endline (string_of_float v);

  let x = 65535l and y = 65535l in
  print_u16_pair x y;
  expected "~0.9999999997671694 (i.e. (2^32-1)/2^32)";
  let v = Ex10.Map.map x y in
  print_endline (string_of_float v)
;;

(* -------------------- Ex11 - Inverse function / reverse_map (sanity checks) -------------------- *)
let () =
  print_endline "";
  print_endline "Inverse function (Ex11)";

  let x = 0l and y = 0l in
  let n = Ex10.Map.map x y in
  print_endline ("n = " ^ string_of_float n);
  expected "(0, 0)";
  let (rx, ry) = Ex11.Reverse_map.reverse_map n in
  print_u16_pair rx ry;

  let x = 12345l and y = 54321l in
  print_u16_pair x y;
  let n = Ex10.Map.map x y in
  print_endline ("n = " ^ string_of_float n);
  expected "(12345, 54321)";
  let (rx, ry) = Ex11.Reverse_map.reverse_map n in
  print_u16_pair rx ry
;;


