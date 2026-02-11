
let print_truth_line (arg: string) =
  let rec print_truth_line_rec arg i =
    if (i = 0) then (print_char '|');
    if (i < String.length arg)
    then (print_char ' ';print_char arg.[i]; print_char ' '; print_char '|' ; print_truth_line_rec arg (i + 1));
    if (i = String.length arg) then print_endline ""
  in print_truth_line_rec arg 0

let print_truth_mid_line (arg: string) =
  let rec print_truth_mid_line_rec arg i =
    if (i = 0) then (print_char '|');
    if (i < String.length arg)
    then (print_char '-';print_char '-'; print_char '-'; print_char '|' ; print_truth_mid_line_rec arg (i + 1));
    if (i = String.length arg) then print_endline ""
  in print_truth_mid_line_rec arg 0




let print_truth_table (formula: string): Unit.t = 
  let arg = Utils.Sort_string.sort_string (Utils.Find_unique.find_unique (Utils.Find_letter.find_letter formula)) in
  print_truth_line (arg ^ String.make 1 '=');
  print_truth_mid_line (arg ^ String.make 1 '=');


  let rec print_truth_rec i =
    if i < 1 lsl String.length arg
    then (
      let bin = Utils.Int_to_bin_str.int_to_bin_str (Int32.of_int i) (String.length arg) in print_truth_line (bin ^ String.make 1 (if (Eval_formula.eval_formula (Utils.Calc.calc bin arg formula)) = true then '1' else '0'));
      print_truth_rec (i + 1)
    )

      in print_truth_rec 0

