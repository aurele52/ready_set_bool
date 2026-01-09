let () = print_endline "truth table"

let print_bool (b : bool) =
  if b then print_string "true"
  else print_string "false"

let is_number (c : char) : bool =
  c = '0' || c = '1'

let find_op op = 
  match op with
  | '=' -> (=)
  | '|' -> (||)
  | '&' -> (&&)
  | '^' -> (<>)
  | _ -> (=)


let apply_op_couple (stack: bool list) op =
  match stack with
  | a :: b :: rest -> (op a b) :: rest
  | _ -> stack

let apply_op_simple (stack: bool list) op =
  match stack with
  | a :: rest -> (op a) :: rest
  | _ -> stack

let apply_op stack op =
  if (op = '!') then apply_op_simple stack (not) else apply_op_couple stack (find_op op)

let eval_formula (a: string): bool=
  let rec eval_formula_rec (a: string) (i: int) (stack: bool list): bool =
    if (i < String.length a)
    then (
      if (is_number a.[i])
      then eval_formula_rec a (i + 1) ((if a.[i] = '1' then true else false) :: stack)
      else eval_formula_rec a (i + 1) (apply_op stack a.[i]);
      )
    else (List.hd stack)
  in eval_formula_rec a 0 []


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

let find_unique (formula: string) =
  let rec find_unique_rec i ret =
    if i = String.length formula then
      ret
    else
      let c = formula.[i] in
      if String.contains ret c then
        find_unique_rec (i + 1) ret
      else
        find_unique_rec (i + 1) (ret ^ String.make 1 c)
  in find_unique_rec 0 ""

let is_maj c =
  c >= 'A' && c <= 'Z'

let find_letter (formula: string) =
  let rec find_letter_rec i ret =
    if i = String.length formula then
      ret
    else
      let c = formula.[i] in
      if (is_maj c) then
        find_letter_rec (i + 1) (ret ^ String.make 1 c)
      else
        find_letter_rec (i + 1) ret
  in find_letter_rec 0 ""


let sort_string s =
  let chars = String.to_seq s in
  let chars_list = List.of_seq chars in
  let sorted = List.sort Char.compare chars_list in
  let sorted_seq = List.to_seq sorted in
  String.of_seq sorted_seq

let int_to_bin_str (n : Int32.t) (size : int) : string =
  let rec int_to_bin_str_rec i ret =
    if i = size then ret
    else
      let bit_index = size - 1 - i in
      let mask = Int32.shift_left 1l bit_index in
      let bit =
        if Int32.logand n mask = Int32.zero then '0' else '1'
      in
      int_to_bin_str_rec (i + 1) (ret ^ String.make 1 bit)
  in
  int_to_bin_str_rec 0 ""



let replace_by (formula: string) (arg: char) (by: char)= 
  let rec replace_by_rec i ret =
    if i < String.length formula then
      replace_by_rec (i + 1) (ret ^ String.make 1 (if formula.[i] = arg then by else formula.[i]))
    else
        ret
  in replace_by_rec 0 ""

let calc bin letter formula = 
  let rec calc_rec i formula =
    if i < String.length letter
    then calc_rec (i + 1) (replace_by formula letter.[i] bin.[i])
    else formula
  in calc_rec 0 formula

(* let print_truth (formula: string) (arg: string) =  *)
let print_truth_table (formula: string) = 
  let arg = sort_string (find_unique (find_letter formula)) in
  print_truth_line (arg ^ String.make 1 '=');
  print_truth_mid_line (arg ^ String.make 1 '=');


  let rec print_truth_rec i =
    if i < 1 lsl String.length arg
    then (
      let bin = int_to_bin_str (Int32.of_int i) (String.length arg) in print_truth_line (bin ^ String.make 1 (if (eval_formula (calc bin arg formula)) = true then '1' else '0'));
      print_truth_rec (i + 1)
    )

      in print_truth_rec 0


let toTest = "AB&C|"
let () = print_truth_table toTest

