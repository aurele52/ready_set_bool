let print_hex_uint32 (a : Int32.t) =
  Printf.printf "0x%08lx" a


let print_dec_uint32 (a : Int32.t) =
  print_int (Int32.to_int a)


let print_bit_at_index (a : Int32.t) (index : int) =
  let mask = Int32.shift_left 1l index in
  print_int (if Int32.logand a mask = Int32.zero then 0 else 1)


let print_bin_uint32 (a : Int32.t) =
  let rec print_bin_uint32_rec a index =
    if index < 32 then (
      print_bin_uint32_rec a (index + 1);
       print_bit_at_index a index; if (index mod 4 = 0 && index != 0)  then print_char ' '
    )
  in
  print_bin_uint32_rec a 0


let print_bin_uint32_ln a =
  print_bin_uint32 a;
  print_endline ""


let print_mem (a: Int32.t) = print_dec_uint32 a; print_char ' '; print_hex_uint32 a; print_char ' '; print_bin_uint32_ln a

(* let () = print_hex_uint32 (Int32.of_int 5) *)
(* let () = print_bin_uint32_ln (Int32.of_int 7) *)
(* let () = print_bin_uint32_ln (Int32.of_int 6) *)

let print_couple a b = print_mem a; print_mem b; print_char '\n'

let false_adder (a : Int32.t) (b : Int32.t) =  Int32.add a b
let adder (a : Int32.t) (b : Int32.t) = let rec adder_rec a b =  if b <> Int32.zero then adder_rec (Int32.logxor a b) (Int32.shift_left (Int32.logand a b) 1) else a in adder_rec a b

let a = 64l
let b = 14l

let () = print_endline "add"

let () = print_couple a b
let () = print_mem (false_adder a b)
let () = print_mem (adder a b)

let false_multiplier (a : Int32.t) (b : Int32.t) =  Int32.mul a b

let multiplier (a : Int32.t) (b : Int32.t) =
  let rec multiplier_rec (a: Int32.t) (b: Int32.t) (result: Int32.t) =
    if (a = Int32.zero)
    then result
    else (
      if ((Int32.logand a 1l) = 1l)
      then multiplier_rec (Int32.shift_right a 1) (Int32.shift_left b 1) (adder result b)
      else multiplier_rec (Int32.shift_right a 1) (Int32.shift_left b 1) (result)
    )
  in multiplier_rec a b 0l

let grey_code a = Int32.logxor a (Int32.shift_right a 1)

let () = print_endline "mult"
let () = print_couple a b
let () = print_mem (false_multiplier a b)
let () = print_mem (multiplier a b)

let () = print_endline "gray code"
let () = print_mem a
let () = print_mem (grey_code a)

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

let rev (a: string): bool=
  let rec rev_rec (a: string) (i: int) (stack: bool list): bool =
    if (i < String.length a)
    then (
      if (is_number a.[i])
      then rev_rec a (i + 1) ((if a.[i] = '1' then true else false) :: stack)
      else rev_rec a (i + 1) (apply_op stack a.[i]);
      )
    else (List.hd stack)
  in rev_rec a 0 []

let () = print_endline "rev"
let str = "011|&"
let () = print_endline str
let () = print_bool (rev str)
let () = print_endline ""
let () = print_endline "truth table"


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
let print_truth (formula: string) = 
  print_truth_line "ABC=";
  print_truth_mid_line "ABC=";
  let arg = sort_string (find_unique (find_letter formula)) in


  let rec print_truth_rec i =
    if i < 1 lsl String.length arg
    then (
      let bin = int_to_bin_str (Int32.of_int i) (String.length arg) in print_truth_line (bin ^ String.make 1 (if (rev (calc bin arg formula)) = true then '1' else '0'));
      print_truth_rec (i + 1)
    )

      in print_truth_rec 0


let toTest = "AB&C|"
(* let () = print_endline toTest *)
(* let bT = int_to_bin_str 3l 3 *)
(* let () = print_endline bT *)
(* let letterT =  sort_string (find_unique (find_letter toTest)) *)
(* let () = print_endline letterT *)
(* let () = print_endline (calc bT letterT toTest) *)
(* let () = print_bool (rev "011|&");print_endline "" *)
let () = print_truth toTest

let () = print_endline ""
let () = print_endline "NNF"


type 'a btree =
  | Empty
  | Node of 'a * 'a btree * 'a btree


  let remove_one_stack stack = 
    match stack with
    | a :: rest -> (a,rest)
    | _ -> failwith "Stack vide"

  let remove_two_stack stack = 
    match stack with
    | a :: b :: rest -> (a,b,rest)
    | _ -> failwith "Stack vide"

let newNode value l r: char btree = Node (value, l, r)

let btree_construct (a: string): char btree=
  let rec btree_construct_rec (a: string) (i: int) (stack: char btree list): char btree =
    if (i < String.length a)
    then (
      if (is_maj a.[i])
      then btree_construct_rec a (i + 1) ((newNode a.[i] Empty Empty) :: stack)
      else if a.[i] = '!'
        then let (first, rest) = remove_one_stack stack in btree_construct_rec a (i + 1) ((newNode a.[i] first Empty) :: rest)
        else let (first, second, rest) = remove_two_stack stack in btree_construct_rec a (i + 1) ((newNode a.[i] first second) :: rest)
          )
      else List.hd stack
          in btree_construct_rec a 0 []


let print_btree (to_str : 'a -> string) (t : 'a btree) : unit =
  let rec aux prefix is_left = function
    | Empty ->
        Printf.printf "%s%s∅\n" prefix (if is_left then "├── " else "└── ")
    | Node (v, l, r) ->
        Printf.printf "%s%s%s\n" prefix (if is_left then "├── " else "└── ") (to_str v);
        let prefix' = prefix ^ (if is_left then "│   " else "    ") in
        (* On affiche d’abord le sous-arbre droit, puis gauche (ça fait un rendu plus lisible) *)
        aux prefix' true  r;
        aux prefix' false l
  in
  match t with
  | Empty -> print_endline "∅"
  | Node (v, l, r) ->
      Printf.printf "%s\n" (to_str v);
      aux "" true  r;
      aux "" false l

let char_to_string c =
  String.make 1 c

(* (A = B) = ((A > B) & (B > A)) *)

(* let rec remove_equivalence (btree: char btree): char btree =  *)
(*   match btree with *)
(*   | Node (v, l, r)-> if v = '=' then Node ('&', Node ('>', remove_equivalence l, remove_equivalence r), Node ('>', remove_equivalence r, remove_equivalence l)) else Node (v, remove_equivalence l, remove_equivalence r) *)
(*   | Empty -> Empty *)

(* (A > B) = (!A | B) *)
let rec remove_implication (btree: char btree): char btree = 
  match btree with
  | Node (v, l, r)-> if v = '>' then Node ('|', remove_implication l, Node ('!', remove_implication r, Empty)) else Node (v, remove_implication l, remove_implication r)
  | Empty -> Empty

let btree_to_RPN (btree: char btree):string = 
  let rec btree_to_RPN_rec btree =
  match btree with
  | Node (v, l, r) ->
      (btree_to_RPN_rec r) ^
      (btree_to_RPN_rec l) ^
      (String.make 1 v)
  | Empty -> ""
  in btree_to_RPN_rec btree
let remove_nor_nand_not (btree: char btree): char btree = 
  match btree with
  | Node (v, l, r)->
      if v = '!'
      then l
      else
        if v = '|'
        then Node ('&',Node ('!', l, Empty) , Node ('!', r, Empty))
        else
          if v = '&'
        then (Node ('|',Node ('!', l, Empty) , Node ('!', r, Empty)))
          else btree
  | Empty -> Empty

let rec remove_not (btree: char btree): char btree = 
  match btree with
  | Node (v, l, _)-> if v = '!' then remove_not (remove_nor_nand_not l) else btree
  | Empty -> Empty



let test = "AB>"
let () = print_endline test
let () = print_btree char_to_string (btree_construct test)
let () = print_btree char_to_string (remove_implication(btree_construct test))
let () = print_endline (btree_to_RPN (btree_construct test))
let () = print_endline (btree_to_RPN (remove_implication (btree_construct test)))

let test = "AB&!"
let () = print_endline test
let () = print_btree char_to_string (btree_construct test)
let () = print_btree char_to_string (remove_not(btree_construct test))
let () = print_endline (btree_to_RPN (btree_construct test))
let () = print_endline (btree_to_RPN (remove_not (btree_construct test)))





























