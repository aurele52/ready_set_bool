let () = print_endline "negation_normal_form"

let is_maj c =
  c >= 'A' && c <= 'Z'

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
  let rec print_btree_rec prefix is_left = function
    | Empty ->
        Printf.printf "%s%s∅\n" prefix (if is_left then "├── " else "└── ")
    | Node (v, l, r) ->
        Printf.printf "%s%s%s\n" prefix (if is_left then "├── " else "└── ") (to_str v);
        let prefix' = prefix ^ (if is_left then "│   " else "    ") in
        (* On affiche d’abord le sous-arbre droit, puis gauche (ça fait un rendu plus lisible) *)
        print_btree_rec prefix' true  r;
        print_btree_rec prefix' false l
  in
  match t with
  | Empty -> print_endline "∅"
  | Node (v, l, r) ->
      Printf.printf "%s\n" (to_str v);
      print_btree_rec "" true  r;
      print_btree_rec "" false l

let char_to_string c =
  String.make 1 c

(* (A = B) = ((!A & !B) | (A & B)) *)

let rec remove_equivalence (btree: char btree): char btree = 
  match btree with
  | Node (v, l, r)->(
    if v = '='
    then Node ('|', Node ('&', remove_equivalence (Node ('!', l, Empty)), remove_equivalence (Node ('!', r , Empty))), Node ('&', remove_equivalence l, remove_equivalence r))
    else Node (v, remove_equivalence l, remove_equivalence r))
  | Empty -> Empty

  (* A B ^  =  (A & !B) | (!A & B) *)

  let rec remove_xor (btree: char btree): char btree = 
  match btree with
  | Node (v, l, r)->(
    if v = '^'
    then Node ('|', Node ('&', remove_xor l, remove_xor (Node ('!', r , Empty))), Node ('&', remove_xor (Node ('!', l, Empty)), remove_xor r))
    else Node (v, remove_xor l, remove_xor r))
  | Empty -> Empty


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


let rec remove_nor_nand_not (btree: char btree): char btree = 
  match btree with
  | Node ('!', l, _)-> (
    let l = remove_nor_nand_not l in
      match l with
    | Node ('!', el, _)-> remove_nor_nand_not el
    | Node ('|', el, er)-> Node ('&',remove_nor_nand_not (Node ('!', el, Empty)) , remove_nor_nand_not (Node ('!', er, Empty)))
    | Node ('&', el, er)-> (Node ('|',remove_nor_nand_not (Node ('!', el, Empty)) , remove_nor_nand_not (Node ('!', er, Empty))))
    | Empty -> Empty
    | _ -> Node ('!', l, Empty)
  )
  | Empty -> Empty
  | Node (v, l, r) -> Node (v, remove_nor_nand_not l, remove_nor_nand_not r)


let test = "AB|C&!"
let () = print_endline test
let () = print_btree char_to_string (btree_construct test)
let () = print_btree char_to_string (remove_nor_nand_not (remove_implication(remove_equivalence (btree_construct test))))
let () = print_endline (btree_to_RPN (btree_construct test))
let () = print_endline (btree_to_RPN (remove_nor_nand_not (remove_implication(remove_equivalence (btree_construct test)))))


let negation_normal_form (str: string): string = btree_to_RPN (remove_nor_nand_not (remove_implication (remove_equivalence (remove_xor(btree_construct str)))))


let test = "ABC&&!"
let () = print_endline test
let () = print_endline (negation_normal_form test)
let () = print_endline ""

let test = "AB^C>D|"
let () = print_endline test
let () = print_endline (negation_normal_form test)
let () = print_endline ""

let test = "AB=CD|&"
let () = print_endline test
let () = print_endline (negation_normal_form test)
let () = print_endline ""

