open Utils.Btree
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


let negation_normal_form (str: string): string = btree_to_RPN (remove_nor_nand_not (remove_implication (remove_equivalence (remove_xor(Utils.Btree_construct.btree_construct str)))))
