open Btree
let btree_to_RPN (btree: char btree):string = 
  let rec btree_to_RPN_rec btree =
  match btree with
  | Node (v, l, r) ->
      (btree_to_RPN_rec r) ^
      (btree_to_RPN_rec l) ^
      (String.make 1 v)
  | Empty -> ""
  in btree_to_RPN_rec btree
