open Btree
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
