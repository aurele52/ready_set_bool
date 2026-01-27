open Utils.Btree

let rec distrib (btree: char btree): char btree = 
  match btree with
  | Empty -> Empty
  | Node ('|', l, r)-> (
    let l = distrib l in
    let r = distrib r in
    match l with
    | Node ('&', el, er)-> Node ('&', distrib (Node ('|', el, r)), distrib (Node ('|', er, r)))
    | _ ->
      match r with
      | Node ('&', el, er)-> Node ('&', distrib (Node ('|', l, el)), distrib (Node ('|', l, er)))
      | _ -> Node ('|', l, r)
    )
  | Node (v, l, r) -> Node (v, distrib l, distrib r)



let btree_to_RPN_app (btree: char btree):string = 
  let rec btree_to_RPN_app_rec btree =
  match btree with
  | Node ('|', _, _) -> (let (suite, nbr) = count_or btree in (suite ^ String.make (nbr-1) '|'))
  | Node ('&', _, _) -> (let (suite, nbr) = count_and btree in (suite ^ String.make (nbr-1) '&'))
  | Node (v, l, r) ->
      (btree_to_RPN_app_rec r) ^
      (btree_to_RPN_app_rec l) ^
      (String.make 1 v)
  | Empty -> ""

and count_or btree = match btree with
| Node ('|', r, l) -> let ls, ln = count_or l in let rs, rn = count_or r in(ls ^ rs, ln + rn)
| _ -> (btree_to_RPN_app_rec btree, 1)

and count_and btree = match btree with
| Node ('&', r, l) -> let ls, ln = count_and l in let rs, rn = count_and r in(ls ^ rs, ln + rn)
| _ -> (btree_to_RPN_app_rec btree, 1)


  in btree_to_RPN_app_rec btree



let conjunctive_normal_form (str: string): string = btree_to_RPN_app (distrib (Utils.Btree_construct.btree_construct (Ex05.Negation_normal_form.negation_normal_form str)))

