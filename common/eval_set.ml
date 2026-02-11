module Int32Set = Set.Make(struct
  type t = int32
  let compare = Int32.compare
end)

let apply_op_couple (stack: Int32Set.t list) op =
  match stack with
  | a :: b :: rest -> (if op = '|' then Int32Set.union a b else Int32Set.inter a b) :: rest
  | _ -> stack



let apply_neg (stack: Int32Set.t list) (univer: Int32Set.t) =
  match stack with
  | a :: rest -> (Int32Set.diff univer a) :: rest
  | _ -> stack


let apply_op stack op univers=
  if (op = '!') then apply_neg stack univers else apply_op_couple stack op

let set_of (sets : Int32Set.t list) (a: char) : Int32Set.t =
  List.nth sets (Char.code a - Char.code 'A')

let find_univer (sets : Int32Set.t list) : Int32Set.t =
  List.fold_left Int32Set.union Int32Set.empty sets

let eval_set (formula: string) (sets: Int32Set.t list): Int32Set.t = 
  let formula = Negation_normal_form.negation_normal_form formula in
  let univers: Int32Set.t  = find_univer sets in
  let rec eval_formula_rec (i: int) (stack: Int32Set.t list): Int32Set.t =
    if (i < String.length formula)
    then (
      if (Utils.Is_maj.is_maj formula.[i])
      then eval_formula_rec (i + 1) (set_of sets formula.[i]  :: stack)
      else eval_formula_rec (i + 1) (apply_op stack formula.[i] univers);
      )
    else (List.hd stack)
  in eval_formula_rec 0 []
