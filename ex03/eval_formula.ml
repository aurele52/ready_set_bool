
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

let () = print_endline "eval_formula"
let str = "011|&"
let () = print_endline str
let () = print_bool (eval_formula str)
