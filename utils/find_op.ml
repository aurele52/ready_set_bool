
let find_op op = 
  match op with
  | '=' -> (=)
  | '|' -> (||)
  | '&' -> (&&)
  | '^' -> (<>)
  | _ -> (=)

