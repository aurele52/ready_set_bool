
let print_liste (l : Int.t List.t) =
  let rec print_el_rec = function
    | [] -> ()
    | [x] ->
        print_int x
    | x :: xs ->
        print_int x;
        print_string ", ";
        print_el_rec xs
  in
  print_string "{";
  print_el_rec l;
  print_string "}"
