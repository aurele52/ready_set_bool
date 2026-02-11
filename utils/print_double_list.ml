let print_double_liste (l : Int32.t List.t List.t) =
  let rec aux = function
    | [] -> ()
    | [x] ->
        Print_list.print_liste x
    | x :: xs ->
        Print_list.print_liste x;
        print_string ", ";
        aux xs
  in
  print_string "{";
  aux l;
  print_string "}"

