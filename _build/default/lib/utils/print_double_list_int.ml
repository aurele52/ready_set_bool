let print_double_liste_int (l : Int.t List.t List.t) =
  let rec aux = function
    | [] -> ()
    | [x] ->
        Print_list_int.print_liste x
    | x :: xs ->
        Print_list_int.print_liste x;
        print_string ", ";
        aux xs
  in
  print_string "{";
  aux l;
  print_string "}"

