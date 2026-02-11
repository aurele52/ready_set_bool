
let print_liste (l : Int32.t List.t) =
  let rec print_el_rec = function
    | [] -> ()
    | [x] ->
        Print_dec_uint32.print_dec_uint32 x
    | x :: xs ->
        Print_dec_uint32.print_dec_uint32 x;
        print_string ", ";
        print_el_rec xs
  in
  print_string "{";
  print_el_rec l;
  print_string "}"
