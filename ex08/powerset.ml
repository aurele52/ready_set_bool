let () = print_endline "Powerset"

let powerset (set: Int32.t list): Int32.t List.t List.t =
  let rec powerset_rec i (ret: int32 list list): int32 list list =
    if i < (1 lsl (List.length set))
    then (
      let rec create_vect a ret =
        if a = List.length set
        then List.rev ret
        else if (i land (1 lsl a)) <> 0 then
          create_vect (a + 1) (List.nth set a :: ret)
        else
          create_vect (a + 1) ret in
      powerset_rec (i + 1) (create_vect 0 [] :: ret);
      
    )
    else
      List.rev ret

  in powerset_rec 0 []



let print_liste (l : Int32.t List.t) =
  let rec aux = function
    | [] -> ()
    | [x] ->
        print_dec_uint32 x
    | x :: xs ->
        print_dec_uint32 x;
        print_string ", ";
        aux xs
  in
  print_string "{";
  aux l;
  print_string "}"

let print_double_liste (l : Int32.t List.t List.t) =
  let rec aux = function
    | [] -> ()
    | [x] ->
        print_liste x
    | x :: xs ->
        print_liste x;
        print_string ", ";
        aux xs
  in
  print_string "{";
  aux l;
  print_string "}"

let test = [1l;2l;3l]

let () = print_double_liste (powerset test)
