
(* let powerset (set: Int32.t list): Int32.t List.t List.t = *)
(*   let rec powerset_rec i (ret: int32 list list): int32 list list = *)
(*     if i < (1 lsl (List.length set)) *)
(*     then ( *)
(*       let rec create_vect a ret = *)
(*         if a = List.length set *)
(*         then List.rev ret *)
(*         else if (i land (1 lsl a)) <> 0 then *)
(*           create_vect (a + 1) (List.nth set a :: ret) *)
(*         else *)
(*           create_vect (a + 1) ret in *)
(*       powerset_rec (i + 1) (create_vect 0 [] :: ret); *)
(**)
(*     ) *)
(*     else *)
(*       List.rev ret *)
(**)
(*   in powerset_rec 0 [] *)

let add_first_el_to_list (el: Int32.t) (liste: Int32.t list)=
  el :: liste


let add_first_el_to_all_list_of_list (liste: Int32.t list list) (el: Int32.t) =
  List.map (add_first_el_to_list el) liste



let powerset (set: Int32.t list): Int32.t List.t List.t =
  let rec powerset_rec (todo: Int32.t list) =
    match todo with
    | [] -> [[]]
    | first :: reste -> let fut_sub = powerset_rec reste in fut_sub @ add_first_el_to_all_list_of_list fut_sub first

  in powerset_rec set



