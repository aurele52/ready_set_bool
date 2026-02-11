module Int32Set = Eval_set.Int32Set

let () =
  (* ---- Test 1 ---- *)
  let sets1 = [
    Int32Set.of_list [0l; 1l; 2l];
    Int32Set.of_list [0l; 3l; 4l];
  ] in

  let result1 = Eval_set.eval_set "AB&" sets1 in
  (* attendu : [0] *)

  (* ---- Test 2 ---- *)
  let sets2 = [
    Int32Set.of_list [0l; 1l; 2l];
    Int32Set.of_list [3l; 4l; 5l];
  ] in

  let result2 = Eval_set.eval_set "AB|" sets2 in
  (* attendu : [0; 1; 2; 3; 4; 5] *)

  (* ---- Test 3 ---- *)
  let sets3 = [
    Int32Set.of_list [0l; 1l; 2l];
  ] in

  let result3 = Eval_set.eval_set "A!" sets3 in
  (* attendu : [] *)

  let print_set s =
    Int32Set.iter (fun x ->
      Printf.printf "%ld " x
    ) s;
    print_newline ()
  in

  print_endline "Test 1:";
  print_set result1;

  print_endline "Test 2:";
  print_set result2;

  print_endline "Test 3:";
  print_set result3
