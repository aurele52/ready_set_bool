
let sat (formula: string): bool = 
  let arg = sort_string (find_unique (find_letter formula)) in

  let rec sat_rec i =
    if i < 1 lsl String.length arg
    then (
      let bin = int_to_bin_str (Int32.of_int i) (String.length arg) in if (rev (calc bin arg formula)) = true then true else
      sat_rec (i + 1))
    else
      false

      in (sat_rec 0)

let () = print_endline ""
let () = print_endline "SAT"

let test = "AB=CD=|"      (* (A=B) | (C=D) *)
let () = print_endline (test)
let () = print_bool (sat (test))
