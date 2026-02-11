
let sat (formula: string): bool = 
  let arg = Utils.Sort_string.sort_string (Utils.Find_unique.find_unique (Utils.Find_letter.find_letter formula)) in

  let rec sat_rec i =
    if i < 1 lsl String.length arg
    then (
      let bin = Utils.Int_to_bin_str.int_to_bin_str (Int32.of_int i) (String.length arg) in if (Eval_formula.eval_formula (Utils.Calc.calc bin arg formula)) = true then true else
      sat_rec (i + 1))
    else
      false

      in (sat_rec 0)

