let find_letter (formula: string) =
  let rec find_letter_rec i ret =
    if i = String.length formula then
      ret
    else
      let c = formula.[i] in
      if (Is_maj.is_maj c) then
        find_letter_rec (i + 1) (ret ^ String.make 1 c)
      else
        find_letter_rec (i + 1) ret
  in find_letter_rec 0 ""
