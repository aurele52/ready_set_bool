let find_unique (formula: string) =
  let rec find_unique_rec i ret =
    if i = String.length formula then
      ret
    else
      let c = formula.[i] in
      if String.contains ret c then
        find_unique_rec (i + 1) ret
      else
        find_unique_rec (i + 1) (ret ^ String.make 1 c)
  in find_unique_rec 0 ""
