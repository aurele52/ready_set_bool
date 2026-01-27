
let replace_by (formula: string) (arg: char) (by: char)= 
  let rec replace_by_rec i ret =
    if i < String.length formula then
      replace_by_rec (i + 1) (ret ^ String.make 1 (if formula.[i] = arg then by else formula.[i]))
    else
        ret
  in replace_by_rec 0 ""
