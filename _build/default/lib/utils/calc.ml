
let calc bin letter formula = 
  let rec calc_rec i formula =
    if i < String.length letter
    then calc_rec (i + 1) (Replace_by.replace_by formula letter.[i] bin.[i])
    else formula
  in calc_rec 0 formula
