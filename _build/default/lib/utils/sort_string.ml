let sort_string s =
  let chars = String.to_seq s in
  let chars_list = List.of_seq chars in
  let sorted = List.sort Char.compare chars_list in
  let sorted_seq = List.to_seq sorted in
  String.of_seq sorted_seq
