let () = print_endline "Map Reverse"
let () = let (x, y) = Reverse_map.reverse_map (Common.Map.map 3l 2l) in (Utils.Print_dec_uint32.print_dec_uint32 x; print_endline " "; Utils.Print_dec_uint32.print_dec_uint32 y)
let () = print_endline ""
let () = let (x, y) = Reverse_map.reverse_map (Common.Map.map 10220l 12334l) in (Utils.Print_dec_uint32.print_dec_uint32 x; print_endline " "; Utils.Print_dec_uint32.print_dec_uint32 y)




