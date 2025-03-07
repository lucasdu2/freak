let gen_ascii_char () = Random.int_in_range ~min:32 ~max:126 |> Char.chr
let gen_ascii_string size = String.init size (fun _ -> gen_ascii_char ())
