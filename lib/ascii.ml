let gen_ascii_char () = Random.int_in_range ~min:32 ~max:126 |> Char.chr
