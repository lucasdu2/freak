(** [gen_ascii_string] creates a string containing [size] random bytes. Note
    that a byte is represented by the [char] type in the [Bytes] module. *)
let gen_ascii_string size =
  (Bytes.init size
     (fun _ -> Random.int_in_range ~min:32 ~max:126 |> Char.chr)) |>
    Bytes.to_string

let gen_ascii_option_string maxchars =  gen_ascii_string (Random.int maxchars)
