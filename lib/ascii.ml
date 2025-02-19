(** [gen_ascii_string] creates a string containing [size] random bytes. Note
    that a byte is represented by the [char] type in the [Bytes] module. *)
let gen_ascii_string size =
  Bytes.to_string (Bytes.init size (fun _ -> Random.int 256 |> Char.chr))

let gen_ascii_option_string maxchars =  gen_ascii_string (Random.int maxchars)
