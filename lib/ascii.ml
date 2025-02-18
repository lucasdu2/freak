let random_ascii_char () = Random.int 256 |> Char.chr
let gen_ascii_option_string maxchars =
  let size = Random.int maxchars in
  let rec aux s acc =
    if (s = 0) then acc
    else aux (s - 1) (acc ^ (random_ascii_char () |> Char.escaped)) in
  aux size ""

(** [gen_ascii_input] creates a string containing [size] random bytes. Note that
    a byte is represented by the [char] type in the [Bytes] module. *)
let gen_ascii_input size =
  Bytes.to_string (Bytes.init size (fun _ -> random_ascii_char ()))
