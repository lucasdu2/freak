open Grammar
open Printf

(** [gen_regex] generates a random regex up to a depth of [depth] using a
    define grammar and set of weights for the grammar rules. *)
let rec gen_regex depth : regex =
  if (depth = 0) then CharSet(pick_charset ())
  else
    let depth' = depth - 1 in
    match pick_regex_symbol () with
    | CharSet'       -> CharSet(pick_charset ())
    | Not'           -> Not(pick_charset ())
    | And'           -> Not(pick_charset ())
    | StartsWith'    -> StartsWith(gen_regex depth')
    | EndsWith'      -> EndsWith(gen_regex depth')
    | Concat'        -> Concat(gen_regex depth', gen_regex depth')
    | Or'            -> Or(gen_regex depth', gen_regex depth')
    | Optional'      -> Optional(gen_regex depth')
    | KleeneStar'    -> KleeneStar(gen_regex depth')
    | Repeat'        -> Repeat(gen_regex depth', Random.int 30)
    | RepeatAtLeast' -> RepeatAtLeast(gen_regex depth', Random.int 30)
    | RepeatRange'   ->
       let start_range = Random.int 10 in
       let end_range = Random.int_in_range ~min:start_range ~max:30 in
       RepeatRange(gen_regex depth', start_range, end_range)

let random_ascii_char () = Random.int 256 |> Char.chr
let gen_ascii_option_string maxchars =
  let size = Random.int maxchars in
  let rec aux s acc =
    if (s = 0) then acc
    else aux (s - 1) (acc ^ (random_ascii_char () |> Char.escaped)) in
  aux size ""

(** [realize_rust_regex] realizes a syntactically valid regex for the rust-regex
    library from a general symbolic regex. *)
let rec realize_rust_regex (r: regex) : string =
  let realize_cs = function
    | Char -> gen_ascii_option_string 5
    | Empty -> ""
    | Any -> "."
    | Digit -> "[0-9]"
    | AnyLetter -> "[a-zA-Z]"
    | CapLetter -> "[A-Z]"
    | LowLetter -> "[a-z]"
  in
  match r with
  | CharSet cs -> realize_cs cs
  | Not cs -> "[^" ^ realize_cs cs ^ "]"
  | And (cs1, cs2) -> "[" ^ realize_cs cs1 ^ "&&" ^ realize_cs cs2 ^ "]"
  | StartsWith p -> "^" ^ realize_rust_regex p
  | EndsWith p -> realize_rust_regex p ^ "$"
  | Concat (p1, p2) -> realize_rust_regex p1 ^ realize_rust_regex p2
  | Or (p1, p2) -> realize_rust_regex p1 ^ "|" ^ realize_rust_regex p2
  | Optional p -> realize_rust_regex p ^ "?"
  | KleeneStar p -> realize_rust_regex p ^ "*"
  | Repeat (p, n) -> realize_rust_regex p ^ (sprintf "{%d}" n)
  | RepeatAtLeast (p, n) -> realize_rust_regex p ^ (sprintf "{%d,}" n)
  | RepeatRange (p, startn, endn) ->
     realize_rust_regex p ^ (sprintf "{%d,%d}" startn endn)


(** [gen_input] creates a byte array containing [size] random bytes. Note that
 a byte is represented by the OCaml [char] type in the [Bytes] module. *)
let gen_ascii_input size = Bytes.init size (fun _ -> random_ascii_char ())
