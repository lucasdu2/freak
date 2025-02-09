open Grammar
open Printf

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

let pick_random_chars_ascii () =
  let size = Random.int 5 in
  let get_random_char () = Random.int 256 |> Char.chr |> Char.escaped in
  let rec aux s acc = if (s = 0) then acc
                      else aux (s - 1) (acc ^ get_random_char ()) in
  aux size ""

let rec realize_regex_rust (r: regex) : string =
  let realize_cs = function
    | Char -> pick_random_chars_ascii ()
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
  | StartsWith p -> "^" ^ realize_regex_rust p
  | EndsWith p -> realize_regex_rust p ^ "$"
  | Concat (p1, p2) -> realize_regex_rust p1 ^ realize_regex_rust p2
  | Or (p1, p2) -> realize_regex_rust p1 ^ "|" ^ realize_regex_rust p2
  | Optional p -> realize_regex_rust p ^ "?"
  | KleeneStar p -> realize_regex_rust p ^ "*"
  | Repeat (p, n) -> realize_regex_rust p ^ (sprintf "{%d}" n)
  | RepeatAtLeast (p, n) -> realize_regex_rust p ^ (sprintf "{%d,}" n)
  | RepeatRange (p, startn, endn) ->
     realize_regex_rust p ^ (sprintf "{%d,%d}" startn endn)
