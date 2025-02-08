open Grammar

(* TODO: Add an upper bound on this recursion to prevent non-termination. *)

let rec gen_regex () : regex =
  match pick_regex_symbol () with
  | CharSet'       -> CharSet(pick_charset ())
  | Not'           -> Not(pick_charset ())
  | And'           -> Not(pick_charset ())
  | StartsWith'    -> StartsWith(gen_regex ())
  | EndsWith'      -> EndsWith(gen_regex ())
  | Concat'        -> Concat(gen_regex (), gen_regex ())
  | Or'            -> Or(gen_regex (), gen_regex ())
  | Optional'      -> Optional(gen_regex ())
  | KleeneStar'    -> KleeneStar(gen_regex ())
  | Repeat'        -> Repeat(gen_regex (), Random.int 30)
  | RepeatAtLeast' -> RepeatAtLeast(gen_regex (), Random.int 30)
  | RepeatRange'   ->
     let start_range = Random.int 10 in
     let end_range = Random.int_in_range ~min:start_range ~max:30 in
     RepeatRange(gen_regex (), start_range, end_range)
