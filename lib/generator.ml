open Grammar

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
