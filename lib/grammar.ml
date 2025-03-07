exception Grammar_error of string

type charset =
  | None
  | Chars of string
  | Any
  | Digit
  | AnyLetter
  | CapLetter
  | LowLetter

(** [regex] is a type that represents the grammar of the regular expressions
    we can generate. We stick to "extended" regular expressions, without certain
    advanced features like backreferences or lookaround, which means that our
    regexes can only recognize regular languages. This follows the lead of
    engines like Google's RE2 and Rust's regex crate. *)
type regex =
  | Empty
  | CharSet of charset
  | Not of charset
  | And of charset * charset
  | StartsWith of regex
  | EndsWith of regex
  | Concat of regex * regex
  | Or of regex * regex
  | Optional of regex
  | KleeneStar of regex
  | Repeat of regex * int
  | RepeatAtLeast of regex * int
  | RepeatRange of regex * int * int

let regex_weights =
  [
    (CharSet None             , 4);
    (Not None                 , 2);
    (And (None, None)         , 1);
    (StartsWith Empty         , 2);
    (EndsWith Empty           , 2);
    (Concat (Empty, Empty)    , 3);
    (Or (Empty, Empty)        , 2);
    (Optional Empty           , 2);
    (KleeneStar Empty         , 2);
    (Repeat (Empty, 0)        , 2);
    (RepeatAtLeast (Empty, 0) , 1);
    (RepeatRange (Empty, 0, 0), 1);
  ]

let charset_weights =
  [
    (Chars "" , 15);
    (Any      , 1);
    (Digit    , 3);
    (AnyLetter, 1);
    (CapLetter, 2);
    (LowLetter, 2);
  ]

(** [pick_weighted] randomly picks from a weighted list of symbols using the
    provided symbol weights. *)
let pick_weighted wl =
  let total_weight =
    List.fold_left (fun acc (_, w) -> acc + w) 0 wl in
  let random = Random.int total_weight in
  let rec weighted_random l weight_acc =
    match l with
    | [] -> raise (Grammar_error "No more symbols to choose from")
    | [(sym, _)] -> sym
    | (sym, w) :: xs ->
       let weight_acc' = weight_acc + w in
       if random < weight_acc' then sym
       else weighted_random xs weight_acc'
  in
  weighted_random wl 0


let pick_regex () = pick_weighted regex_weights
let pick_charset () =
  (* If charset picked is Chars, immediately generate a random option string as
     an unescaped string. Further escaping should be done with regex is fully
     realized for an engine. *)
  match pick_weighted charset_weights with
  | Chars _ -> Chars (Ascii.gen_ascii_string ((Random.int 5) + 1))
  | cs -> cs

(** [gen_regex] generates a random regex up to a depth of [depth] using a
    defined grammar and set of weights for the grammar rules. *)
let rec gen_regex depth : regex =
  if (depth = 0) then CharSet(pick_charset ())
  else
    let depth' = depth - 1 in
    match pick_regex () with
    | Empty                 -> Empty
    | CharSet _             -> CharSet(pick_charset ())
    | Not _                 -> Not(pick_charset ())
    | And (_, _)            -> Not(pick_charset ())
    | StartsWith _          -> StartsWith(gen_regex depth')
    | EndsWith _            -> EndsWith(gen_regex depth')
    | Concat (_, _)         -> Concat(gen_regex depth', gen_regex depth')
    | Or (_, _)             -> Or(gen_regex depth', gen_regex depth')
    | Optional _            -> Optional(gen_regex depth')
    | KleeneStar _          -> KleeneStar(gen_regex depth')
    | Repeat (_, _)         -> Repeat(gen_regex depth', Random.int 15)
    | RepeatAtLeast (_, _)  -> RepeatAtLeast(gen_regex depth', Random.int 5)
    | RepeatRange (_, _, _) ->
       let start_range = Random.int 5 in
       let end_range = Random.int_in_range ~min:start_range ~max:15 in
       RepeatRange(gen_regex depth', start_range, end_range)
