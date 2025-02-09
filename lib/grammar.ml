exception Grammar_error of string

type charset =
  | Char
  | Empty
  | Any
  | Digit
  | AnyLetter
  | CapLetter
  | LowLetter

(** [regex] is a type that represents the grammar of the regular expressions
    we can generate. We stick to extended regular expressions, without certain
    advanced features like backreferences or lookaround, i.e. we don't support
    NOT over regexes that aren't character classes, CONTAINS,
    etc. This means that our regexes can only recognize regular languages. This
    follows the lead of engines like Google's RE2 and Rust's regex crate. *)
type regex =
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

type symbols =
  | CharSet'
  | Not'
  | And'
  | StartsWith'
  | EndsWith'
  | Concat'
  | Or'
  | Optional'
  | KleeneStar'
  | Repeat'
  | RepeatAtLeast'
  | RepeatRange'

let symbol_of_regex = function
  | CharSet _             -> CharSet'
  | Not _                 -> Not'
  | And _                 -> And'
  | StartsWith _          -> StartsWith'
  | EndsWith _            -> EndsWith'
  | Concat (_, _)         -> Concat'
  | Or (_, _)             -> Or'
  | Optional _            -> Optional'
  | KleeneStar _          -> KleeneStar'
  | Repeat (_, _)         -> Repeat'
  | RepeatAtLeast (_, _)  -> RepeatAtLeast'
  | RepeatRange (_, _, _) -> RepeatRange'

let symbol_weights =
  [
    (CharSet'      , 1);
    (Not'          , 1);
    (And'          , 1);
    (StartsWith'   , 2);
    (EndsWith'     , 2);
    (Concat'       , 2);
    (Or'           , 2);
    (Optional'     , 2);
    (KleeneStar'   , 2);
    (Repeat'       , 2);
    (RepeatAtLeast', 2);
    (RepeatRange'  , 2);
  ]

let charset_weights =
  [
    (Char     , 1);
    (Empty    , 1);
    (Any      , 1);
    (Digit    , 1);
    (AnyLetter, 1);
    (CapLetter, 1);
    (LowLetter, 1);
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

let pick_regex_symbol () = pick_weighted symbol_weights
let pick_charset () = pick_weighted charset_weights
