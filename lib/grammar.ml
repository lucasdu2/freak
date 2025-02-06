exception Grammar_error of string

type charset =
  | Char | Empty | Any
  | Digit | AnyLetter | CapLetter | LowLetter

(** [regex] is a type that represents the grammar of the regular expressions
    we can generate. We stick to extended regular expressions, without certain
    advanced features like backreferences or lookaround, i.e. we don't support
    NOT over regexes that aren't character classes, CONTAINS,
    etc. This means that our regexes can only recognize regular languages. This
    follows the lead of engines like Google's RE2 and Rust's regex crate. *)
type regex =
  | CharSet of charset
  | Not of charset
  | And of charset
  | StartsWith of regex
  | EndsWith of regex
  | Concat of regex * regex
  | Or of regex * regex
  | Optional of regex
  | KleeneStar of regex
  | Repeat of regex * int
  | RepeatAtLeast of regex * int
  | RepeatRange of regex * int * int

module StringSet = Set.Make(String)

let symbols =
  ["<charset>"; "<not>"; "<and>"; "<startswith>"; "<endswith>"; "<or>";
   "<concat>"; "<optional>"; "<kleenestar>"; "<repeat>"; "<repeatatleast>";
   "<repeatrange>"]

let symbol_of_regex = function
  | CharSet _             -> "<charset>"
  | Not _                 -> "<not>"
  | And _                 -> "<and>"
  | StartsWith _          -> "<startswith>"
  | EndsWith _            -> "<endswith>"
  | Concat (_, _)         -> "<concat>"
  | Or (_, _)             -> "<or>"
  | Optional _            -> "<optional>"
  | KleeneStar _          -> "<kleenestar>"
  | Repeat (_, _)         -> "<repeat>"
  | RepeatAtLeast (_, _)  -> "<repeatatleast>"
  | RepeatRange (_, _, _) -> "<repeatrange>"

let symbol_to_weight =
  [
    ("<charset>"       , 1);
    ("<not>"           , 1);
    ("<and>"           , 1);
    ("<startswith>"    , 2);
    ("<endswith>"      , 2);
    ("<concat>"        , 2);
    ("<or>"            , 2);
    ("<optional>"      , 2);
    ("<kleenestar>"    , 2);
    ("<repeat>"        , 2);
    ("<repeatatleast>" , 2);
    ("<repeatrange>"   , 2);
  ]

let pick_symbol ws =
  let total_weight = List.fold_left (fun acc (_, w) -> acc + w) 0 ws in
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
  weighted_random ws 0
