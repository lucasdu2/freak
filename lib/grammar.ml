(** [regex] is a type that represents the grammar of the regular expressions
    we can generate. We stick to extended regular expressions (standard regular
    expressions extended with AND and OR), without certain advanced features
    like backreferences or lookaround. This means that our regexes can only
    recognize regular languages. This follows the lead of regex engines like
    Google's RE2 and Rust's regex crate, which limit expressiveness in this way
    in exchange for guaranteed avoidance of exponential time complexity (which
    can be exploited in ReDoS attacks). *)
type regex =
  | CharSet
  | Empty
  | Null
  | StartsWith of regex
  | EndsWith of regex
  | Contains of regex
  | Not of regex
  | Optional of regex
  | KleeneStar of regex
  | Concat of regex
  | Or of regex
  | And of regex
  | Repeat of regex * int
  | RepeatAtLeast of regex * int
  | RepeatRange of regex * int * int
