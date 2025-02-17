open Grammar
open Ascii
open Printf

module type Wrapper = sig
  val compiler : string
  val executable_path : string -> string
  val realize_regex : regex -> string
  val setup_env : unit -> unit
  val produce_program : regex -> string
  val build_program : string -> regex -> unit
  val run : string -> unit
end

module Rust_regex : Wrapper = struct
  let compiler = "rustc"
  let executable_path prefix =
    let exe = "rust_regex_wrap" in
    sprintf "%s/%s" prefix exe
  let rec realize_regex (r: regex) : string =
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
    | StartsWith p -> "^" ^ realize_regex p
    | EndsWith p -> realize_regex p ^ "$"
    | Concat (p1, p2) -> realize_regex p1 ^ realize_regex p2
    | Or (p1, p2) -> realize_regex p1 ^ "|" ^ realize_regex p2
    | Optional p -> realize_regex p ^ "?"
    | KleeneStar p -> realize_regex p ^ "*"
    | Repeat (p, n) -> realize_regex p ^ (sprintf "{%d}" n)
    | RepeatAtLeast (p, n) -> realize_regex p ^ (sprintf "{%d,}" n)
    | RepeatRange (p, startn, endn) ->
       realize_regex p ^ (sprintf "{%d,%d}" startn endn)
  let setup_env () = ()
  let produce_program r =
    let realr = realize_regex r in
    (* TODO: See if you can construct a string using quoted string literals and
       then use format_of_string to convert it into a format string. *)
    sprintf
"extern crate once_cell; extern crate regex;
use {
    once_cell::sync::Lazy,
    regex::Regex,
    std::env,
};

fn matcher(haystack: &str) -> bool {
    static RE: Lazy<Regex> = Lazy::new(|| Regex::new(r\"%s\").unwrap());
    RE.is_match(haystack)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let haystack = &args[1];
    if matcher(haystack) {
        println!(\"1\")
    } else {
        println!(\"0\")
    }
}" realr

  let build_program prefix r =
    let content = produce_program r in
    let exec = executable_path prefix in
    let filename = exec ^ ".rs" in
    (* NOTE: The file permissions must be in octal. *)
    (* TODO: It really would be nice to do this with Jane Street's Core and nice
      Stdio modules. But it's unclear if they work well with Eio, which I would
      like to use for parallelism. *)
    let fd = Unix.openfile filename [O_CREAT; O_WRONLY; O_TRUNC] 0o764 in
    let bcontent = Bytes.of_string content in
    let _ = Unix.write fd bcontent 0 (Bytes.length bcontent) in
    Unix.close fd;
    let _ = Sys.command(sprintf "/usr/bin/env %s %s" compiler exec) in ()

  let _setup_env () =
    (* Ensure compiler exists *)
    (* Ensure cargo is installed *)
    (* Ensure required crates are installed *)
    ()
  let run _ = ()

end
