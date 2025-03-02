(** [Wrappers] contains the imperative, side-effecting code needed to interact
    with a Unix-like operating system and run the test wrappers. *)
open Grammar
open Ascii
open Printf

exception Setup_error of string

let check_exists (exe : string) : unit =
  (* NOTE: This script is dependent on 'whereis' --- this probably limits its
     usage to Linux systems. *)
  let cmd = Filename.quote_command "whereis" ~stdout:"/dev/null" [exe] in
  match Unix.system(cmd) with
  | WEXITED 0 -> ()
  | WEXITED _ ->
      raise (Setup_error
              (sprintf "Cannot find %s; ensure it is installed." exe))
  | WSIGNALED s -> raise (Setup_error (sprintf "%s killed by signal %d." exe s))
  | WSTOPPED s -> raise (Setup_error (sprintf "%s killed by signal %d." exe s))

(* TODO: Instead of returning a raw Unix.process_status, try to return some
   option type defined in this module. *)
module type WRAPPER = sig
  val realize_regex : regex -> string
  val produce_program : regex -> string
  val setup_env : string -> string -> Unix.process_status
  val pre_wrap : string -> regex -> Unix.process_status
  val run_wrap : string-> string -> Unix.process_status
end

(* TODO P1: Need to fix Rust regex generation to stop failing various syntactic
   checks. *)
module Rust_regex : WRAPPER = struct
  let compiler = "rustc"
  let project_dir = "rust-regex-wrap"

  let gen_ascii_string size =
    let ascii_string_escaped () =
      let c = gen_ascii_char () in
      (* TODO: There is probably more to do here. *)
      match c with
      | '\\' -> {|\\|}
      | '{' -> {|\{|}
      | '}' -> {|\}|}
      | '[' -> {|\[|}
      | ']' -> {|\]|}
      | '(' -> {|\(|}
      | ')' -> {|\)|}
      | '+' -> {|\+|}
      | '$' -> {|\$|}
      | '^' -> {|\^|}
      | '\"' -> {|\\"|}
      | _ -> (Bytes.make 1 c) |> Bytes.to_string
    in
    let rec aux str s =
      if s = 0 then str else
        aux ((ascii_string_escaped ()) ^ str) (s - 1)
    in
    aux "" size

  let rec realize_regex (r: regex) : string =
    let realize_cs = function
      | Char -> gen_ascii_string ((Random.int 5) + 1)
      (* TODO: Should figure out how to handle this "empty" case; also how to
         generally get random whitespace and newlines/returns. *)
      | Empty -> " "
      | Any -> "."
      | Digit -> "[0-9]"
      | AnyLetter -> "[a-zA-Z]"
      | CapLetter -> "[A-Z]"
      | LowLetter -> "[a-z]"
    in
    match r with
    | CharSet cs ->
       let set = realize_cs cs in
       if ((String.length set) = 1) then set else "[" ^ set ^ "]"
    | Not cs -> "[^" ^ realize_cs cs ^ "]"
    | And (cs1, cs2) -> "[" ^ realize_cs cs1 ^ "&&" ^ realize_cs cs2 ^ "]"
    | StartsWith p -> "^" ^ realize_regex p
    | EndsWith p -> realize_regex p ^ "$"
    | Concat (p1, p2) -> realize_regex p1 ^ realize_regex p2
    | Or (p1, p2) -> realize_regex p1 ^ "|" ^ realize_regex p2
    (* NOTE: Add parentheses around all repetition operators as a semi-hack that
       prevents syntactic errors on nested repetitions (at the cost of adding
       capture group semantics). *)
    | Optional p -> "(" ^ realize_regex p ^ ")" ^ "?"
    | KleeneStar p -> "(" ^ realize_regex p ^ ")" ^ "*"
    | Repeat (p, n) -> "(" ^ realize_regex p ^ ")" ^ (sprintf "{%d}" n)
    | RepeatAtLeast (p, n) -> "(" ^ realize_regex p ^ ")" ^ (sprintf "{%d,}" n)
    | RepeatRange (p, startn, endn) ->
       "(" ^ realize_regex p ^ ")" ^ (sprintf "{%d,%d}" startn endn)

  let produce_program r =
    let realr = realize_regex r in
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

  (* [setup_env] contains all the side-effecting imperative code needed to get
     the Rust build environment --- i.e. the Rust compiler and the Cargo build
     tool --- up and ready. *)
  let setup_env parent_dir engine_version =
    (* Ensure compiler exists *)
    check_exists(compiler);
    (* Ensure cargo is installed *)
    check_exists("cargo");
    (* Create project directory *)
    let full_project_dir = sprintf "%s/%s" parent_dir project_dir in
    (* TODO: Try to set up permissions (and general structure!) for these files
       in a more principled way. *)
    if (not (Sys.file_exists full_project_dir)) then
      Unix.mkdir full_project_dir 0o777;
    (* Set up basic cargo project*)
    let _ = Unix.system(Filename.quote_command "cargo" ["init"; full_project_dir]) in
    (* Ensure required crates are installed *)
    Unix.chdir full_project_dir;
    let _ = Unix.system(Filename.quote_command "cargo" ["add"; "once_cell"]) in
    let regex_crate_name = sprintf "regex@%s" engine_version in
    Unix.system(Filename.quote_command "cargo" ["add"; regex_crate_name])

  let pre_wrap parent_dir r =
    let full_project_dir = sprintf "%s/%s" parent_dir project_dir in
    (* Write wrapper program to file *)
    let content = produce_program r in
    (* NOTE: src/main.rs is the cargo convention for entry point *)
    let filename = sprintf "%s/src/main.rs" full_project_dir in
    (* NOTE: The file permissions must be in octal. *)
    let fd = Unix.openfile filename [O_CREAT; O_WRONLY; O_TRUNC] 0o764 in
    let bcontent = Bytes.of_string content in
    let _ = Unix.write fd bcontent 0 (Bytes.length bcontent) in
    Unix.close fd;
    Unix.chdir full_project_dir;
    let build_cmd = Filename.quote_command "cargo" ["build"] in
    Unix.system(build_cmd)

  let run_wrap parent_dir input =
    let full_project_dir = sprintf "%s/%s" parent_dir project_dir in
    (* NOTE: cargo builds executables in target/debug *)
    let executable = sprintf "%s/target/debug/%s" full_project_dir project_dir in
    let cmd = Filename.quote_command executable [input] in
    Unix.system(cmd)
end

module Go_regexp : WRAPPER = struct
  let compiler = "go"
  let project_dir = "go-regexp-wrap"

  let gen_ascii_string size =
    let ascii_string_escaped () =
      let c = gen_ascii_char () in
      match c with
      | '\\' -> {|\\|}
      | '\"' -> {|\"|}
      | _ -> (Bytes.make 1 c) |> Bytes.to_string
    in
    let rec aux str s =
      if s = 0 then str else
        aux ((ascii_string_escaped ()) ^ str) (s - 1)
    in
    aux "" size

  let rec realize_regex (r: regex) : string =
    let realize_cs = function
      | Char -> gen_ascii_string ((Random.int 5) + 1)
      (* TODO: Should figure out how to handle this "empty" case; also how to
         generally get random whitespace and newlines/returns. *)
      | Empty -> " "
      | Any -> "."
      | Digit -> "0-9"
      | AnyLetter -> "a-zA-Z"
      | CapLetter -> "A-Z"
      | LowLetter -> "a-z"
    in
    match r with
    | CharSet cs ->
       let set = realize_cs cs in
       if ((String.length set) = 1) then set else "[" ^ set ^ "]"
    | Not cs -> "[^" ^ realize_cs cs ^ "]"
    | And (cs1, cs2) -> "[" ^ realize_cs cs1 ^ "&&" ^ realize_cs cs2 ^ "]"
    | StartsWith p -> "^" ^ realize_regex p
    | EndsWith p -> realize_regex p ^ "$"
    | Concat (p1, p2) -> realize_regex p1 ^ realize_regex p2
    | Or (p1, p2) -> realize_regex p1 ^ "|" ^ realize_regex p2
    (* NOTE: Add parentheses around all repetition operators as a semi-hack that
       prevents syntactic errors on nested repetitions (at the cost of adding
       capture group semantics). *)
    | Optional p -> "(" ^ realize_regex p ^ ")" ^ "?"
    | KleeneStar p -> "(" ^ realize_regex p ^ ")" ^ "*"
    | Repeat (p, n) -> "(" ^ realize_regex p ^ ")" ^ (sprintf "{%d}" n)
    | RepeatAtLeast (p, n) -> "(" ^ realize_regex p ^ ")" ^ (sprintf "{%d,}" n)
    | RepeatRange (p, startn, endn) ->
       "(" ^ realize_regex p ^ ")" ^ (sprintf "{%d,%d}" startn endn)

  let produce_program r =
    let realr = realize_regex r in
    sprintf
"package main

import (
    \"fmt\"
    \"os\"
    \"regexp\"
)

func main() {
    input := os.Args[1]
    r := regexp.MustCompile(regexp.QuoteMeta(\"%s\"))
    if (r.MatchString(input)) {
        fmt.Println(\"1\")
    } else {
        fmt.Println(\"0\")
    }
}" realr

  let setup_env parent_dir engine_version =
    (* Ensure compiler exists *)
    check_exists(compiler);
    (* Create project directory *)
    let full_project_dir = sprintf "%s/%s" parent_dir project_dir in
    (* TODO: Try to set up permissions (and general structure!) for these files
       in a more principled way. *)
    if (not (Sys.file_exists full_project_dir)) then
      Unix.mkdir full_project_dir 0o777;
    Unix.chdir full_project_dir;
    let init_status = Unix.system(Filename.quote_command "go"
                                    ["mod"; "init"; project_dir]) in
    (* Ensure specified regexp version is installed *)
    (* NOTE: This check seems fairly brittle. *)
    if String.length(engine_version) <> 0 then
      let reqcmd = sprintf "-require regexp@%s" engine_version in
      Unix.system(Filename.quote_command "go" ["mod"; "edit"; reqcmd])
    else
      init_status

  let pre_wrap parent_dir r =
    let full_project_dir = sprintf "%s/%s" parent_dir project_dir in
    (* Write wrapper program to file *)
    let content = produce_program r in
    let filename = sprintf "%s/main.go" full_project_dir in
    (* NOTE: The file permissions must be in octal. *)
    let fd = Unix.openfile filename [O_CREAT; O_WRONLY; O_TRUNC] 0o764 in
    let bcontent = Bytes.of_string content in
    let _ = Unix.write fd bcontent 0 (Bytes.length bcontent) in
    Unix.close fd;
    Unix.chdir full_project_dir;
    let build_cmd = Filename.quote_command "go" ["build"] in
    Unix.system(build_cmd)

  let run_wrap parent_dir input =
    let executable = sprintf "%s/%s/%s" parent_dir project_dir project_dir in
    let cmd = Filename.quote_command executable [input] in
    Unix.system(cmd)
end
