(** [Wrappers] contains the imperative, side-effecting code needed to interact
    with a Unix-like operating system and run the test wrappers. *)
open Grammar
open Printf

exception Setup_error of string

let check_exists (exe : string) : unit =
  let cmd = Filename.quote_command exe ~stdout:"/dev/null" [] in
  match Unix.system(cmd) with
  | WEXITED 0 -> ()
  | WEXITED _ ->
      raise (Setup_error
              (sprintf "Cannot find %s; make sure it is installed and in your PATH." exe))
  | WSIGNALED s -> raise (Setup_error (sprintf "%s killed by signal %d." exe s))
  | WSTOPPED s -> raise (Setup_error (sprintf "%s killed by signal %d." exe s))

module type WRAPPER = sig
  val realize_regex : regex -> string
  val setup : string -> string -> regex -> Unix.process_status
  val run : string-> string -> Unix.process_status
end

module Rust_regex : WRAPPER = struct
  open Grammar
  open Ascii
  let compiler = "rustc"
  let project_dir = "rust-regex-wrap"

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

  (* [setup] contains all the ugly side-effecting imperative code needed. *)
  let setup parent_dir engine_version r =
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
    let _ = Unix.system(Filename.quote_command "cargo" ["add"; regex_crate_name]) in
    (* Write wrapper program to file *)
    let content = produce_program r in
    (* NOTE: src/main.rs is the cargo convention for entry point *)
    let filename = sprintf "%s/%s/src/main.rs" parent_dir project_dir in
    (* NOTE: The file permissions must be in octal. *)
    let fd = Unix.openfile filename [O_CREAT; O_WRONLY; O_TRUNC] 0o764 in
    let bcontent = Bytes.of_string content in
    let _ = Unix.write fd bcontent 0 (Bytes.length bcontent) in
    Unix.close fd;
    let build_cmd = Filename.quote_command "cargo" ["build"] in
    Unix.system(build_cmd)

  let run parent_dir input =
    let full_project_dir = sprintf "%s/%s" parent_dir project_dir in
    Unix.chdir full_project_dir;
    let cmd = Filename.quote_command "cargo" ["run"; input] in
    Unix.system(cmd)

end

(* TODO: An idea for parallelism: have threads on a single core concurrently
   test inputs for one regex compiled for each engine under test. Different
   cores should be running the same thing with different regexes compiled. This
   avoids cross-core synchronization overhead for comparing outputs. *)
