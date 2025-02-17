(** [Harness] is the module containing all the imperative, side-effecting code
    needed to interact with the operating system and run the test harness. *)

open Generator
open Wrappers
open Printf

exception Harness_error of string

type compiler = Rust | OCaml | Go
type engine = Rust_regex | OCaml_Re | Go_regexp

let compiler_bin = function
  | Rust  -> "rustc"
  | OCaml -> "ocamlc"
  | Go    -> "go"
let engine_compiler = function
  | Rust_regex -> Rust
  | OCaml_Re   -> OCaml
  | Go_regexp  -> Go
let engine_wrapper_path = function
  | Rust_regex -> sprintf "%s/rust-regex-wrap" (Sys.getcwd ())
  | OCaml_Re   -> raise (Harness_error "Unimplemented")
  | Go_regexp  -> raise (Harness_error "Unimplemented")
let engine_wrapper_file = function
  | Rust_regex -> sprintf "%s.rs" (engine_wrapper_path Rust_regex)
  | OCaml_Re   -> raise (Harness_error "Unimplemented")
  | Go_regexp  -> raise (Harness_error "Unimplemented")

let check_compiler (c : compiler) =
  (0 = Sys.command(sprintf "%s 1>/dev/null" (compiler_bin c)))

let generate_wrapper_program (e : engine) (depth : int) =
  match e with
  | Rust_regex -> rust_regex_wrapper (gen_regex depth)
  | OCaml_Re   -> raise (Harness_error "Unimplemented")
  | Go_regexp  -> raise (Harness_error "Unimplemented")

let create_wrapper_file (e : engine) (content : string) =
  let filename = engine_wrapper_file e in
  (* NOTE: The file permissions must be in octal. *)
  let fd = Unix.openfile filename [O_CREAT; O_WRONLY; O_TRUNC] 0o764 in
  let bcontent = Bytes.of_string content in
  let _ = Unix.write fd bcontent 0 (Bytes.length bcontent) in
  Unix.close fd

let compile_wrapper_file (e : engine) =
  let cbin = e |> engine_compiler |> compiler_bin in
  let file = engine_wrapper_file e in
  match e with
  | Rust_regex ->
     let _ = Sys.command(sprintf "/usr/bin/env %s %s" cbin file) in ()
  | _ -> raise (Harness_error "Unimplemented")

(* TODO: An idea for parallelism: have threads on a single core concurrently
   test inputs for one regex compiled for each engine under test. Different
   cores should be running the same thing with different regexes compiled. This
   avoids cross-core synchronization overhead for comparing outputs. *)

let setup_wrapper (e : engine) (depth : int) =
  (* TODO: Make a try/catch here, or whatever OCaml has... *)
  create_wrapper_file e (generate_wrapper_program e depth);
  compile_wrapper_file e

(* TODO: Right now, the plan is simply to have a wrapper that takes in input
   strings as a CLI argument. This may not be the most efficient way to do
   things though and you should probably investigate alternatives. *)
let run_random_input (e : engine) =
  let wrapper = engine_wrapper_path e in
  Sys.command(sprintf "%s %s" wrapper (gen_ascii_input 1024))

let compare_out = ()
