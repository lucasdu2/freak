open Stdio
open Grammar
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
let engine_wrapper_fname = function
  | Rust_regex -> sprintf "%s/rust-regex-wrap.rs" (Sys.getcwd ())
  | OCaml_Re   -> raise (Harness_error "Unimplemented")
  | Go_regexp  -> raise (Harness_error "Unimplemented")

let check_compiler (c : compiler) =
  (0 = Sys.command(compiler_bin c))

(* TODO: There's some annoyances with OCaml's vanilla IO libraries. Try
   switching to Jane Street's Stdio to see if things are a bit easier. *)
let create_wrapper_file (e : engine) (content : string) =
  let fd = Unix.openfile (engine_wrapper_fname e) [O_CREAT; O_TRUNC] 644 in
  Unix.write fd


let compile_wrapper_file (e : engine) (r : regex) =
  match e with
  | Rust_regex ->

     Sys.command("/usr/bin/env %s " (rust_regex_wrapper r))
  | _ -> raise (Harness_error "Unimplemented")

let run = ()

let compare_out = ()
