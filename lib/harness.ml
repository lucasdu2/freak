(** [Harness] is the module containing all the imperative, side-effecting code
    needed to interact with the operating system and run the test harness. *)

open Generator
open Grammar
open Wrappers
open Ascii

exception Harness_error of string

type engine = Rust_regex

(* TODO: An idea for parallelism: have threads on a single core concurrently
   test inputs for one regex compiled for each engine under test. Different
   cores should be running the same thing with different regexes compiled. This
   avoids cross-core synchronization overhead for comparing outputs. *)

(* TODO: Right now, the plan is simply to have a wrapper that takes in input
   strings as a CLI argument. This may not be the most efficient way to do
   things though and you should probably investigate alternatives. *)
let setup_test (e : engine) (r : regex) (prefix : string) =
  match e with
  | Rust_regex ->
     Rust_regex.setup_env ();
     Rust_regex.build_program prefix r

let run_random_input (e : engine) =
  match e with
  | Rust_regex -> Rust_regex.run (gen_ascii_input 1024)

let run_test () =
  setup_test Rust_regex (gen_regex 12) ".";
  for _ = 1 to 10000 do
    run_random_input Rust_regex
  done


let compare_out = ()
