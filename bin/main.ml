(* open Cmdliner *)
open Freak.Wrappers
open Freak.Grammar
open Freak.Ascii

(* CLI should have flags for:
   - run the harness, this should also have debug output options
   - return a random regex realized for a specific engine
   - return a random input string *)

(* TODO: This is just a messy placeholder to get things to somewhat work. *)
let run () =
  let parent_dir = Unix.getcwd () in
  let random_regex = gen_regex 12 in
  let _ = Rust_regex.setup parent_dir "1.11.1" random_regex in
  for _ = 1 to 10000 do
    (* TODO: Set up a timeout here and return failure if match takes too long. *)
    match Rust_regex.run parent_dir (gen_ascii_string 5) with
    | WEXITED 0 -> ()
    | _ -> ()
  done

let _compare_out = ()

let () = run ()
