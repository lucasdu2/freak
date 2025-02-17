(* open Cmdliner *)
open Freak.Harness

(* CLI should have flags for:
   - run the harness, this should also have debug output options
   - return a random regex realized for a specific engine
   - return a random input string *)
let () =
  if (check_compiler Rust)
  then
    setup_wrapper Rust_regex 12
  (* if not (0 = run_random_input Rust_regex) then *)
  (*   printf "Failure\n" *)
  (* else *)
  (*   printf "Success\n" *)
