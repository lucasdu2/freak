(* open Cmdliner *)
open Freak.Harness

(* CLI should have flags for:
   - run the harness, this should also have debug output options
   - return a random regex realized for a specific engine
   - return a random input string *)
let () = run ()
