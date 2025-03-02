(* open Eio *) (* TODO: Implement concurrency/parallelism later *)
(* open Cmdliner *)
(* open Uuidm *)
open Freak.Wrappers
open Freak.Grammar
open Freak.Ascii
open Printf

let gen_ascii_input_string size =
  String.init size (fun _ -> gen_ascii_char ())

(* TODO: Let's output everything to stdout for now and pipe to a file for
   debugging for now. It generally might be more efficient just to do batch
   comparisons on file output rather than doing a comparison after each check.*)
let _compare_out = ()

let run () =
  let parent_dir = Unix.getcwd () in
  let wrapper_dir = sprintf "%s/_freak_wrappers" parent_dir in
  (* TODO: As with all mkdir commands, figure out more principled way to set up
     permissions. *)
  if Sys.file_exists(wrapper_dir) then
    (* Clean up _freak_wrappers first if it exists *)
    let _ = Unix.system (Filename.quote_command "rm" ["-rf"; wrapper_dir]) in ();
  Unix.mkdir wrapper_dir 0o777;
  let _ = Go_regexp.setup_env wrapper_dir "" in
  let _ = Rust_regex.setup_env wrapper_dir "1.11.1" in
  while true do
    let random_regex = gen_regex 7 in
    (* NOTE: We use Go's realized regex as a readable regex for logs. *)
    print_endline (sprintf "TEST REGEX: %s" (Go_regexp.realize_regex random_regex));
    let _  = Go_regexp.pre_wrap wrapper_dir random_regex in
    let _  = Rust_regex.pre_wrap wrapper_dir random_regex in
    for _ = 1 to 100 do
      (* TODO: Set up a timeout here and return failure if match takes too long. *)
      let input = (gen_ascii_input_string 32) in
      print_endline input;
      print_endline "rust regex: ";
      let _ = Rust_regex.run_wrap wrapper_dir input in
      print_endline "go regexp: ";
      let _ = Go_regexp.run_wrap wrapper_dir input in
      print_endline "=====";
    done
  done

let () = run ()
