(* open Eio *) (* TODO: Implement concurrency/parallelism later *)
(* open Cmdliner *)
(* open Uuidm *)
open Freak.Wrappers
open Freak.Grammar
open Freak.Ascii
open Printf

let gen_ascii_input_string size =
  String.init size (fun _ -> gen_ascii_char ())

let run () =
  let parent_dir = Unix.getcwd () in
  let wrapper_dir = sprintf "%s/_freak_wrappers" parent_dir in
  (* TODO: As with all mkdir commands, figure out more principled way to set up
     permissions. *)
  Unix.mkdir wrapper_dir 0o777;
  let _ = Go_regexp.setup_env wrapper_dir "" in
  let _ = Rust_regex.setup_env wrapper_dir "1.11.1" in
  while true do
    let random_regex = gen_regex 12 in
    print_endline (sprintf "Compiling regex %s" (Go_regexp.realize_regex random_regex));
    let _  = Go_regexp.pre_wrap wrapper_dir random_regex in
    let _  = Rust_regex.pre_wrap wrapper_dir random_regex in
    for _ = 1 to 100 do
      (* TODO: Set up a timeout here and return failure if match takes too long. *)
      let input = (gen_ascii_input_string 100) in
      print_endline input;
      print_endline "rust regex: ";
      let _ = Rust_regex.run_wrap wrapper_dir input in
      print_endline "go regexp: ";
      let _ = Go_regexp.run_wrap wrapper_dir input in
      print_endline "=====";
    done
  done

let _compare_out = ()

let () = run ()
