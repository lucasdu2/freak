(* open Eio *) (* TODO: Implement concurrency/parallelism later *)
(* open Cmdliner *)
(* open Uuidm *)
open Freak.Wrappers
open Freak.Grammar
open Freak.Ascii
open Printf

let gen_ascii_input_string size =
  String.init size (fun _ -> gen_ascii_char ())

let compare_out input wrapper_dir =
  (* TODO: Should consider how this will interact with logging --- if logs are
     turned on, should save all inputs and related output to a file. Otherwise,
     only save mismatches (as you do already here). *)
  let rust_regex_out = Rust_regex.run_wrap wrapper_dir input in
  let go_regexp_out = Go_regexp.run_wrap wrapper_dir input in
  if not (String.equal rust_regex_out go_regexp_out) then
    print_endline (sprintf "mismatch (rust-regex=%s, go-regexp=%s) on input %s"
                     rust_regex_out go_regexp_out input)

let run () =
  let parent_dir = Unix.getcwd () in
  let wrapper_dir = sprintf "%s/_freak_wrappers" parent_dir in
  (* Clean up _freak_wrappers first if it exists *)
  if Sys.file_exists(wrapper_dir) then
    let _ = Unix.system (Filename.quote_command "rm" ["-rf"; wrapper_dir]) in ();
  (* TODO: As with all mkdir commands, figure out more principled way to set up
     permissions. *)
  Unix.mkdir wrapper_dir 0o777;
  let _ = Go_regexp.setup_env wrapper_dir "" in
  let _ = Rust_regex.setup_env wrapper_dir "1.11.1" in
  while true do
    let random_regex = gen_regex 8 in
    print_endline (sprintf "TEST REGEX: %s" (Go_regexp.realize_regex random_regex));
    let _  = Go_regexp.pre_wrap wrapper_dir random_regex in
    let _  = Rust_regex.pre_wrap wrapper_dir random_regex in
    for _ = 1 to 100 do
      (* TODO: Set up a timeout here. *)
      let input = (gen_ascii_input_string 64) in
      (* TODO: Read results from stdout file descriptor, do live multithreaded
         comparison for each input. Save full inputs and wrappers for each
         discrepancy. *)
      compare_out input wrapper_dir
    done
  done

let () = run ()
