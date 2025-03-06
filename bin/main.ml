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
  let cwd = Unix.getcwd () in
  let wrapper_dir = sprintf "%s/_freak_wrappers" cwd in
  (* Clean up _freak_wrappers first if it exists *)
  if Sys.file_exists(wrapper_dir) then
    let _ = Unix.system (Filename.quote_command "rm" ["-rf"; wrapper_dir]) in ();
  (* TODO: As with all mkdir commands, figure out more principled way to set up
     permissions. *)
  Unix.mkdir wrapper_dir 0o777;
  let _ = Go_regexp.setup_env wrapper_dir "" in
  let _ = Rust_regex.setup_env wrapper_dir "1.11.1" in
  let mismatch_dir = sprintf "%s/_mismatches_found" cwd in
  Unix.mkdir mismatch_dir 0o777;
  while true do
    let random_regex = gen_regex 5 in
    let realized_regex = realize_re2_regex random_regex in
    let _  = Go_regexp.pre_wrap wrapper_dir random_regex in
    let _  = Rust_regex.pre_wrap wrapper_dir random_regex in
    for _ = 1 to 100 do
      let input = (gen_ascii_input_string 64) in
      (* TODO: Read results from stdout file descriptor, do live multithreaded
         comparison for each input. Save full inputs and wrappers for each
         discrepancy. *)
      let rust_regex_out = Rust_regex.run_wrap wrapper_dir input in
      let go_regexp_out = Go_regexp.run_wrap wrapper_dir input in
      if not (String.equal rust_regex_out go_regexp_out) then
        begin
          (* If there is a mismatch, save input and regex engine outputs,
             along with corresponding wrapper files, into a directory. The
             name of the directory is a base64 encoding of the realized regex
             string (to try and maintain uniqueness). *)
          let regex_mismatch_dir =
            let b64enc_regex = Filename.quote (Base64.encode_exn realized_regex) in
            sprintf "%s/%s" mismatch_dir b64enc_regex in
          if not (Sys.file_exists regex_mismatch_dir) then
            begin
              Unix.mkdir regex_mismatch_dir 0o777;
              let rust_regex_file =
                Rust_regex.wrapper_file_full wrapper_dir in
              let _ =
                Unix.system(Filename.quote_command
                              "cp" [rust_regex_file; regex_mismatch_dir]) in
              let go_regexp_file =
                Go_regexp.wrapper_file_full wrapper_dir in
              let _ =
                Unix.system(Filename.quote_command
                              "cp" [go_regexp_file; regex_mismatch_dir]) in ()
            end;
          (* Write mismatch output to a file. *)
          let mismatch_file = sprintf "%s/mismatches.log" regex_mismatch_dir in
          let b_mismatch_out =
            sprintf "mismatch on input %s\n==> rust-regex: %s\n==> go-regexp: %s\n"
              input rust_regex_out go_regexp_out |> Bytes.of_string in
          let fd = Unix.openfile mismatch_file [O_CREAT; O_WRONLY; O_APPEND] 0o764 in
          let _ = Unix.write fd b_mismatch_out 0 (Bytes.length b_mismatch_out) in
          Unix.close fd
        end
    done
  done

let () = run ()
