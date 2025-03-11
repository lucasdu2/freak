(* open Eio *)
(* open Cmdliner *)
open Freak.Wrappers
open Freak.Grammar
open Freak.Ascii
open Printf

let gen_ascii_input_string size =
  String.init size (fun _ -> gen_ascii_char ())

(* TODO: As with all mkdir commands, figure out more principled way to set up
   permissions. *)
let create_dir_clean dirname =
  (* Clean up directory first if it already exists *)
  (if Sys.file_exists(dirname) then
    let _ = Unix.system (Filename.quote_command "rm" ["-rf"; dirname]) in ());
  Unix.mkdir dirname 0o777;
  print_endline (sprintf "created directory %s" dirname)

let run () =
  let thread_id = Thread.self () |> Thread.id in
  print_endline (sprintf "spawning thread %d" thread_id);
  let cwd = Unix.getcwd () in
  let wrapper_dir = sprintf "%s/_freak_wrappers_%d" cwd thread_id in
  print_endline (sprintf "creating wrapper dir at %s" wrapper_dir);
  create_dir_clean wrapper_dir;
  print_endline (sprintf "created wrapper dir at %s" wrapper_dir);
  let mismatch_dir = sprintf "%s/_mismatches_found_%d" cwd thread_id in
  print_endline (sprintf "creating mismatch dir at %s" mismatch_dir);
  create_dir_clean mismatch_dir;
  print_endline (sprintf "created mismatch dir at %s" mismatch_dir);
  (* TODO: Allow the engine versions to be user-specified *)
  let _ = Go_regexp.setup_env wrapper_dir "" in
  let _ = Rust_regex.setup_env wrapper_dir "1.11.1" in

  while true do
    let random_regex = gen_regex 5 in
    (* NOTE: Here, we use the identity function for the extra escapes function
       argument, since we just want to use the common escaped characters. *)
    let realized_regex = realize_re2_regex random_regex (fun s -> s) in
    let _  = Go_regexp.pre_wrap wrapper_dir random_regex in
    let _  = Rust_regex.pre_wrap wrapper_dir random_regex in
    for _ = 1 to 2048 do
      let input = (gen_ascii_input_string 128) in
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
            (* Replace '/' with "(backslash)" *)
            (* NOTE: This replacement is only sufficient on Linux/Unix --- there
               is a larger set of disallowed filename characters on Windows. *)
            let b64enc_final =
              (String.fold_left
                 (fun acc c ->
                   if Char.equal c '/' then
                     acc ^ "(backslash)"
                   else acc ^ String.make 1 c)
                 "" b64enc_regex) in
            sprintf "%s/%s" mismatch_dir b64enc_final in
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

let () =
  print_endline "hellloooooooo";
  (* TODO: Ideally, make regex generation depth customizable in the CLI. *)
  let th1 = Thread.create run () in
  let th2 = Thread.create run () in
  Thread.join th1;
  Thread.join th2
