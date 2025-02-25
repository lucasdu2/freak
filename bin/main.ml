(* open Cmdliner *)
open Freak.Wrappers
open Freak.Grammar
open Freak.Ascii

(* TODO: This is just a messy placeholder to get things to somewhat work. *)
let run () =
  let parent_dir = Unix.getcwd () in
  (* TODO: Put all wrapper directories in a single build directory called
     'wrappers' (or something to that effect). *)
  let _ = Rust_regex.setup_env parent_dir "1.11.1" in
  while true do
    let random_regex = gen_regex 12 in
    let _  = Rust_regex.pre_wrap parent_dir random_regex in
    for _ = 1 to 100 do
      (* TODO: Set up a timeout here and return failure if match takes too long. *)
      let input = (gen_ascii_string 10) in
      print_endline input;
      match Rust_regex.run_wrap parent_dir input with
      | WEXITED 0 -> ()
      | _ -> ()
    done
  done

let _compare_out = ()

let () = run ()
