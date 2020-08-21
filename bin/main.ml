let main f rules =
  let rules = Lexing.from_channel (open_in rules) in
  match f ~rules with
  | None ->
     0
  | Some result ->
     print_endline result;
     1

let main_compiler rules =
  let ast = read_line () in
  main (Lint_ligo.Main.main_serialized ~ast) rules

let main_file rules file =
  main (Lint_ligo.Main.main_file ~file) rules

open Cmdliner

let rules =
  let doc = "Rules for the linter." in
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"RULES_FILE")

let lint =
  let doc = "The LIGO file to lint." in
  Arg.(required & pos 1 (some string) None & info [] ~doc ~docv:"LIGO_FILE")

let cmd_compiler =
  let info = Term.info "compiler" in
  Term.(const main_compiler $ rules), info

let cmd_file =
  let info = Term.info "file" in
  Term.(const main_file $ rules $ lint), info

let () =
  Term.exit_status @@ Term.eval_choice cmd_compiler [cmd_compiler; cmd_file]
