let main f rules =
  let rules = Lexing.from_channel (open_in rules) in
  match f ~rules with
  | Ok None ->
     0
  | Ok (Some result) ->
     print_endline result;
     1
  | Error e ->
     print_endline (Lint_ligo.Errors.to_string e);
     2

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

let cmd_default =
  let doc = "Ligo Linter" in
  let info = Term.info ~doc "ligo_lint" in
  Term.(const 1), info

let cmd_compiler =
  let doc = "Subcommand: interface with the LIGO compiler." in
  let info = Term.info ~doc "compiler" in
  Term.(const main_compiler $ rules), info

let cmd_lint =
  let doc = "Subcommand: lint a file with the given rules." in
  let info = Term.info ~doc "lint" in
  Term.(const main_file $ rules $ lint), info

let () =
  Term.exit_status @@ Term.eval_choice cmd_default [cmd_compiler; cmd_lint]
