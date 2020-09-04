let main rules file entrypoint =
  let rules = Lexing.from_channel (open_in rules) in
  match Lint_ligo.Main.main ~rules ~file ~entrypoint with
  | Ok None ->
     0
  | Ok (Some result) ->
     print_endline result;
     1
  | Error e ->
     print_endline (Lint_ligo.Errors.to_string e);
     2

open Cmdliner

let rules =
  let doc = "Rules for the linter." in
  Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"RULES_FILE")

let file =
  let doc = "The LIGO contract to lint." in
  Arg.(required & pos 1 (some string) None & info [] ~doc ~docv:"LIGO_FILE")

let entry_point =
  let doc = "The entry point of the contract." in
  Arg.(required & pos 2 (some string) None & info [] ~doc ~docv:"ENTRY_POINT")

let lint =
  let info =
    let doc = "Subcommand: lint a file with the given rules." in
    Term.info ~doc "lint_ligo" in
  let t = Term.(const main $ rules $ file $ entry_point) in
  t,info

let () =
  Term.(exit_status (eval lint))
