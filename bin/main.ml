let () =
  let rules = Lexing.from_channel (open_in Sys.argv.(1)) in
  let ast = read_line () in
  match Lint_ligo.Main.main_serialized ~rules ~ast with
  | None -> exit 0
  | Some result ->
     print_endline result;
     exit 1
