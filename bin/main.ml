let () =
  let rules = Lexing.from_channel (open_in Sys.argv.(1)) in
  let ast = read_line () in
  print_endline (Lint_ligo.Main.main_serialized ~rules ~ast)
