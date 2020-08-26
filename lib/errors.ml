type t =
  | Bad_type of string
  | Ast_parsing of string
  | Compiler of Main_errors.all
  | Bad_language of string
  | TypeMismatch

let compiler_err err =
  let open Simple_utils.Display in
  let buff = Buffer.create 42 in
  let format = Format.formatter_of_buffer buff in
  Main_errors.Formatter.error_format.pp ~display_format:Human_readable format err;
  Format.pp_print_flush format ();
  Buffer.contents buff

let to_string = function
  | Bad_type t -> "Type " ^ t ^ " does not exist"
  | Ast_parsing t -> "Cannot parse the AST: " ^ t
  | Compiler c -> compiler_err c
  | Bad_language x -> "Unknown language: " ^ x
  | TypeMismatch -> "There is a type mismatch between the file and the languages of patterns"
