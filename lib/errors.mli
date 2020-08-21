type t =
  | Bad_type of string
  | Ast_parsing of string
  | Compiler of Main_errors.all

val to_string : t -> string
