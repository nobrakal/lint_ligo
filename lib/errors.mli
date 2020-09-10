(* All errors of the linter *)
type t =
  | Bad_type of string
  | Ast_parsing of string
  | Compiler of Main_errors.all
  | Bad_language of string
  | TypeMismatch

val to_string : t -> string
