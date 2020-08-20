type 'a pattern =
  | Pat_lex of string
  | Pat_pat of 'a pattern list
  | Pat_var of string * 'a option

val string_of_pattern : 'a pattern -> string

type 'a ast =
  | Ast_lex of string
  | Ast_node of 'a * 'a ast list

val string_of_ast : ('a -> string) -> 'a ast -> string

val pat_match : ?debug:bool -> 'a pattern -> 'a ast -> bool
