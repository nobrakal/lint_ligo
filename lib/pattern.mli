type 'a pattern =
  | Pat_lex of string
  | Pat_pat of 'a pattern list
  | Pat_var of string * 'a option

type 'a ast =
  | Ast_lex of string
  | Ast_node of 'a * 'a ast list

val pat_match : 'a pattern -> 'a ast -> bool
