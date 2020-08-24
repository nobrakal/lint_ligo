type 'a pattern =
  | Pat_lex of string
  | Pat_pat of 'a pattern list
  | Pat_var of string * 'a option

val string_of_pattern : 'a pattern -> string

val pat_match : ?debug:bool -> 'a pattern -> 'a -> 'a Unparser.Ast.t -> Simple_utils.Location.t option
