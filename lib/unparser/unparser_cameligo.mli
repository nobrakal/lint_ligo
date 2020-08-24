type node =
  | Declarations
  | LetDecl | TypeDecl
  | Expr
  | Par
  | Attribute
  | Type
  | Pattern
  | Field | Field_assign | Field_decl
  | Path | Clause
  | Name
  | Keyword

val string_of_node : node -> string

val node_of_string : string -> node option

val unparse_cst : Cameligo.CST.t -> node Ast.t list
