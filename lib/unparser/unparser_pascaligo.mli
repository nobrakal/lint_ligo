type node =
  | Name | Keyword
  | AttributeDeclaration | TypeDeclaration | ConstDeclaration | FunDeclaration | VarDeclaration
  | Par | Brace | Bracket
  | Type | TVariant | TFieldDecl
  | Expr
  | Instruction
  | Param
  | Case
  | Pattern
  | Block
  | WhileLoop | ForLoop
  | Binding | FieldPathAssignement | FieldAssignment

val string_of_node : node -> string
val node_of_string : string -> node option

val unparse_cst : Pascaligo.CST.t -> node Ast.t list
