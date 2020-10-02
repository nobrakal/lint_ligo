type cst = Pascaligo.CST.t

type node =
  | Name | Keyword
  | TypeDeclaration | ConstDeclaration | FunDeclaration | VarDeclaration
  | Par | Brace | Bracket
  | Type | TVariant | TFieldDecl
  | Attribute
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

val unparse_cst : cst -> node Ast.t list
