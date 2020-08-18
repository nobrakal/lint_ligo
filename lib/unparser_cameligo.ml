open Cameligo.CST
open Pattern

type node = Declarations

let declarations _ = failwith "todo"

let unparse_cst : Cameligo.CST.t -> node Pattern.ast =
  fun cst -> Ast_node (Declarations, declarations cst.decl)
