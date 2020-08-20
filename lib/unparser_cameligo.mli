type node =
  | Ndeclarations
  | NLet | NTypeDecl
  | NExpr
  | Par
  | Attribute
  | TCartesian | TArrow | TSum | TRecord | TTuple | TApp | TVariant
  | Pattern
  | Field | Field_assign | Field_decl
  | Path | Clause
  | Name

val unparse_cst : Cameligo.CST.t -> node Pattern.ast
