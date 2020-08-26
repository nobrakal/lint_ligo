module type Unparser  = sig
  type cst
  type node

  val string_of_node : node -> string
  val node_of_string : string -> node option

  val unparse_cst : cst -> node Ast.t list
end

module Cameligo  : Unparser with type cst = Cameligo.CST.t = Unparser_cameligo
module Pascaligo : Unparser with type cst = Pascaligo.CST.t = Unparser_pascaligo
