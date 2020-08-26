module Make (U : Unparser.Unparse.Unparser) :
  sig
    val pattern :
      ?debug:bool ->
      Rules.annoted_pattern ->
      U.node Unparser.Ast.t ->
      ((Simple_utils.Location.t * string) option, Errors.t) result

    val run_cst :
      U.cst ->
      Rules.annoted_pattern ->
      ((Simple_utils.Location.t * string) list, Errors.t) result
  end
