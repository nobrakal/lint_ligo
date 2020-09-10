val filter_some : 'a option list -> 'a list

val sequence_result : ('a, 'b) result list -> ('a list, 'b) result

val list_map_to_opt : ('a list -> 'b) -> 'a list -> 'b option

val string_of_var : Ast_typed.expression_ Simple_utils.Var.t -> string

module Let_syntax : sig
  val return : 'a -> ('a, 'b) result
  val map : ('a, 'b) result -> f:('a -> 'c) -> ('c, 'b) result
  val bind : ('a, 'b) result -> f:('a -> ('c, 'b) result) -> ('c, 'b) result
  val both : ('a, 'b) result -> ('c, 'b) result -> ('a * 'c, 'b) result
end
