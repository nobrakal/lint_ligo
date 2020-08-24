type 'a t =
  | Ast_lex of string
  | Ast_node of Simple_utils.Region.t * 'a * 'a t list

val string_of_ast : ('a -> string) -> 'a t -> string

val thunked_fold_ast :
  (string -> 'a) ->
  (Simple_utils.Region.t -> 'b -> 'b t list -> (unit -> 'a list) -> 'a) ->
  'b t -> 'a

val eq_ast : 'a t -> 'a t -> bool
