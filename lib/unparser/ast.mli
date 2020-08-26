type 'a t =
  | Ast_lex of string
  | Ast_node of Simple_utils.Region.t * 'a * 'a t list

val string_of_ast : ('a -> string) -> 'a t -> string

val thunked_fold_ast :
  (string -> 'a) ->
  (Simple_utils.Region.t -> 'b -> 'b t list -> (unit -> 'a list) -> 'a) ->
  'b t -> 'a

val eq_ast : 'a t -> 'a t -> bool
val map_node : ('a -> 'b) -> 'a t -> 'b t

val unreg : ('a -> 'b) -> 'a Simple_utils.Region.reg -> 'b

val node : 'a -> 'a t list -> Simple_utils.Region.t -> 'a t

val opt_to_list : ('a -> 'b list) -> 'a option -> 'b list

val print_nsepseq : ('a -> 'b) -> ('c -> 'b) -> 'c * ('a * 'c) list -> 'b list
val print_sepseq : ('a -> 'b) -> ('c -> 'b) -> ('c * ('a * 'c) list) option -> 'b list
