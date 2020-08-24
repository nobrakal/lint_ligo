type 'a t =
  | Ast_lex of string
  | Ast_node of Simple_utils.Region.t * 'a * 'a t list

let thunked_fold_ast lex node x =
  let rec aux = function
    | Ast_lex s -> lex s
    | Ast_node (p,t,n) -> node p t n (fun () -> List.map aux n)
  in aux x

let string_of_ast f x =
  let rec aux = function
    | Ast_lex x -> x
    | Ast_node (_,t,xs) -> f t ^ "%(" ^ String.concat " " (List.map aux xs) ^ "%)"
  in aux x

let eq_ast x y =
  let rec aux x y = match x,y with
    | Ast_lex x, Ast_lex y -> x = y
    | Ast_node (_,x,xs), Ast_node (_,y,ys) ->
       x = y && List.for_all2 aux xs ys
    | _ -> false
  in aux x y
