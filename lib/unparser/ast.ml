type 'a t =
  | Ast_lex of string
  | Ast_node of Simple_utils.Region.t * 'a * 'a t list

let thunked_fold_ast lex node x =
  let rec aux = function
    | Ast_lex s -> lex s
    | Ast_node (p,t,n) -> node p t n (fun () -> List.map aux n)
  in aux x

let string_of_ast ?(print_type=fun _ -> "") x =
  let rec aux = function
    | Ast_lex x -> x
    | Ast_node (_,t,xs) ->
       print_type t ^ "%(" ^ String.concat " " (List.map aux xs) ^ "%)"
  in aux x

let eq_ast x y =
  let rec aux x y = match x,y with
    | Ast_lex x, Ast_lex y -> x = y
    | Ast_node (_,x,xs), Ast_node (_,y,ys) ->
       x = y && List.for_all2 aux xs ys
    | _ -> false
  in aux x y

let map_node f x =
  let rec aux = function
    | Ast_lex x -> Ast_lex x
    | Ast_node (a,b,c) -> Ast_node (a, f b, List.map aux c)
  in aux x

let unreg f x = f x.Simple_utils.Region.value

let node t xs pos = Ast_node (pos, t, xs)

let opt_to_list f x = Option.fold ~none:[] ~some:(fun x -> f x) x

let print_nsepseq f g (x,xs) =
  g x :: List.(concat (map (fun (x,y) -> [f x; g y]) xs))

let print_sepseq f g xs =
  match xs with
  | None -> []
  | Some xs -> print_nsepseq f g xs
