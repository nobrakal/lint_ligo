let filter_some xs =
  List.fold_right (fun x acc -> match x with None -> acc | Some x -> x::acc) xs []

let sequence_result xs =
  let add_if_possible x acc =
    match x,acc with
    | Ok x,Ok xs -> Ok (x::xs)
    | (Error _ as e),_ | _,(Error _ as e) -> e in
  List.fold_right add_if_possible xs (Ok [])

let list_map_to_opt f xs =
  match xs with
  | [] -> None
  | xs -> Some (f xs)

let string_of_var =
  let buf = Buffer.create 16 in
  fun v ->
  let f = Format.formatter_of_buffer buf in
  Simple_utils.Var.pp f v; Format.pp_print_flush f ();
  let var = Buffer.contents buf in
  Buffer.clear buf; var

module Let_syntax = struct
  let return x = Ok x
  let map x ~f = Result.map f x
  let bind x ~f = Result.bind x f
  let both x y =
    match x,y with
    | Ok x, Ok y -> Ok (x,y)
    | Error e,_ | _, Error e -> Error e
end
