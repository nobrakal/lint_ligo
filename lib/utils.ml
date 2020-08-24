let filter_some xs =
  List.fold_right (fun x acc -> match x with None -> acc | Some x -> x::acc) xs []

let sequence_result xs =
  let add_if_possible x acc =
    match x,acc with
    | Ok x,Ok xs -> Ok (x::xs)
    | (Error _ as e),_ | _,(Error _ as e) -> e in
  List.fold_right add_if_possible xs (Ok [])
