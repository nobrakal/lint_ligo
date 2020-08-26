type depreciate =
  { dep : string;
    dep_version : string;
    dep_replacement : string option;
    dep_message : string option
  }

type annoted_pattern =
  { pat: string;
    pat_type: string;
    pat_message: string
  }

type rule =
  | Depreciate of depreciate
  | Pattern of annoted_pattern

let split xs =
  let aux x (deps,pats) =
    match x with
    | Depreciate x -> (x::deps,pats)
    | Pattern x -> (deps,x::pats) in
  List.fold_right aux xs ([],[])
