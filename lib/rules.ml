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

type parsed_rules =
  { plang : string;
    prules: rule list
  }

type rules =
  { lang : Compile.Helpers.v_syntax;
    deps : depreciate list;
    pats : annoted_pattern list
  }

let split xs =
  let aux x (deps,pats) =
    match x with
    | Depreciate x -> (x::deps,pats)
    | Pattern x -> (deps,x::pats) in
  List.fold_right aux xs ([],[])

let rules_of_parsed {plang;prules} =
  match Compile.Helpers.(syntax_to_variant (Syntax_name plang) None) with
  | Ok (lang,_) ->
     let deps,pats = split prules in
     Ok {lang;deps;pats}
  | Error _ -> Error (Errors.Bad_language plang)
