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

val rules_of_parsed : parsed_rules -> (rules, Errors.t) result
