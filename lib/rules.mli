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

type cst =
  | Camel_cst of Cameligo.CST.t
  | Pascal_cst of Pascaligo.CST.t
  | Reason_cst of Reasonligo.CST.t

type stage =
  | Typed of Ast_typed.program
  | Cst of cst

val rules_of_parsed : parsed_rules -> (rules, Errors.t) result
