type depreciate =
  { dep : string;
    dep_version : int;
    dep_replacement : string option;
    dep_message : string option
  }

type pattern =
  | Lexeme of string
  | Pat of pattern list
  | Var of string

type annoted_pattern =
  { pat: string;
    pat_message: string
  }

type rule =
  | Depreciate of depreciate
  | Pattern of annoted_pattern
