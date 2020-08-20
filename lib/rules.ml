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
