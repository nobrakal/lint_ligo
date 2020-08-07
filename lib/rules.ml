type deprecated =
  { name : string;
    replacement : string option;
    message : string option
  }

type rule =
  | Deprecated of deprecated
