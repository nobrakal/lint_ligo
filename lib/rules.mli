type depreciate =
  { name : string;
    version : int;
    replacement : string option;
    message : string option
  }

type rule =
  | Depreciate of depreciate
