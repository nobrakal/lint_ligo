type t =
  | Bad_type of string

let to_string = function
  | Bad_type t -> "Type " ^ t ^ " does not exist"
