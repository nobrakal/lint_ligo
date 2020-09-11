val main :
  ?syntax:string -> ?rules:string -> file:string -> entrypoint:string
  -> (string option, Errors.t) result
