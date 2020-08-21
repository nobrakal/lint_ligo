{ (* -*- tuareg -*- *)

  open Parser

}

(** Regexpr **)
let newline = ('\010' | '\013' | "\013\010")
let blank   = [' ' '\009' '\012']

let digit = ['0'-'9']
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let onlyletters = (lowercase | uppercase | digit )
let ident = onlyletters (onlyletters | '_')*
let eident = '%'ident

rule token = parse
  (** Layout *)
  | newline  { token lexbuf }
  | blank+   { token lexbuf }
  | eof      { EOF }
  | "%("     { MLP }
  | "%)"     { MRP }
  | "%_"     { TWild None }
  | "%_:" (ident as typ) { TWild (Some typ) }
  | eident as id ":" (ident as typ) { TVar (id,Some typ)}
  | eident as id { TVar (id,None) }
  | [^' ' '\009' '\012']+ as str { Word str }
  | _ { failwith ("unexpected character: " ^ (Lexing.lexeme lexbuf)) }
