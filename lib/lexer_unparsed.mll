{ (* -*- tuareg -*- *)

  open Parser

}

(** Regexpr **)
let newline = ('\010' | '\013' | "\013\010")
let blank   = [' ' '\009' '\012']

let digit = ['0'-'9']
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let ident = (lowercase | uppercase | digit | '_')+
let eident = '%'ident

rule token = parse
  (** Layout *)
  | newline  { token lexbuf }
  | blank+   { token lexbuf }
  | eof      { EOF }
  | "%("     { MLP }
  | "%)"     { MRP }
  | eident as id ":" ident as typ { TVar (id,typ)}
  | eident as id { Var id }
  | [^' ' '\009' '\012']+ as str { Word str }
  | _ { failwith ("unexpected character: " ^ (Lexing.lexeme lexbuf)) }
