{ (* -*- tuareg -*- *)

  open Parser

}

(** Regexpr **)
let newline = ('\010' | '\013' | "\013\010")
let blank   = [' ' '\009' '\012']

let digit = ['0'-'9']
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let ident = '%'(lowercase | uppercase | digit)+

rule token = parse
  (** Layout *)
  | newline  { token lexbuf }
  | blank+   { token lexbuf }
  | eof      { EOF }
  | "%("     { MLP }
  | "%)"     { MRP }
  | ident as id { String id }
  | [^' ' '\009' '\012']+ as str { Word str }
  | _ { failwith ("unexpected character: " ^ (Lexing.lexeme lexbuf)) }
