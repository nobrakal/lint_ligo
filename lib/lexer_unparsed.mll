{ (* -*- tuareg -*- *)

  open Parser

}

(** Regexpr **)
let newline = ('\010' | '\013' | "\013\010")
let blank   = [' ' '\009' '\012']

rule token = parse
  (** Layout *)
  | newline  { token lexbuf }
  | blank+   { token lexbuf }
  | eof      { EOF }
  | [^' ' '\009' '\012']+ as str { Word str }
  | _ { failwith ("unexpected character: " ^ (Lexing.lexeme lexbuf)) }
