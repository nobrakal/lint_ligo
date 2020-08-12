{ (* -*- tuareg -*- *)

  open Parser

  let pat_buff = Buffer.create 2048

}

(** Regexpr **)
let newline = ('\010' | '\013' | "\013\010")
let blank   = [' ' '\009' '\012']

let digit = ['0'-'9']
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let ident = (lowercase | uppercase | digit | '_')+

rule token = parse
  (** Layout *)
  | newline  { token lexbuf }
  | blank+   { token lexbuf }
  | eof      { EOF }

  | "depreciate" { DEPRECIATE }
  | "replacement" { REPLACEMENT }
  | "message" { MESSAGE }
  | "in" { IN }
  | "pattern" { PATTERN }
  | "%{" { Buffer.clear pat_buff ; pattern lexbuf }

  | digit+ as num { Int (int_of_string num) }
  | ident as id { String id }

  | _ { failwith ("unexpected character: " ^ (Lexing.lexeme lexbuf)) }

and pattern = parse
  | "%}" { FullyEscapedString (Buffer.contents pat_buff) }
  | _ as c { Buffer.add_char pat_buff c; pattern lexbuf }
