{ (* -*- tuareg -*- *)

  open Lint_parser

  let pat_buff = Buffer.create 2048

  let str_buff = Buffer.create 1024

  (* Transform char into its ascii code *)
  let get_char s =
    let first_char = String.get s 0 in
    let length = String.length s in
    if length = 1 then first_char     (* 'char' *)
    else
      if first_char = '\\' then
        match String.get s 1 with     (* 'escaped' *)
        | 'n' -> Char.chr 10
        | 'r' -> Char.chr 13
        | 'b' -> Char.chr 8
        | 't' -> Char.chr 9
        | '\'' -> Char.chr 39
        | '\\' -> Char.chr 92
        | '0' | '1' | '2' ->
           (String.sub s 1 (length-1)) |> int_of_string |> Char.chr
        | _  -> failwith "unexpected escaped character."
      else failwith "unexpected character."
}

(** Regexpr **)
let newline = ('\010' | '\013' | "\013\010")
let blank   = [' ' '\009' '\012']

let digit = ['0'-'9']
let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let letters = (digit | lowercase | uppercase)
let ident = (lowercase | uppercase | digit | '_')+

(* ASCII *)
let ascii_num = '\\' ['0'-'2']['0'-'9']['0'-'9']
let ascii_print =  ['\x20'-'\x7f'] # ['"' '\'']
let ascii_hex = "\\0x" letters letters
let ascii_esc = "\\\\" | "\\n" | "\\b" | "\\'" | "\\t" | "\\r"
let atom_pur = ascii_esc | ascii_print | ascii_hex | ascii_num
let atom = atom_pur | '"'
let str_token = atom_pur | '\'' | "\\\""

rule token = parse
  (** Layout *)
  | newline  { token lexbuf }
  | blank+   { token lexbuf }
  | eof      { EOF }

  | "deprecate" { DEPRECATE }
  | "replacement" { REPLACEMENT }
  | "message" { MESSAGE }
  | "in" { IN }
  | "pattern" { PATTERN }
  | "language" { LANGUAGE }

  | '"'  { Buffer.clear str_buff ; str lexbuf }
  | "%{" { Buffer.clear pat_buff ; pattern lexbuf }

  | ident as id { String id }

  | _ { failwith ("unexpected character: " ^ (Lexing.lexeme lexbuf)) }

and str = parse
  | "\\\""         { Buffer.add_string str_buff "\"" ; str lexbuf }
  | '"'            { String (Buffer.contents str_buff) }
  | str_token as s { Buffer.add_char str_buff (get_char s) ; str lexbuf }
  | _              { failwith "unexpected character." }

and pattern = parse
  | "%}" { FullyEscapedString (Buffer.contents pat_buff) }
  | _ as c { Buffer.add_char pat_buff c; pattern lexbuf }
