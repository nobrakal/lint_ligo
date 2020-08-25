open Simple_utils.Region

let print_int lex x =
  let (_,value) = x.value in
  lex (Z.to_string value) x.region

let print_nat lex x =
  let (_,value) = x.value in
  lex (Z.to_string value ^ "n") x.region

let print_mutez lex x =
  let (_,value) = x.value in
  lex (Z.to_string value ^ "mutez") x.region

let print_bytes lex e =
  lex ("0x" ^ Hex.show (snd e.value)) e.region
