open Simple_utils

val print_int : (string -> Region.t -> 'a) -> ('b * Z.t) Region.reg -> 'a
val print_nat : (string -> Region.t -> 'a) -> ('b * Z.t) Region.reg -> 'a
val print_mutez : (string -> Region.t -> 'a) -> ('b * Z.t) Region.reg -> 'a
val print_bytes : (string -> Region.t -> 'a) -> ('b * Hex.t) Region.reg -> 'a
