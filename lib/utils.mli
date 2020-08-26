val filter_some : 'a option list -> 'a list

val sequence_result : ('a, 'b) result list -> ('a list, 'b) result

val list_map_to_opt : ('a list -> 'b) -> 'a list -> 'b option
