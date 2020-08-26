%{ (* -*- tuareg -*- *)
   open Rules

   let new_var =
     let counter = ref 0 in
     fun () -> let res = "_" ^ string_of_int !counter in
               counter := !counter + 1; res
%}

%token EOF

(* For rules *)
%token DEPRECIATE IN REPLACEMENT MESSAGE
%token PATTERN

%token LANGUAGE

%token<string> String

%token<string> FullyEscapedString

%start<Rules.parsed_rules> rules

(* For unparsed patterns *)
%token<string> Word
%token<string * string option> TVar
%token MLP MRP
%token<string option> TWild
%start<string Pattern.pattern> unparsed_pattern

%%

rules:
| LANGUAGE plang=String prules=list(rule) EOF { {plang;prules} }

rule:
| DEPRECIATE dep=String IN dep_version=String dep_replacement=option(replacement) dep_message=option(message)
  { Depreciate {dep; dep_version; dep_replacement; dep_message} }
| PATTERN pat_type=String pat=FullyEscapedString MESSAGE pat_message=String
  { Pattern {pat;pat_type;pat_message} }

replacement:
| REPLACEMENT replacement=String { replacement }

message:
| MESSAGE message=String { message }

unparsed_pattern:
| xs=nonempty_list(pattern) EOF { Pat_pat xs }

pattern:
| x=Word { Pat_lex x }
| x=TVar { let id,typ = x in
           Pat_var (id, typ)}
| typ=TWild { Pat_var (new_var (), typ)}
| MLP xs=nonempty_list(pattern) MRP { Pat_pat xs }
