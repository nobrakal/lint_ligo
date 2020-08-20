%{ (* -*- tuareg -*- *)
   open Rules
%}

%token EOF

(* For rules *)
%token DEPRECIATE IN REPLACEMENT MESSAGE
%token PATTERN

%token<string> String

%token<string> FullyEscapedString

%start<Rules.rule list> rules

(* For unparsed patterns *)
%token<string> Word
%token<string> Var
%token<string * string> TVar
%token MLP MRP

%start<Unparser_cameligo.node Pattern.pattern> unparsed_pattern

%%

rules: xs=list(rule) EOF { xs }

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
        match Unparser_cameligo.node_of_string typ with
        | None -> failwith ("Non-existent type: " ^ typ)
        | Some typ -> Pat_var (id, Some typ )}
| x=Var  { Pat_var (x,None) }
| MLP xs=nonempty_list(pattern) MRP { Pat_pat xs }
