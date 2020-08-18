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
%token MLP MRP

%start<Unparser_cameligo.node Pattern.pattern> unparsed_pattern

%%

rules: xs=list(rule) EOF { xs }

rule:
| DEPRECIATE dep=String IN dep_version=String dep_replacement=option(replacement) dep_message=option(message)
  { Depreciate {dep; dep_version; dep_replacement; dep_message} }
| PATTERN pat=FullyEscapedString MESSAGE pat_message=String { Pattern {pat;pat_message} }

replacement:
| REPLACEMENT replacement=String { replacement }

message:
| MESSAGE message=String { message }

unparsed_pattern:
| xs=nonempty_list(pattern) EOF { Pat_pat xs }

pattern:
| x=Word { Pat_lex x }
| x=String  { Pat_var (x,None) } (* TODO *)
| MLP xs=nonempty_list(pattern) MRP { Pat_pat xs }
