%{ (* -*- tuareg -*- *)
   open Rules
%}

%token EOF

(* For rules *)
%token DEPRECIATE IN REPLACEMENT MESSAGE
%token PATTERN

%token<string> String

%token<string> FullyEscapedString
%token<int> Int

%start<Rules.rule list> rules

(* For unparsed patterns *)
%token<string> Word
%token MLP MRP

%start<string list> words

%%

rules: xs=list(rule) EOF { xs }

rule:
| DEPRECIATE dep=String IN dep_version=Int dep_replacement=option(replacement) dep_message=option(message)
  { Depreciate {dep; dep_version; dep_replacement; dep_message} }
| PATTERN pat=FullyEscapedString MESSAGE pat_message=String { Pattern {pat;pat_message} }

replacement:
| REPLACEMENT replacement=String { replacement }

message:
| MESSAGE message=String { message }

words: xs=list(Word) EOF { xs }
