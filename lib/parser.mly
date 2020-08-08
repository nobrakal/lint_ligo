%{ (* -*- tuareg -*- *)
   open Rules
%}

%token EOF

%token DEPRECIATE IN REPLACEMENT MESSAGE

%token<string> String
%token<int> Int

%start<Rules.rule list> rules

%%

rules: xs=list(rule) EOF { xs }

rule:
| DEPRECIATE name=String IN version=Int replacement=option(replacement) message=option(message)
  { Depreciate {name; version; replacement; message} }

replacement:
| REPLACEMENT replacement=String { replacement }

message:
| MESSAGE message=String { message }
