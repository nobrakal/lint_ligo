%{ (* -*- tuareg -*- *)
   open Rules
%}

%token EOF

%token DEPRECATED

%token<string> String

%start<Rules.rule list> rules

%%

rules: xs=list(rule) EOF {xs}

rule:
| DEPRECATED name=String { Deprecated {name; replacement=None; message=None}}
