# lint_ligo

A linter for [LIGO](https://ligolang.org/).

## Build

lint_ligo uses [dune](https://github.com/ocaml/dune) as a build system.
To build the executable, just run `dune build bin/main.exe`.

## Usage

### As a standalone executable

The command

```
lint_ligo lint file.rules file.mligo entrypoint
```

will lint `file.mligo` (with entry point `entrypoint`) using the rules in `file.rules`.

Note that currently, it will lint only CameLIGO and PascaLIGO files.

## Rules syntax

A rules file contains a language and a list of rule. The file must follow the following syntax

```
<rules> ::=
  language <lang>
  list(<rule>)

<lang> ::=
  | CameLIGO
  | PascaLIGO
  | ReasonLIGO

<rule> ::=
  | pattern <type> %{ <pattern> %} message "<text>"
  | deprectiate <string> in <string> opt(replacement <string>) opt(message <string>)

<type> ::= expr | type | keyword | (* under development *)

<pattern> ::=
  | %<identifier> (* A named variable *)
  | %<identifier>:<type> (* A named variable with a type *)
  | %_ (* A fresh variable *)
  | %_:<type> (* A fresh typed variable *)
  | %( <pattern> %) (* a pattern in a sub-tree *)
  | <word> (* Any word *)
```
### Depreciation

You can mark a function name as depreciated. It must be followed by a version tag, and may be followed by a suggested replacement and/or a custom message.

### Patterns
Given a pattern `P` of type `T` and a ast `A`, the linter will search a node `N` of `A` of type ̀`T` and a substitution `sigma` of the variables in `P` such that `sigma(P)` is equal to `N`. If it exists, it will display the given message.

The pattern matching algorithm is based on the [unparsed pattern][1]. This induces some unusual characteristics:

* Patterns are _not_ parsed, thus they can correspond to invalid LIGO code, and no warning will be issued. Such patterns will simply not match anything.
* Patterns are not linear, meaning that a variable can appear more than once in a pattern.
* Variables can be typed. A typed variable will only match a node of the given type.
* For now, patterns must be written to target the CamelLIGO syntax.

[1]: Rinderknecht, Christian & Volanschi, Nic. (2010). Theory and practice of unparsed patterns for metacompilation. Science of Computer Programming. 75. 85-105.
