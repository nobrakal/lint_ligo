# lint_ligo

A linter for the [LIGO](https://ligolang.org/) language.

## Build

The compilation of `lint_ligo` requires for now to have the LIGO library from the `dev` branch (the linter will compile from commit `919c412eb0dd40fcc8366c94ddf9f7b71af10b67`).

The simplest way to achieve this is to install LIGO in an opam switch and use this switch to build `lint_ligo`.

### Installation of LIGO in a fresh switch

In the root of the LIGO directory, you can run:
```
make build
opam install .
```

This will create a fresh switch in the LIGO directory, install LIGO dependencies and
install LIGO itself.

### Build of lint_ligo

Go to the ̀`lint_ligo` directory.
If you installed LIGO using the previous section, now select it by running:
```
opam switch set PATH_OF_LIGO_DIRECTORY
```

`lint_ligo` uses [dune](https://github.com/ocaml/dune) as a build system.

To ensure that you have needed libraries:
```
opam install dune cmdliner ppx_let yojson menhir
```

Then, to build the executable, just run:
```
dune build bin/main.exe
```

You can run it with:
```
dune exec bin/main.exe -- ARGS
```

To install it, run `opam install .`

## Features

The linter is fully configurable and will:

* Detect unused variables.
* Detect the use of deprecated functions (from the LIGO standard library), and allow the user to add their proper deprecations.
* Pattern-match the code against user-defined patterns to detect the use of bad programming patterns.
* In the case of a PascaLIGO file, detect the dialect and show a warning if it uses mixed dialects.

### Example

With the following configuration:

```
language cameligo
pattern expr %{ if %_ then %x else %x %} message "Useless test."
```

and the cameligo file `test.mligo`:

```ocaml
let f (b:bool) (x:int) (y:int)  =
  if b then x else x

let main (action, store : int * int) =
 ([] : operation list), f false action store
```

the linter will print:

```
Linter error in file "test.mligo", line 1, characters 23-30:
Unused variable y.
Linter error in file "test.mligo", line 2, characters 2-20:
Useless test.
```

See more examples in the `examples` folder.

## Usage

The command:

```
lint_ligo -s syntax -r file.rules file.ligo entrypoint
```

will lint `file.ligo` (written in `syntax` with entry point `entrypoint`) using the rules in `file.rules`.

* If no syntax is specified, the syntax will be inferred.
* If no rules are specified, only deprecated constants and unused variables will be detected.

## Rules syntax

A rules file contains a language and a list of rules. The file must follow the following syntax:

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
  | deprecate <string> in <string> opt(replacement <string>) opt(message <string>)

<type> ::= expr | type | keyword | (* under development *)

<pattern> ::=
  | %<identifier>        (* A named variable *)
  | %<identifier>:<type> (* A named variable with a type *)
  | %_                   (* A fresh variable *)
  | %_:<type>            (* A fresh typed variable *)
  | %( <pattern> %)      (* a pattern in a sub-tree *)
  | <word>               (* Any word *)
```
### Deprecation

You can mark a function name as deprecated. It must be followed by a version tag and may be followed by a suggested replacement and/or a custom message.
The engine will search for a free variable with the given name, and print a message if found.

### Patterns

Patterns are a way to capture the shape of a piece of code. They are composed of:

* Pattern variables (identifiers preceded by `%`). Note that a "hole" variable representing an always fresh variable is available using `%_`.
To have better control over variables, they can be typed to match only a kind of node in the AST.
* Meta parentheses `%(` `%)` (parentheses not included in the targeted code but useful to indicate the shape of the AST).
* Any words.

More precisely, given a pattern `P` of type `T` and a ast `A`, the linter will search a node `N` of `A` of type  `T` and a substitution `sigma` of the variables in `P` such that `sigma(P)` is equal to `N`. If it exists, it will display the given message.

The pattern matching algorithm is based on the [unparsed pattern][1]. This induces some unusual characteristics:

* Patterns are _not_ parsed, thus they can correspond to invalid LIGO code, and no warning will be issued. Such patterns will simply not match anything.
* Patterns are not linear, meaning that a variable can appear more than once in a pattern.
* Variables can be typed. A typed variable will only match a node of the given type.

[1]: Rinderknecht, Christian & Volanschi, Nic. (2010). Theory and practice of unparsed patterns for metacompilation. Science of Computer Programming. 75. 85-105.
