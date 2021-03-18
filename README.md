# Bel#

*Bel#* (belsharp) is an implementation of a subset of the
[Bel](http://paulgraham.com/bel.html) programming language in F#.

## Getting started

### Prerequisites
- .NET Core 5.0

### Running a REPL
You can run a REPL from the solution root as follow:
```sh
dotnet run --project REPL/REPL.fsproj
```
The `REPL` make target is available as well.

### Running the tests
From the solution root:
```sh
dotnet test
```
The `test` make target is available as well.

## Features
Only a basic interpreter is implemented.

**Supported special forms:**
* `dyn`: create a dynamic scope, execute the body in it
* `if`: branch conditionally.
* `lit`: prevent evaluation, define closure and macros.
* `quote`: prevent evaluation.
* `set`: bound a value to a symbol.

**Supported primitives:**
* `+`: add numbers.
* `-`: subtract numbers.
* `*`: multiply numbers.
* `car`: return the car of a list (or pair) or nil.
* `cdr`: return the cdr of a list (or pair) or nil.
* `do`: evaluate expression in sequence.
* `id`: reference equality.
* `join`: create a pair.
* `list`: create a list .
