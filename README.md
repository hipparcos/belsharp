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
* `quote`: prevent evaluation.

**Supported primitives:**
* `+`: add numbers.

