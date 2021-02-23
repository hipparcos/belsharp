# Bel#

*Bel#* (belsharp) is an implementation of a subset of the
[Bel](http://paulgraham.com/bel.html) programming language in F#.

## Getting started

### Prerequisites
- .NET Core 5.0

### Running a R(E)PL
You can run a RPL from the solution root as follow:
```sh
dotnet run --project REPL/REPL.fsproj
```
The `REPL` make target is available as well.
For now, it is a RPL because inputs are just printed back, unevaluated.

### Running the tests
From the solution root:
```sh
dotnet test
```
The `test` make target is available as well.

## Features
Only basic reader & printer are implemented.