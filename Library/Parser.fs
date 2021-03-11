namespace Library

// Parser uses the FParsec parser combinator library to parse the input.
module Parser =

    open Lisp

    /// Parse: parse stream of TOKENS, return a Sexpr.
    let parse (input : string) : Result<Sexpr, string> =
        Result.Error "not implemented"