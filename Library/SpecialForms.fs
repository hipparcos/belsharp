namespace Library

open System

/// Special forms are forns that does not evaluate as per function
/// evaluation rules. An example is the `if` form which does not
/// evaluate all of its branches.
module SpecialForms =

    let environmentToAList (env : Lisp.Environment) : Lisp.Sexpr =
        let mutable acc : Lisp.Sexpr list = []
        for KeyValue(symbol, value) in env do
            let symbol = Lisp.Atom (Lisp.Symbol symbol)
            acc <- (Lisp.Pair (symbol, value))::acc
        Lisp.Sexpr acc

    let globe (context : Lisp.Context) (nargs : int) : unit =
        context.PushData(environmentToAList context.Scope.Global) |> ignore

    let scope (context : Lisp.Context) (nargs : int) : unit =
        context.PushData(environmentToAList context.Scope.Lexical) |> ignore

    let quote (context : Lisp.Context) (nargs : int) : unit =
        ()
