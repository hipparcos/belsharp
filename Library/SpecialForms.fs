namespace Library

open System

/// Special forms are forns that does not evaluate as per function
/// evaluation rules. An example is the `if` form which does not
/// evaluate all of its branches.
module SpecialForms =

    let environmentToAList (env : Lisp.Environment) : Lisp.Value =
        let mutable acc : Lisp.Sexpr list = []
        for KeyValue(symbol, value) in env do
            let symbol = Lisp.Atom (Lisp.Symbol symbol)
            let value = match value with
                            | Lisp.Value.Sexpr sexpr -> sexpr
                            | _ -> symbol
            acc <- (Lisp.Pair (symbol, value))::acc
        Lisp.Value.Sexpr (Lisp.Sexpr acc)

    let globe (context : Lisp.Context) (nargs : int) : unit =
        context.PushData(environmentToAList context.Scope.Global) |> ignore

    let scope (context : Lisp.Context) (nargs : int) : unit =
        context.PushData(environmentToAList context.Scope.Lexical) |> ignore

    let quote (context : Lisp.Context) (nargs : int) : unit =
        ()
