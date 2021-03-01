namespace Library

open System

/// Special forms are forns that does not evaluate as per function
/// evaluation rules. An example is the `if` form which does not
/// evaluate all of its branches.
module SpecialForms =

    let environmentToAList (env : Lisp.Environment) : Lisp.Sexpr =
        Map.fold (fun acc s v ->
                      (Lisp.Pair (Lisp.Symbol s |> Lisp.Atom, v))::acc)
                 [] env
        |> Lisp.Sexpr

    let globe (context : Lisp.Context) (nargs : int) : unit =
        context.PushData(environmentToAList context.Scope.Global) |> ignore

    let scope (context : Lisp.Context) (nargs : int) : unit =
        context.PushData(environmentToAList context.Scope.Lexical) |> ignore

    let quote (context : Lisp.Context) (nargs : int) : unit =
        ()
