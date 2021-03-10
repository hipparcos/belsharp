namespace Library

open System

/// Special forms are forns that does not evaluate as per function
/// evaluation rules. An example is the `if` form which does not
/// evaluate all of its branches.
module SpecialForms =

    let globe (scope : Lisp.Scope) (args : Lisp.DataStack) : Lisp.SpecialFormResult =
        scope, [], [Lisp.environmentToAList scope.Global]

    let scope (scope : Lisp.Scope) (args : Lisp.DataStack) : Lisp.SpecialFormResult =
        scope, [], [Lisp.environmentToAList scope.Lexical]

    let quote (scope : Lisp.Scope) (args : Lisp.DataStack) : Lisp.SpecialFormResult =
        scope, [], args

    let internal clo = Lisp.Atom (Lisp.Symbol "clo")

    let lit (scope : Lisp.Scope) (args : Lisp.DataStack) : Lisp.SpecialFormResult =
        match args with
            | clo::env::(Lisp.Sexpr parameters)::[body] ->
                let fScope = match env with
                             | Lisp.Sexpr env -> Lisp.alistToEnvironment env
                             | _ -> Map.empty
                let parameters = parameters
                                 |> List.filter (fun it -> match it with
                                                           | Lisp.Atom (Lisp.Symbol _) -> true
                                                           | _ -> false)
                                 |> List.map (fun it -> match it with
                                                        | Lisp.Atom (Lisp.Symbol s) -> s
                                                        | _ -> "")
                scope, [], [
                    Lisp.Atom (Lisp.Function {
                        Scope = fScope
                        Parameters = parameters
                        Body = body
                    })
                ]
            | _ ->
                let l = Lisp.Symbol "lit" |> Lisp.Atom
                scope, [], [Lisp.Sexpr (l::args)]