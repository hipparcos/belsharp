namespace Library

open System

/// Primitives are functions that evaluate as per function evaluation
/// rules (left to right, depth first) but can not be defined in Bel
/// itself.
module Primitives =

    /// add: add numbers.
    let add (stack : Lisp.ValueStack) (nargs : int) : Lisp.ValueStack =
        let args, rest = List.splitAt nargs stack
        let sum = args
                  |> List.map (fun s -> match s with
                                            | Lisp.Value.Sexpr (Lisp.Atom (Lisp.Number n)) -> n
                                            | _ -> 0)
                  |> List.sum
        (Lisp.Value.Sexpr (Lisp.Atom (Lisp.Number sum)))::rest
