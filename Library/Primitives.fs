namespace Library

/// Primitives are functions that evaluate as per function evaluation
/// rules (left to right, depth first) but can not be defined in Bel
/// itself.
module Primitives =

    /// add: add numbers.
    let add (args : Lisp.Sexpr list) : Lisp.Sexpr =
        args
        |> List.map (fun s -> match s with
                                  | Lisp.Atom (Lisp.Number n) -> n
                                  | _ -> 0)
        |> List.sum
        |> Lisp.Number
        |> Lisp.Atom

    /// mul: multiply numbers.
    let mul (args : Lisp.Sexpr list) : Lisp.Sexpr =
        args
        |> List.map (fun s -> match s with
                                  | Lisp.Atom (Lisp.Number n) -> n
                                  | _ -> 1)
        |> List.fold (*) 1
        |> Lisp.Number
        |> Lisp.Atom

    /// car: return the car of a list / pair.
    let car (stack : Lisp.Sexpr list) : Lisp.Sexpr =
        match stack.Head with
            | Lisp.Sexpr [] -> Lisp.Atom Lisp.Nil
            | Lisp.Sexpr (it::_) -> it
            | Lisp.Pair (it, _) -> it
            | _ -> Lisp.Atom Lisp.Nil

    /// cdr: return the cdr of a list / pair.
    let cdr (stack : Lisp.Sexpr list) : Lisp.Sexpr =
        match stack.Head with
            | Lisp.Sexpr [] -> Lisp.Atom Lisp.Nil
            | Lisp.Sexpr (_::it) -> Lisp.Sexpr it
            | Lisp.Pair (_, it) -> it
            | _ -> Lisp.Atom Lisp.Nil
