namespace Library

/// Primitives are functions that evaluate as per function evaluation
/// rules (left to right, depth first) but can not be defined in Bel
/// itself.
module Primitives =
    
    open Lisp

    /// add: add numbers.
    let add (args : Sexpr list) : Sexpr =
        args
        |> List.map (fun s -> match s with
                                  | Atom (Number n) -> n
                                  | _ -> 0)
        |> List.sum
        |> Number
        |> Atom

    /// mul: multiply numbers.
    let mul (args : Sexpr list) : Sexpr =
        args
        |> List.map (fun s -> match s with
                                  | Atom (Number n) -> n
                                  | _ -> 1)
        |> List.fold (*) 1
        |> Number
        |> Atom

    /// car: return the car of a list / pair.
    let car (stack : Sexpr list) : Sexpr =
        match stack.Head with
            | Sexpr [] -> Atom Nil
            | Sexpr (it::_) -> it
            | Pair (it, _) -> it
            | _ -> Atom Nil

    /// cdr: return the cdr of a list / pair.
    let cdr (stack : Sexpr list) : Sexpr =
        match stack.Head with
            | Sexpr [] -> Atom Nil
            | Sexpr (_::it) -> Sexpr it
            | Pair (_, it) -> it
            | _ -> Atom Nil
