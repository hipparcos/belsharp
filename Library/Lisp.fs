namespace Library

open System

/// List data structures.
module Lisp =

    /// Atom: the leaf of S-expressions.
    type Atom =
        | Nil
        | Number of int
        | Symbol of string

    /// Sexpr: lists, pairs the node of S-expressions and their atoms.
    /// Lists are represented as F# lists instead of cons Pair so
    /// F# list functions can be used.
    type Sexpr =
        | Atom of Atom
        | Pair of Sexpr * Sexpr
        | Sexpr of Sexpr list

    /// AtomToString: for debugging, will be implemented by the printer.
    let AtomToString = function
        | Nil -> "nil"
        | Number n -> string n
        | Symbol s -> s

    // SexprRoString: for debugging, will be implemented by the printer.
    let rec SexprToString sexpr =
        let concat l = String.concat " " (l
                                          |> List.rev
                                          |> List.tail
                                          |> List.rev
                                          |> List.map SexprToString)
        match sexpr with
            | Atom a -> AtomToString a
            | Pair (car, cdr) -> $"( {SexprToString car} . {SexprToString cdr} )"
            | Sexpr l -> $"( {concat l} )"
