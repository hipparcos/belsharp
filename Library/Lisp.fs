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
