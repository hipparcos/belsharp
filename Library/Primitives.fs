namespace Library

open System

/// Primitives are functions that evaluate as per function evaluation
/// rules (left to right, depth first) but can not be defined in Bel
/// itself.
module Primitives =

    /// add: add numbers.
    let add (args : Lisp.Value list) (nargs : int) : Lisp.Value =
        args
        |> List.map (fun s -> match s with
                                  | Lisp.Value.Sexpr (Lisp.Atom (Lisp.Number n)) -> n
                                  | _ -> 0)
        |> List.sum
        |> Lisp.Number
        |> Lisp.Atom
        |> Lisp.Value.Sexpr

    /// mul: multiply numbers.
    let mul (args : Lisp.Value list) (nargs : int) : Lisp.Value =
        args
        |> List.map (fun s -> match s with
                                  | Lisp.Value.Sexpr (Lisp.Atom (Lisp.Number n)) -> n
                                  | _ -> 1)
        |> List.fold (fun acc it -> acc * it) 1
        |> Lisp.Number
        |> Lisp.Atom
        |> Lisp.Value.Sexpr

    /// car: return the car of a list / pair.
    let car (stack : Lisp.Value list) (nargs : int) : Lisp.Value =
        match stack.Head with
            | Lisp.Value.Sexpr (Lisp.Sexpr s) ->
                let it = match s with
                             | [] -> Lisp.Atom Lisp.Nil
                             | h::_ -> h
                Lisp.Value.Sexpr it
            | Lisp.Value.Sexpr (Lisp.Pair (it, _)) ->
                Lisp.Value.Sexpr it
            | _ ->
                Lisp.Value.Sexpr (Lisp.Atom Lisp.Nil)

    /// cdr: return the cdr of a list / pair.
    let cdr (stack : Lisp.Value list) (nargs : int) : Lisp.Value =
        match stack.Head with
            | Lisp.Value.Sexpr (Lisp.Sexpr s) ->
                let it = match s with
                             | [] -> Lisp.Atom Lisp.Nil
                             | _::rest -> Lisp.Sexpr rest
                Lisp.Value.Sexpr it
            | Lisp.Value.Sexpr (Lisp.Pair (_, it)) ->
                Lisp.Value.Sexpr it
            | _ ->
                Lisp.Value.Sexpr (Lisp.Atom Lisp.Nil)
