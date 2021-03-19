namespace Library

/// Primitives are functions that evaluate as per function evaluation
/// rules (left to right, depth first) but can not be defined in Bel
/// itself.
module Primitives =
    
    open Lisp

    let eq (args : Sexpr list) : Sexpr =
        if args.IsEmpty then
            nil
        else
            if List.forall ((=) args.Head) args.Tail then
                t
            else
                nil

    let gt (args : Sexpr list) : Sexpr =
        let rec loop lst =
            match lst with
            | [ ] -> nil
            | [_] -> t
            | first::second::rest ->
                if first > second then
                    loop (second::rest)
                else
                    nil
        loop args

    let lt (args : Sexpr list) : Sexpr =
        let rec loop lst =
            match lst with
            | [ ] -> nil
            | [_] -> t
            | first::second::rest ->
                if first < second then
                    loop (second::rest)
                else
                    nil
        loop args

    /// add: add numbers.
    let add (args : Sexpr list) : Sexpr =
        args
        |> List.map (fun s -> match s with
                                  | Atom (Number n) -> n
                                  | _ -> 0)
        |> List.sum
        |> Number
        |> Atom

    let sub (args : Sexpr list) : Sexpr =
        match args with
        | [] -> Atom (Number 0)
        | [Atom (Number it)] -> Atom (Number -it)
        | _ ->
            args
            |> List.map (fun s -> match s with
                                  | Atom (Number n) -> n
                                  | _ -> 0)
            |> fun lst -> List.fold (-) lst.Head lst.Tail
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
            | Sexpr [] | Sexpr [_] -> Atom Nil
            | Sexpr (_::it) -> Sexpr it
            | Pair (_, it) -> it
            | _ -> Atom Nil

    let id (args : Sexpr list) : Sexpr =
        match args with
        | [Atom x; Atom y] ->
            if x = y then
                t
            else
                nil
        | [Sexpr x; Sexpr y] ->
            if LanguagePrimitives.PhysicalEquality x y then
                t
            else
                nil
        | [Pair _ as x; Pair _ as y] ->
            if LanguagePrimitives.PhysicalEquality x y then
                t
            else
                nil
        | _ -> Atom Nil

    let list (stack : Sexpr list) : Sexpr =
        Sexpr stack

    let join (stack : Sexpr list) : Sexpr =
        match stack with
        | [car; Atom Nil] -> list [car]
        | [car; Sexpr cdr] -> Sexpr (car::cdr)
        | [car; cdr] -> Pair (car, cdr)
        | _ -> Atom Nil

    /// Print all arguments separated by spaces, return the last one.
    /// Return nil when no arguments passed.
    let prn (args : Sexpr list) : Sexpr =
        if args.IsEmpty then
            Atom Nil
        else
            let output = Printer.print >> printf "%s "
            List.fold (fun _ it -> output it; it) nil args
            |> fun last -> printfn ""; last

    let progn (stack : Sexpr list) : Sexpr =
        match List.tryLast stack with
        | Some v -> v
        | None -> Atom Nil

    let defPrim n p =
        { Primitive.Name = n
          Primitive.Func = p }

    let primitives : Map<Symbol, Primitive> =
        let prims = [
            defPrim "=" eq
            defPrim "<" lt
            defPrim ">" gt
            defPrim "+" add
            defPrim "-" sub
            defPrim "*" mul
            defPrim "car" car
            defPrim "cdr" cdr
            defPrim "do" progn
            defPrim "id" id
            defPrim "join" join
            defPrim "list" list
            defPrim "prn" prn
        ]
        List.fold (fun prims (p:Primitive) -> prims.Add(Sym p.Name, p)) Map.empty prims
