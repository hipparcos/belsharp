namespace Library

open System

module Printer =

    /// AtomToString: for debugging, will be implemented by the printer.
    let AtomToString = function
        | Lisp.Nil -> "nil"
        | Lisp.Number n -> string n
        | Lisp.Symbol s -> s

    type internal PrintingOp =
        | PrintingAtom of Lisp.Atom
        | PrintingChar of string
        | PrintingList of Lisp.Sexpr list
        | PrintingSexpr of Lisp.Sexpr

    let internal generatePrintingOps (sexpr : Lisp.Sexpr) : PrintingOp list =
        match sexpr with
            | Lisp.Atom a ->
                [PrintingAtom a]
            | Lisp.Pair (car, cdr) ->
                [PrintingChar "(";
                 PrintingSexpr car;
                 PrintingChar " . ";
                 PrintingSexpr cdr;
                 PrintingChar ")"]
            | Lisp.Sexpr [] ->
                [PrintingAtom Lisp.Nil]
            | Lisp.Sexpr list ->
                [PrintingChar "(";
                 PrintingList list;
                 PrintingChar ")"]

    let SexprToString (sexpr : Lisp.Sexpr) : string =
        let builder = Text.StringBuilder()
        let rec loop (ops : PrintingOp list) : unit =
            if ops.IsEmpty then ()
            else
                let op = ops.Head
                match op with
                    | PrintingAtom a ->
                        builder.Append(AtomToString a) |> ignore
                        loop ops.Tail
                    | PrintingChar s ->
                        builder.Append(s) |> ignore
                        loop ops.Tail
                    | PrintingList [] ->
                        loop ops.Tail
                    | PrintingList [car] ->
                        loop (List.append (generatePrintingOps car)
                                          (ops.Tail))
                    | PrintingList (car::cdr) ->
                        loop (List.append (generatePrintingOps car)
                                          ((PrintingChar " ")::(PrintingList cdr)::ops.Tail))
                    | PrintingSexpr sexpr ->
                        loop (List.append (generatePrintingOps sexpr)
                                          (ops.Tail))
        loop (generatePrintingOps sexpr)
        builder.ToString()

    let Print (sexpr : Lisp.Sexpr) : string =
        SexprToString sexpr
