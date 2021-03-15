namespace Library

open System

module Printer =
    
    open Lisp

    /// AtomToString: for debugging, will be implemented by the printer.
    let AtomToString = function
        | Nil -> "nil"
        | Number n -> string n
        | Symbol (Sym s) -> s
        | Function f -> $"(lit clo nil {f.parameterList} {f.Body}"
        | Macro f -> $"(lit mac nil {f.parameterList} {f.Body}"
        | Primitive p -> $"(lit prim {p.Name})"
        | SpecialForm f -> $"(lit form {f.Name})"
        | Error err -> $"Error: {err}"

    type internal PrintingOp =
        | PrintingAtom of Atom
        | PrintingChar of string
        | PrintingList of Sexpr list
        | PrintingSexpr of Sexpr

    let internal generatePrintingOps (sexpr : Sexpr) : PrintingOp list =
        match sexpr with
            | Atom a ->
                [PrintingAtom a]
            | Pair (car, cdr) ->
                [PrintingChar "(";
                 PrintingSexpr car;
                 PrintingChar " . ";
                 PrintingSexpr cdr;
                 PrintingChar ")"]
            | Sexpr [] ->
                [PrintingAtom Nil]
            | Sexpr list ->
                [PrintingChar "(";
                 PrintingList list;
                 PrintingChar ")"]

    let internal generatePrintingOpsForFunction (f : Function) typ : PrintingOp list =
        [PrintingChar $"(lit {typ} "] @
        generatePrintingOps (scopeToAlist !f.Environment) @
        [PrintingChar " "] @
        generatePrintingOps f.parameterList @
        [PrintingChar " "] @
        generatePrintingOps f.Body @
        [PrintingChar ")"]

    let SexprToString (sexpr : Sexpr) : string =
        let builder = Text.StringBuilder()
        let rec loop (ops : PrintingOp list) : unit =
            if ops.IsEmpty then ()
            else
                let op = ops.Head
                match op with
                    | PrintingAtom (Function f) ->
                        loop (List.append (generatePrintingOpsForFunction f "clo")
                                           ops.Tail)
                    | PrintingAtom (Macro f) ->
                        loop (List.append (generatePrintingOpsForFunction f "mac")
                                           ops.Tail)
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

    let print (sexpr : Sexpr) : string =
        SexprToString sexpr
