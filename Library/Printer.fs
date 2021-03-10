namespace Library

open System

module Printer =

    /// AtomToString: for debugging, will be implemented by the printer.
    let AtomToString = function
        | Lisp.Nil -> "nil"
        | Lisp.Number n -> string n
        | Lisp.Symbol s -> s
        | Lisp.Function f -> $"(lit clo nil {f.parameterList} {f.Body}"
        | Lisp.Macro f -> $"(lit mac nil {f.parameterList} {f.Body}"
        | Lisp.Primitive p -> $"(lit prim {p.Name})"
        | Lisp.SpecialForm f -> $"(lit form {f.Name})"
        | Lisp.Error err -> $"Error: {err}"

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

    let internal generatePrintingOpsForFunction (f : Lisp.Function) typ : PrintingOp list =
        [PrintingChar $"(lit {typ} "] @
        generatePrintingOps (Lisp.environmentToAList f.Scope) @
        [PrintingChar " "] @
        generatePrintingOps f.parameterList @
        [PrintingChar " "] @
        generatePrintingOps f.Body @
        [PrintingChar ")"]

    let SexprToString (sexpr : Lisp.Sexpr) : string =
        let builder = Text.StringBuilder()
        let rec loop (ops : PrintingOp list) : unit =
            if ops.IsEmpty then ()
            else
                let op = ops.Head
                match op with
                    | PrintingAtom (Lisp.Function f) ->
                        loop (List.append (generatePrintingOpsForFunction f "clo")
                                           ops.Tail)
                    | PrintingAtom (Lisp.Macro f) ->
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

    let Print (sexpr : Lisp.Sexpr) : string =
        SexprToString sexpr
