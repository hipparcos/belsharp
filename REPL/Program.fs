open System

open Library

let banner : string = "BEL# REPL (press Ctrl+C to exit)"
let prompt : string = "> "

/// read: generate an infinite sequence of prompt.
let read (prompt : string) parser =
    Seq.initInfinite (fun _ -> printf "%s" prompt ; Console.ReadLine())
    |> Seq.choose (parser >> function true, v -> Some v | _ -> None)

/// tryReadSexpr: a Sexpr parser that uses the Reader from Library.
let tryReadSexpr str : bool * Lisp.Sexpr option =
    match (Reader.read str) with
        | Ok sexpr -> true, Some sexpr
        | Error err -> printfn "%s" err ; false, None

[<EntryPoint>]
let main argv =
    // Exit with code 0 on Ctrl+C.
    Console.CancelKeyPress.Add(
        fun arg -> printfn "\nexit" ; Environment.Exit 0
    )
    // REPL 'loop'
    printfn "%s" banner
    let mutable globe = Evaluator.defaultGlobe
    for sexpr in (read prompt tryReadSexpr) do
        match sexpr with
            | Some sexpr ->
                let scope, result = Evaluator.eval Evaluator.defaultGlobe sexpr
                Printer.print result |> printfn "%s"
                globe <- scope
            | None -> ()
    0
