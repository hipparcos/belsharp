namespace Library

open System

module Reader =

    let read (input : string) : Result<Lisp.Sexpr, string> =
        input |> Parser.parse
