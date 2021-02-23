namespace Library

open System

module Reader =

    let Read (input : string) : Result<Lisp.Sexpr, string> =
        input |> Lexer.Lex |> Parser.Parse |> function
            | Ok sexpr -> Ok sexpr
            | Error err -> Error $"can not read: {err}"
