namespace Library

module Reader =

    let read (input : string) : Result<Lisp.Sexpr, string> =
        input |> Parser.parse
