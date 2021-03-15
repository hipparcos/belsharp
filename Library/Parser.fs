namespace Library

// Parser uses the FParsec parser combinator library to parse the input.
module Parser =

    open FParsec
    open Lisp

    let internal ws = spaces

    let internal lparen = pstring "(" .>> ws

    let internal rparen = pstring ")" .>> ws

    let internal dot = pstring "." .>> ws

    let internal quote = pstring "'" <?> "quote"

    let internal number = pint64 .>> ws |>> (int >> Number)

    let internal nil =
        choice [
            pstringCI "nil"
            lparen >>? rparen
        ] .>> ws <?> "nil" >>. preturn Nil

    let internal isSpecial = isAnyOf "!@#$%^&*_-+=|\\~`[{]};:'<>,.?/"

    let internal symbol =
        let isSymbolFirstChar c = isLetter c || isSpecial c
        let isSymbolChar c = isSymbolFirstChar c || isDigit c
        many1Satisfy2 isSymbolFirstChar isSymbolChar .>> ws |>> (Sym >> Symbol)

    let internal atom =
            number
        <|> nil
        <|> symbol
        <?> "atom"
        |>> Atom

    let internal sexpr, sexprRef = createParserForwardedToRef<Sexpr,unit>()

    let rec internal pair =
        lparen >>? (sexpr .>>? dot .>>. sexpr) .>> rparen <?> "pair"
        |>> Pair

    let rec internal list =
        between lparen rparen (many1 sexpr) <?> "list"
        |>> Sexpr

    let internal quoted =
        quote >>. sexpr
        |>> fun s -> Sexpr [
            Atom (Symbol (Sym "quote"))
            s
        ]

    do sexprRef :=
            quoted
        <|> atom
        <|> pair
        <|> list

    /// Parse: parse stream of TOKENS, return a Sexpr.
    let parse (input : string) : Result<Sexpr, string> =
        let got = run (ws >>. opt sexpr .>> eof) input
        match got with
        | Success (Some result,_,_) -> Result.Ok result
        | Success (None,_,_) -> Result.Ok (Atom Nil)
        | Failure (err,_,_) -> Result.Error err