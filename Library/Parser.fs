namespace Library

open System

module Parser =

    type Atom =
        | Nil
        | Number of int
        | Symbol of string

    let AtomToString = function
        | Nil -> "nil"
        | Number n -> string n
        | Symbol s -> s

    type Sexpr =
        | Atom of Atom
        | Pair of Sexpr * Sexpr

    let SexprToString sexpr =
        let rec loop = function
            | Atom a -> AtomToString a
            | Pair (l, Atom r) -> (loop l) + " . " + (AtomToString r)
            | Pair (l, r) -> (loop l) + " " + (loop r)
        match sexpr with
            | Atom a -> AtomToString a
            | _ -> "(" + (loop sexpr) + ")"

    type State =
        | EndOfParsing
        | Parsing of Parser

    and Parser = Lexer.Token -> Sexpr list -> Result<Sexpr, string> option * State

    type ParsingResult = Result<Sexpr, string>

    let ResultToString = function
        | Ok sexpr -> SexprToString sexpr
        | Error err -> err

    let internal ParseAtom = function
        | Lexer.Number n -> Ok (Number n)
        | Lexer.Symbol s when s = "nil" -> Ok Nil
        | Lexer.Symbol s -> Ok (Symbol s)
        | _ -> Error "not an atom"

    let internal ParseClosePair tok acc =
        match tok with
            | Lexer.EOF -> (Some(Error "unclosed pair"), EndOfParsing)
            | Lexer.ParenL -> (Some(Error "nested list not supported"), EndOfParsing)
            | Lexer.ParenR ->
                match acc with
                    | [f; s] -> (Some(Ok (Pair (s, f))), EndOfParsing)
                    | _ -> (Some(Error "malformed pair"), EndOfParsing)
            | _ -> (Some(Error "trailing object in pair"), EndOfParsing)

    let internal ParsePair tok acc =
        match tok with
            | Lexer.EOF -> (Some(Error "unclosed pair"), EndOfParsing)
            | Lexer.ParenR -> (Some(Error "malformed pair"), EndOfParsing)
            | Lexer.ParenL -> (Some(Error "nested list not supported"), EndOfParsing)
            | Lexer.Number _ | Lexer.Symbol _ ->
                let atom = ParseAtom tok
                match atom with
                    | Ok(atom) -> (Some(Ok (Atom atom)), Parsing ParseClosePair)
                    | Error(err) -> (Some(Error err), EndOfParsing)

    let rec internal ParseList tok acc =
        match tok with
            | Lexer.EOF -> (Some(Error("unclosed pair")), EndOfParsing)
            | Lexer.ParenR ->
                match acc with
                    | [] -> (Some(Ok(Atom(Nil))), EndOfParsing)
                    | _ -> let result = List.fold (fun a e -> Pair (e, a))
                                                  (Pair ((List.head acc), Atom Nil))
                                                  (List.tail acc)
                           (Some(Ok(result)), EndOfParsing)
            | Lexer.ParenL -> (Some(Error("nested list not supported")), EndOfParsing)
            | Lexer.Symbol s when s = "."  ->
                match acc with
                    | [ _ ] -> (None, Parsing ParsePair)
                    | _ -> (Some(Ok(Atom (Symbol "."))), Parsing ParseList)
            | Lexer.Number _ | Lexer.Symbol _ ->
                let atom = ParseAtom tok
                match atom with
                    | Ok(atom) -> (Some(Ok(Atom atom)), Parsing ParseList)
                    | Error(err) -> (Some(Error err), EndOfParsing)

    let internal ParseTopLevel tok acc =
        match tok with
            | Lexer.EOF -> (Some(Ok(Atom Nil)), EndOfParsing)
            | Lexer.ParenR -> (Some(Error "unexpected closing parenthesis"), EndOfParsing)
            | Lexer.ParenL -> (None, Parsing(ParseList))
            | Lexer.Number _ | Lexer.Symbol _ ->
                let atom = ParseAtom tok
                match atom with
                    | Ok(atom) -> (Some(Ok (Atom atom)), EndOfParsing)
                    | Error(err) -> (Some(Error err), EndOfParsing)

    let Parse (tokens : seq<Lexer.Token>) : ParsingResult =
        let rec loop tokens (state : State) (acc : Sexpr list) =
            match state with
                | EndOfParsing -> Ok(List.head acc)
                | Parsing parser ->
                    if Seq.isEmpty tokens then Error "unexcepted end of stream"
                    else
                        let tok = Seq.head tokens
                        let result = parser tok acc
                        match result with
                            | (Some(Error err), _) -> Error err
                            | (None, state) -> loop (Seq.tail tokens) state acc
                            | (Some(Ok sexpr), state) ->
                                loop (Seq.tail tokens) state (sexpr::acc)
        if Seq.isEmpty tokens then Error "empty stream of tokens"
        else loop tokens (Parsing ParseTopLevel) []
