namespace Library

open System

/// Lexer is a recursive lexer implementation.
/// The public interface is the Lexer.Lex function and the different
/// kinds of tokens.
module Lexer =

    /// Token: the productions of the lexer.
    type Token =
        | EOF
        | Number of int
        | ParenL
        | ParenR
        | Symbol of string

    /// Active Patterns used to match the current character.
    let internal (|Character|Digit|Null|ParenL|ParenR|Whitespace|) (c: char) =
        if c = '\000' then Null
        elif Char.IsDigit(c) then Digit
        elif Char.IsWhiteSpace(c) then Whitespace
        elif c = '(' then ParenL
        elif c = ')' then ParenR
        else Character

    /// firstOrNull: return the first character of STR or the null char.
    let internal firstOrNull (str: string) : char =
        if (String.length str) = 0 then '\000'
        else str.[0]

    /// lexSymbol: lex a symbol from STR (characters + digits).
    let rec internal lexSymbol (str: string) (acc: string) : Token * string =
        match (firstOrNull str) with
            | Character|Digit -> lexSymbol str.[1..] (acc + string str.[0])
            | _ -> (Symbol acc, str)

    /// lexNumber: lex a Number from STR (digits).
    let rec internal lexNumber (str: string) (acc: string) : Token * string =
        match (firstOrNull str) with
            | Digit -> lexNumber str.[1..] (acc + string str.[0])
            | _ -> (Number (acc |> int), str)

    /// lexToken: lex any kind of token from STR.
    /// Return a token and the rest of STR that has not been consumed.
    let rec internal lexToken (str: string) : Token * string =
        if (String.length str) = 0 then (EOF, "")
        else match str.[0] with
                 | Whitespace -> lexToken str.[1..]
                 | Digit -> lexNumber str.[1..] (string str.[0])
                 | ParenL -> (ParenL, str.[1..])
                 | ParenR -> (ParenR, str.[1..])
                 | _ -> lexSymbol str.[1..] (string str.[0])

    /// Lex: lex STR producing a lazy sequence of Tokens.
    /// The returned sequence ends with the EOF token.
    /// At the moment STR is just a string but might be replaced by
    /// by a seq<char>.
    let rec Lex (str: string) : seq<Token> =
        match (lexToken str) with
            | (tok, _) when tok = EOF -> seq { yield EOF }
            | (tok, str) -> seq { yield tok
                                  yield! Lex str }
