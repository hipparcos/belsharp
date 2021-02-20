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
    let internal (|Character|Digit|ParenL|ParenR|Whitespace|) (c: char) =
        if Char.IsDigit(c) then Digit
        elif Char.IsWhiteSpace(c) then Whitespace
        elif c = '(' then ParenL
        elif c = ')' then ParenR
        else Character

    /// maybeCharAt: return the character at OFFSET of STR or None.
    let internal maybeCharAt (str: string) (offset : int) : char option =
        if (String.length str) <= offset then None
        else Some(str.[offset])

    /// lexSymbol: lex a symbol from STR (characters + digits).
    let rec internal lexSymbol (str: string) (offset: int) : Token * string =
        match (maybeCharAt str offset) with
            | Some(Character) | Some(Digit) -> lexSymbol str (offset + 1)
            | _ -> (Symbol str.[0..(offset-1)], str.[offset..])

    /// lexNumber: lex a Number from STR (digits).
    let rec internal lexNumber (str: string) (offset: int) : Token * string =
        match (maybeCharAt str offset) with
            | Some(Digit) -> lexNumber str (offset + 1)
            | _ -> (Number (str.[0..(offset-1)] |> int), str.[offset..])

    /// lexToken: lex any kind of token from STR.
    /// Return a token and the rest of STR that has not been consumed.
    let rec internal lexToken (str: string) : Token * string =
        if (String.length str) = 0 then (EOF, "")
        else match str.[0] with
                 | Whitespace -> lexToken str.[1..]
                 | Digit -> lexNumber str 1
                 | ParenL -> (ParenL, str.[1..])
                 | ParenR -> (ParenR, str.[1..])
                 | _ -> lexSymbol str 1

    /// Lex: lex STR producing a lazy sequence of Tokens.
    /// The returned sequence ends with the EOF token.
    /// At the moment STR is just a string but might be replaced by
    /// by a seq<char>.
    let rec Lex (str: string) : seq<Token> =
        match (lexToken str) with
            | (tok, _) when tok = EOF -> seq { yield EOF }
            | (tok, str) -> seq { yield tok
                                  yield! Lex str }
