module Tests.Lexer

open Expecto
open Library

[<Tests>]
let tests =
    testList "Lexer" [
        testCase "it should accept empty input" <| fun _ ->
            let inp = ""
            let got = Lexer.Lex inp
            let want = [ Lexer.EOF ]
            Expect.sequenceEqual got want
                "empty input should return EOF"

        testCase "it should skip whitespaces" <| fun _ ->
            let inp = "  \t\n\r\v"
            let got = Lexer.Lex inp
            let want = [ Lexer.EOF ]
            Expect.sequenceEqual got want
                "white spaces should return EOF"

        testCase "it should lex numbers" <| fun _ ->
            let inp = "1 10 100"
            let got = Lexer.Lex inp
            let want = [
                Lexer.Number 1;
                Lexer.Number 10;
                Lexer.Number 100;
                Lexer.EOF
            ]
            Expect.sequenceEqual got want
                "it should lex some numbers"

        testCase "it should lex symbols" <| fun _ ->
            let inp = "+ symbol i-am-a-symbol symbol0"
            let got = Lexer.Lex inp
            let want = [
                Lexer.Symbol "+";
                Lexer.Symbol "symbol";
                Lexer.Symbol "i-am-a-symbol";
                Lexer.Symbol "symbol0";
                Lexer.EOF
            ]
            Expect.sequenceEqual got want
                "it should lex some symbols"

        testCase "it should lex parenthesis" <| fun _ ->
            let inp = "())()"
            let got = Lexer.Lex inp
            let want = [
                Lexer.ParenL
                Lexer.ParenR
                Lexer.ParenR
                Lexer.ParenL
                Lexer.ParenR
                Lexer.EOF
            ]
            Expect.sequenceEqual got want
                "it should lex some parenthesis"
    ]
