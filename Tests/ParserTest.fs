module Tests.Parser

open Expecto
open Expecto.Flip
open Library

[<Tests>]
let tests =
    testList "Parser" [
        testCase "it should return nil" <| fun _ ->
            let inp = seq {
                Lexer.EOF
            }
            let got = Parser.Parse (inp)
            let want = Lisp.Atom Lisp.Nil
            Expect.wantOk "ok" got |> Expect.equal "equality" want

        testCase "it should parse number" <| fun _ ->
            let inp = seq {
                Lexer.Number 100
                Lexer.EOF
            }
            let got = Parser.Parse (inp)
            let want = Lisp.Atom (Lisp.Number 100)
            Expect.wantOk "ok" got |> Expect.equal "equality" want

        testCase "it should parse symbol" <| fun _ ->
            let inp = seq {
                Lexer.Symbol "symbol"
                Lexer.EOF
            }
            let got = Parser.Parse (inp)
            let want = Lisp.Atom (Lisp.Symbol "symbol")
            Expect.wantOk "ok" got |> Expect.equal "equality" want

        testCase "it should parse nil symbol as nil" <| fun _ ->
            let inp = seq {
                Lexer.Symbol "nil"
                Lexer.EOF
            }
            let got = Parser.Parse (inp)
            let want = Lisp.Atom Lisp.Nil
            Expect.wantOk "ok" got |> Expect.equal "equality" want

        testCase "it should parse empty list as nil" <| fun _ ->
            let inp = seq {
                Lexer.ParenL
                Lexer.ParenR
                Lexer.EOF
            }
            let got = Parser.Parse (inp)
            let want = Lisp.Atom Lisp.Nil
            Expect.wantOk "ok" got |> Expect.equal "equality" want

        testCase "it should parse pair" <| fun _ ->
            let inp = seq {
                Lexer.ParenL
                Lexer.Symbol "symbol1"
                Lexer.Symbol "."
                Lexer.Symbol "symbol2"
                Lexer.ParenR
                Lexer.EOF
            }
            let got = Parser.Parse (inp)
            let want = Lisp.Pair ((Lisp.Atom (Lisp.Symbol "symbol1")),
                                  (Lisp.Atom (Lisp.Symbol "symbol2")))
            Expect.wantOk "ok" got |> Expect.equal "equality" want

        testCase "it should parse pair of dots" <| fun _ ->
            let inp = seq {
                Lexer.ParenL
                Lexer.Symbol "."
                Lexer.Symbol "."
                Lexer.Symbol "."
                Lexer.ParenR
                Lexer.EOF
            }
            let got = Parser.Parse (inp)
            let want = Lisp.Pair ((Lisp.Atom (Lisp.Symbol ".")),
                                  (Lisp.Atom (Lisp.Symbol ".")))
            Expect.wantOk "ok" got |> Expect.equal "equality" want

        testCase "it should parse cdr nested pairs" <| fun _ ->
            let inp = seq {
                Lexer.ParenL        // ( sym1 . ( sym2 . ( sym3 ) ) )
                Lexer.Symbol "sym1"
                Lexer.Symbol "."
                Lexer.ParenL        // ( sym2 . ( sym3 ) )
                Lexer.Symbol "sym2"
                Lexer.Symbol "."
                Lexer.ParenL        // ( sym3 )
                Lexer.Symbol "sym3"
                Lexer.ParenR
                Lexer.ParenR
                Lexer.ParenR
                Lexer.EOF
            }
            let got = Parser.Parse (inp)
            let want = Lisp.Pair ((Lisp.Atom (Lisp.Symbol "sym1")),
                                  (Lisp.Pair ((Lisp.Atom (Lisp.Symbol "sym2")),
                                              (Lisp.Sexpr [Lisp.Atom (Lisp.Symbol "sym3")]))))
            Expect.wantOk "ok" got |> Expect.equal "equality" want

        testCase "it should parse list" <| fun _ ->
            let inp = seq {
                Lexer.ParenL
                Lexer.Symbol "sym1"
                Lexer.Symbol "sym2"
                Lexer.Symbol "sym3"
                Lexer.ParenR
                Lexer.EOF
            }
            let got = Parser.Parse (inp)
            let want = Lisp.Sexpr [Lisp.Atom (Lisp.Symbol "sym1");
                                   Lisp.Atom (Lisp.Symbol "sym2");
                                   Lisp.Atom (Lisp.Symbol "sym3")]
            Expect.wantOk "ok" got |> Expect.equal "equality" want

        testCase "it should parse car nested list" <| fun _ ->
            let inp = seq {
                Lexer.ParenL        // ( ( ( sym1 ) sym2 ) sym3 )
                Lexer.ParenL        // ( ( sym1 ) sym2 )
                Lexer.ParenL        // ( sym1 )
                Lexer.Symbol "sym1"
                Lexer.ParenR
                Lexer.Symbol "sym2"
                Lexer.ParenR
                Lexer.Symbol "sym3"
                Lexer.ParenR
                Lexer.EOF
            }
            let got = Parser.Parse (inp)
            let want = Lisp.Sexpr [Lisp.Sexpr [Lisp.Sexpr [Lisp.Atom (Lisp.Symbol "sym1")];
                                               Lisp.Atom (Lisp.Symbol "sym2")];
                                   Lisp.Atom (Lisp.Symbol "sym3")]
            Expect.wantOk "ok" got |> Expect.equal "equality" want

        testCase "it should parse cadr nested list" <| fun _ ->
            let inp = seq {
                Lexer.ParenL        // ( sym1 ( sym2 ( sym3 ) ) )
                Lexer.Symbol "sym1"
                Lexer.ParenL        // ( sym2 ( sym3 ) )
                Lexer.Symbol "sym2"
                Lexer.ParenL        // ( sym3 )
                Lexer.Symbol "sym3"
                Lexer.ParenR
                Lexer.ParenR
                Lexer.ParenR
                Lexer.EOF
            }
            let got = Parser.Parse (inp)
            let want = Lisp.Sexpr [Lisp.Atom (Lisp.Symbol "sym1");
                            Lisp.Sexpr [Lisp.Atom (Lisp.Symbol "sym2");
                                        Lisp.Sexpr [Lisp.Atom (Lisp.Symbol "sym3")]]]
            Expect.wantOk "ok" got |> Expect.equal "equality" want

        testCase "it should throw unexpected closing parenthesis" <| fun _ ->
            let inp = seq {
                Lexer.ParenR
                Lexer.EOF
            }
            let got = Parser.Parse (inp)
            let want = "unexpected closing parenthesis"
            Expect.wantError "error" got |> Expect.equal "equality" want

        testCase "it should throw unclosed list error" <| fun _ ->
            let inp = seq {
                Lexer.ParenL
                Lexer.Symbol "symbol"
                Lexer.EOF
            }
            let got = Parser.Parse (inp)
            let want = "unclosed pair"
            Expect.wantError "error" got |> Expect.equal "equality" want

        testCase "it should throw unclosed pair error" <| fun _ ->
            let inp = seq {
                Lexer.ParenL
                Lexer.Symbol "symbol"
                Lexer.Symbol "."
                Lexer.EOF
            }
            let got = Parser.Parse (inp)
            let want = "unclosed pair"
            Expect.wantError "error" got |> Expect.equal "equality" want

        testCase "it should throw trailing element in pair" <| fun _ ->
            let inp = seq {
                Lexer.ParenL
                Lexer.Symbol "sym1"
                Lexer.Symbol "."
                Lexer.Symbol "sym2"
                Lexer.Symbol "sym3"
                Lexer.EOF
            }
            let got = Parser.Parse (inp)
            let want = "trailing object in pair"
            Expect.wantError "error" got |> Expect.equal "equality" want
    ]
