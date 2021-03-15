module Tests.Parser

open Expecto
open Expecto.Flip
open Library
open Library.Lisp

[<Tests>]
let tests =
    testList "Parser" [
        testCase "it should return nil" <| fun _ ->
            let input = ""
            let got = Parser.parse input
            let want = Atom Nil
            Expect.wantOk "ok" got |> Expect.equal "equality" want

        testCase "it should parse number" <| fun _ ->
            let input = "100"
            let got = Parser.parse input
            let want = Atom (Number 100)
            Expect.wantOk "ok" got |> Expect.equal "equality" want

        testCase "it should parse symbol" <| fun _ ->
            let input = "symbol"
            let got = Parser.parse input
            let want = Atom (Symbol (Sym "symbol"))
            Expect.wantOk "ok" got |> Expect.equal "equality" want

        testCase "it should parse nil symbol as nil" <| fun _ ->
            let input = "nil"
            let got = Parser.parse input
            let want = Atom Nil
            Expect.wantOk "ok" got |> Expect.equal "equality" want

        testCase "it should parse empty list as nil" <| fun _ ->
            let input = "()"
            let got = Parser.parse input
            let want = Atom Nil
            Expect.wantOk "ok" got |> Expect.equal "equality" want

        testCase "it should parse pair" <| fun _ ->
            let input = "(symbol1 . symbol2)"
            let got = Parser.parse input
            let want = Pair ((Atom (Symbol (Sym "symbol1"))),
                             (Atom (Symbol (Sym "symbol2"))))
            Expect.wantOk "ok" got |> Expect.equal "equality" want

        testCase "it should parse pair of dots" <| fun _ ->
            let input = "(. . .)"
            let got = Parser.parse input
            let want = Pair ((Atom (Symbol (Sym "."))),
                             (Atom (Symbol (Sym "."))))
            Expect.wantOk "ok" got |> Expect.equal "equality" want

        testCase "it should parse cdr nested pairs" <| fun _ ->
            let input = "(sym1 . (sym2 . (sym3)))"
            let got = Parser.parse input
            let want = Pair ((Atom (Symbol (Sym "sym1"))),
                             (Pair ((Atom (Symbol (Sym "sym2"))),
                                    (Sexpr [Atom (Symbol (Sym "sym3"))]))))
            Expect.wantOk "ok" got |> Expect.equal "equality" want

        testCase "it should parse list" <| fun _ ->
            let input = "(sym1 sym2 sym3)"
            let got = Parser.parse input
            let want = Sexpr [Atom (Symbol (Sym "sym1"));
                              Atom (Symbol (Sym "sym2"));
                              Atom (Symbol (Sym "sym3"))]
            Expect.wantOk "ok" got |> Expect.equal "equality" want

        testCase "it should parse car nested list" <| fun _ ->
            let input = "(((sym1) sym2) sym3)"
            let got = Parser.parse input
            let want = Sexpr [Sexpr [Sexpr [Atom (Symbol (Sym "sym1"))];
                                            Atom (Symbol (Sym "sym2"))];
                                            Atom (Symbol (Sym "sym3"))]
            Expect.wantOk "ok" got |> Expect.equal "equality" want

        testCase "it should parse cadr nested list" <| fun _ ->
            let input = "(sym1 (sym2 (sym3)))"
            let got = Parser.parse input
            let want = Sexpr [Atom (Symbol (Sym "sym1"));
                              Sexpr [Atom (Symbol (Sym "sym2"));
                                     Sexpr [Atom (Symbol (Sym "sym3"))]]]
            Expect.wantOk "ok" got |> Expect.equal "equality" want

        testCase "it should quote the list" <| fun _ ->
            let input = "'(1 2 3)"
            let got = Parser.parse input
            let want = Sexpr [Atom (Symbol (Sym "quote"));
                              Sexpr [Atom (Number 1)
                                     Atom (Number 2)
                                     Atom (Number 3)]]
            Expect.wantOk "ok" got |> Expect.equal "equality" want

        testCase "it should throw unexpected closing parenthesis" <| fun _ ->
            let input = ")"
            let got = Parser.parse input
            let want = "Error in Ln: 1 Col: 1
)
^
Expecting: atom, end of input, list, pair or quote
"
            Expect.wantError "error" got |> Expect.equal "equality" want

        testCase "it should throw unclosed list error" <| fun _ ->
            let input = "(symbol"
            let got = Parser.parse input
            let want = "Error in Ln: 1 Col: 8
(symbol
       ^
Note: The error occurred at the end of the input stream.
Expecting: atom, list, pair, quote or ')'
"
            Expect.wantError "error" got |> Expect.equal "equality" want

        testCase "it should throw unclosed pair error" <| fun _ ->
            let input = "(symbol ."
            let got = Parser.parse input
            let want = "Error in Ln: 1 Col: 10
(symbol .
         ^
Note: The error occurred at the end of the input stream.
Expecting: atom, list, pair or quote
"
            Expect.wantError "error" got |> Expect.equal "equality" want

        testCase "it should throw trailing element in pair" <| fun _ ->
            let input = "(sym1 . sym2 sym3"
            let got = Parser.parse input
            let want = "Error in Ln: 1 Col: 14
(sym1 . sym2 sym3
             ^
Expecting: ')'
"
            Expect.wantError "error" got |> Expect.equal "equality" want
    ]
