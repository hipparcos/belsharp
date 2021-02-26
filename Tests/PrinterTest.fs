module Tests.Printer

open Expecto
open Library

[<Tests>]
let tests =
    testList "Lexer" [
        testCase "it should print nil" <| fun _ ->
            let inp = Lisp.Value.Sexpr (Lisp.Atom Lisp.Nil)
            let got = Printer.Print(inp)
            Expect.equal got "nil"
                "nil atom should be printed as nil"

        testCase "it should print empty list" <| fun _ ->
            let inp = Lisp.Value.Sexpr (Lisp.Sexpr [])
            let got = Printer.Print(inp)
            Expect.equal got "nil"
                "empty list should be printed as nil"

        testCase "it should print number" <| fun _ ->
            let inp = Lisp.Value.Sexpr (Lisp.Atom (Lisp.Number 100))
            let got = Printer.Print(inp)
            Expect.equal got "100"
                "100 atom should be printed as 100"

        testCase "it should print symbol" <| fun _ ->
            let inp = Lisp.Value.Sexpr (Lisp.Atom (Lisp.Symbol "symbol"))
            let got = Printer.Print(inp)
            Expect.equal got "symbol"
                "symbol atom should be printed as symbol"

        testCase "it should print pair" <| fun _ ->
            let inp =
                Lisp.Value.Sexpr (
                    Lisp.Pair ((Lisp.Atom (Lisp.Symbol "symbol1")),
                               (Lisp.Atom (Lisp.Symbol "symbol2"))))
            let got = Printer.Print(inp)
            Expect.equal got "(symbol1 . symbol2)"
                "pair should be printed as dotted pair"

        testCase "it should print nested pairs" <| fun _ ->
            let inp =
                Lisp.Value.Sexpr (
                    Lisp.Pair ((Lisp.Atom (Lisp.Symbol "sym1")),
                               (Lisp.Pair ((Lisp.Atom (Lisp.Symbol "sym2")),
                                           (Lisp.Sexpr [Lisp.Atom (Lisp.Symbol "sym3")])))))
            let got = Printer.Print(inp)
            Expect.equal got "(sym1 . (sym2 . (sym3)))"
                "nested pairs should be printed as dotted pairs"

        testCase "it should print list" <| fun _ ->
            let inp =
                Lisp.Value.Sexpr (
                    Lisp.Sexpr [Lisp.Atom (Lisp.Symbol "sym1");
                                Lisp.Atom (Lisp.Symbol "sym2");
                                Lisp.Atom (Lisp.Symbol "sym3")])
            let got = Printer.Print(inp)
            Expect.equal got "(sym1 sym2 sym3)"
                "list should be printed as list"

        testCase "it should print list of nil" <| fun _ ->
            let inp = Lisp.Value.Sexpr (Lisp.Sexpr [Lisp.Atom Lisp.Nil])
            let got = Printer.Print(inp)
            Expect.equal got "(nil)"
                "list of nil atom should be printed as list of nil"

        testCase "it should print car nested lists" <| fun _ ->
            let inp =
                Lisp.Value.Sexpr (
                    Lisp.Sexpr [Lisp.Sexpr [Lisp.Sexpr [Lisp.Atom (Lisp.Symbol "sym1")];
                                            Lisp.Atom (Lisp.Symbol "sym2")];
                                Lisp.Atom (Lisp.Symbol "sym3")])
            let got = Printer.Print(inp)
            Expect.equal got "(((sym1) sym2) sym3)"
                "car nested list should print as list"

        testCase "it should print cadr nested lists" <| fun _ ->
            let inp =
                Lisp.Value.Sexpr (
                    Lisp.Sexpr [Lisp.Atom (Lisp.Symbol "sym1");
                                Lisp.Sexpr [Lisp.Atom (Lisp.Symbol "sym2");
                                            Lisp.Sexpr [Lisp.Atom (Lisp.Symbol "sym3")]]])
            let got = Printer.Print(inp)
            Expect.equal got "(sym1 (sym2 (sym3)))"
                "cadr nested list should print as list"
    ]
