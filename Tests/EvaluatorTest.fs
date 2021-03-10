module Tests.Evaluator

open Expecto
open Library

[<Tests>]
let tests =
    testList "Evaluator" [
        testCase "it should eval sum" <| fun _ ->
            let inp = Lisp.Sexpr [Lisp.Atom (Lisp.Symbol "+");
                                  Lisp.Atom (Lisp.Number 1);
                                  Lisp.Atom (Lisp.Number 2);
                                  Lisp.Atom (Lisp.Number 3)]
            let got = Evaluator.Eval Evaluator.DefaultScope (inp)
            let want = Lisp.Atom (Lisp.Number 6)
            Expect.equal got want "the sum should be 6"

        testCase "it should eval product" <| fun _ ->
            let inp = Lisp.Sexpr [Lisp.Atom (Lisp.Symbol "*");
                                  Lisp.Atom (Lisp.Number 1);
                                  Lisp.Atom (Lisp.Number 2);
                                  Lisp.Atom (Lisp.Number 4)]
            let got = Evaluator.Eval Evaluator.DefaultScope (inp)
            let want = Lisp.Atom (Lisp.Number 8)
            Expect.equal got want "the product should be 8"

        testCase "it should eval nested expressions" <| fun _ ->
            let inp = Lisp.Sexpr [Lisp.Atom (Lisp.Symbol "*");
                                  Lisp.Atom (Lisp.Number 10);
                                  Lisp.Sexpr [Lisp.Atom (Lisp.Symbol "+");
                                              Lisp.Atom (Lisp.Number 1);
                                              Lisp.Atom (Lisp.Number 2)]]
            let got = Evaluator.Eval Evaluator.DefaultScope (inp)
            let want = Lisp.Atom (Lisp.Number 30)
            Expect.equal got want "the sum should be 30"

        testCase "it should eval quote" <| fun _ ->
            let inp = Lisp.Sexpr [Lisp.Atom (Lisp.Symbol "quote");
                                  Lisp.Sexpr [Lisp.Atom (Lisp.Number 1);
                                              Lisp.Atom (Lisp.Number 2);
                                              Lisp.Atom (Lisp.Number 3)]]
            let got = Evaluator.Eval Evaluator.DefaultScope (inp)
            let want = Lisp.Sexpr [Lisp.Atom (Lisp.Number 1);
                                   Lisp.Atom (Lisp.Number 2);
                                   Lisp.Atom (Lisp.Number 3)]
            Expect.equal got want "it should prevent evaluation"

        testCase "it should eval car" <| fun _ ->
            let inp = Lisp.Sexpr [Lisp.Atom (Lisp.Symbol "car");
                                  Lisp.Sexpr [Lisp.Atom (Lisp.Symbol "quote");
                                              Lisp.Sexpr [Lisp.Atom (Lisp.Number 1);
                                                          Lisp.Atom (Lisp.Number 2);
                                                          Lisp.Atom (Lisp.Number 3)]]]
            let got = Evaluator.Eval Evaluator.DefaultScope (inp)
            let want = Lisp.Atom (Lisp.Number 1)
            Expect.equal got want "it should eval car"

        testCase "it should eval cdr" <| fun _ ->
            let inp = Lisp.Sexpr [Lisp.Atom (Lisp.Symbol "cdr");
                                  Lisp.Sexpr [Lisp.Atom (Lisp.Symbol "quote");
                                              Lisp.Sexpr [Lisp.Atom (Lisp.Number 1);
                                                          Lisp.Atom (Lisp.Number 2);
                                                          Lisp.Atom (Lisp.Number 3)]]]
            let got = Evaluator.Eval Evaluator.DefaultScope (inp)
            let want = Lisp.Sexpr [Lisp.Atom (Lisp.Number 2);
                                   Lisp.Atom (Lisp.Number 3)]
            Expect.equal got want "it should eval cdr"

        testCase "it should eval literal function" <| fun _ ->
            let inp = Lisp.Sexpr [Lisp.Sexpr [Lisp.Atom (Lisp.Symbol "lit");
                                              Lisp.Atom (Lisp.Symbol "clo");
                                              Lisp.Atom (Lisp.Symbol "nil")
                                              Lisp.Sexpr [Lisp.Atom (Lisp.Symbol "x");
                                                          Lisp.Atom (Lisp.Symbol "y")]
                                              Lisp.Sexpr [Lisp.Atom (Lisp.Symbol "+")
                                                          Lisp.Atom (Lisp.Symbol "x");
                                                          Lisp.Atom (Lisp.Symbol "y")]];
                                  Lisp.Atom (Lisp.Number 1);
                                  Lisp.Atom (Lisp.Number 2)]
            let got = Evaluator.Eval Evaluator.DefaultScope (inp)
            let want = Lisp.Atom (Lisp.Number 3)
            Expect.equal got want "the sum should be 3"
    ]
