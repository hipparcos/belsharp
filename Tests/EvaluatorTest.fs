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
            match got with
                | Lisp.Value.Sexpr got ->
                    Expect.equal got want "the sum should be 6"
                | e -> failtestf "fail: got %A" e

        testCase "it should eval quote" <| fun _ ->
            let inp = Lisp.Sexpr [Lisp.Atom (Lisp.Symbol "quote");
                                  Lisp.Sexpr [Lisp.Atom (Lisp.Number 1);
                                              Lisp.Atom (Lisp.Number 2);
                                              Lisp.Atom (Lisp.Number 3)]]
            let got = Evaluator.Eval Evaluator.DefaultScope (inp)
            let want = Lisp.Sexpr [Lisp.Atom (Lisp.Number 1);
                                   Lisp.Atom (Lisp.Number 2);
                                   Lisp.Atom (Lisp.Number 3)]
            match got with
                | Lisp.Value.Sexpr got ->
                    Expect.equal got want "it should prevent evaluation"
                | e -> failtestf "fail: got %A" e
    ]
