module Tests.Primitives

open Expecto
open Library

[<Tests>]
let tests =
    testList "Primitives" [
        testCase "it should return the sum neutral element" <| fun _ ->
            let inp = []
            let got = Primitives.add inp 0
            let want = Lisp.Atom (Lisp.Number 0)
            match got with
                | Lisp.Value.Sexpr got ->
                    Expect.equal got want "the sum should be 0"
                | e -> failtestf "fail: got %A" e

        testCase "it should return the sum" <| fun _ ->
            let inp = [Lisp.Value.Sexpr (Lisp.Atom (Lisp.Number 1));
                       Lisp.Value.Sexpr (Lisp.Atom (Lisp.Number 2));
                       Lisp.Value.Sexpr (Lisp.Atom (Lisp.Number 3))]
            let got = Primitives.add inp 3
            let want = Lisp.Atom (Lisp.Number 6)
            match got with
                | Lisp.Value.Sexpr got ->
                    Expect.equal got want "the sum should be 6"
                | e -> failtestf "fail: got %A" e
    ]
