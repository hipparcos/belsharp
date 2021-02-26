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
            Expect.equal got want "the sum should be 0"

        testCase "it should return the sum" <| fun _ ->
            let inp = [Lisp.Atom (Lisp.Number 1);
                       Lisp.Atom (Lisp.Number 2);
                       Lisp.Atom (Lisp.Number 3)]
            let got = Primitives.add inp 3
            let want = Lisp.Atom (Lisp.Number 6)
            Expect.equal got want "the sum should be 6"

        testCase "it should return the product neutral elemment" <| fun _ ->
            let inp = []
            let got = Primitives.mul inp 0
            let want = Lisp.Atom (Lisp.Number 1)
            Expect.equal got want "the product should be 1"

        testCase "it should return the product" <| fun _ ->
            let inp = [Lisp.Atom (Lisp.Number 1);
                       Lisp.Atom (Lisp.Number 2);
                       Lisp.Atom (Lisp.Number 4)]
            let got = Primitives.mul inp 3
            let want = Lisp.Atom (Lisp.Number 8)
            Expect.equal got want "the product should be 8"

        testCase "it should return the car of a list" <| fun _ ->
            let inp = [Lisp.Sexpr [Lisp.Atom (Lisp.Number 1);
                                   Lisp.Atom (Lisp.Number 2);
                                   Lisp.Atom (Lisp.Number 3)]]
            let got = Primitives.car inp 1
            let want = Lisp.Atom (Lisp.Number 1)
            Expect.equal got want "the prod should be 8"

        testCase "it should return the car of an empty list" <| fun _ ->
            let inp = [Lisp.Sexpr []]
            let got = Primitives.car inp 1
            let want = Lisp.Atom Lisp.Nil
            Expect.equal got want "the car should be nil"

        testCase "it should return the car of a pair" <| fun _ ->
            let inp = [Lisp.Pair (Lisp.Atom (Lisp.Number 1),
                                  Lisp.Atom (Lisp.Number 2))]
            let got = Primitives.car inp 1
            let want = Lisp.Atom (Lisp.Number 1)
            Expect.equal got want "the car should be 1"

        testCase "it should return the cdr of a list" <| fun _ ->
            let inp = [Lisp.Sexpr [Lisp.Atom (Lisp.Number 1);
                                   Lisp.Atom (Lisp.Number 2);
                                   Lisp.Atom (Lisp.Number 3)]]
            let got = Primitives.cdr inp 1
            let want = Lisp.Sexpr [Lisp.Atom (Lisp.Number 2);
                                   Lisp.Atom (Lisp.Number 3)]
            Expect.equal got want "the cdr should be (2 3)"

        testCase "it should return the cdr of an empty list" <| fun _ ->
            let inp = [Lisp.Sexpr []]
            let got = Primitives.cdr inp 1
            let want = Lisp.Atom Lisp.Nil
            Expect.equal got want "the cdr should be nil"

        testCase "it should return the cdr of a pair" <| fun _ ->
            let inp = [Lisp.Pair (Lisp.Atom (Lisp.Number 1),
                                  Lisp.Atom (Lisp.Number 2))]
            let got = Primitives.cdr inp 1
            let want = Lisp.Atom (Lisp.Number 2)
            Expect.equal got want "the cdr should be 2"
    ]
