module Tests.Primitives

open Expecto
open Library
open Library.Lisp

[<Tests>]
let tests =
    testList "Primitives" [
        testCase "it should return the sum neutral element" <| fun _ ->
            let inp = []
            let got = Primitives.add inp
            let want = Atom (Number 0)
            Expect.equal got want "the sum should be 0"

        testCase "it should return the sum" <| fun _ ->
            let inp = [Atom (Number 1);
                       Atom (Number 2);
                       Atom (Number 3)]
            let got = Primitives.add inp
            let want = Atom (Number 6)
            Expect.equal got want "the sum should be 6"

        testCase "it should return the product neutral elemment" <| fun _ ->
            let inp = []
            let got = Primitives.mul inp
            let want = Atom (Number 1)
            Expect.equal got want "the product should be 1"

        testCase "it should return the product" <| fun _ ->
            let inp = [Atom (Number 1);
                       Atom (Number 2);
                       Atom (Number 4)]
            let got = Primitives.mul inp
            let want = Atom (Number 8)
            Expect.equal got want "the product should be 8"

        testCase "it should return the car of a list" <| fun _ ->
            let inp = [Sexpr [Atom (Number 1);
                              Atom (Number 2);
                              Atom (Number 3)]]
            let got = Primitives.car inp
            let want = Atom (Number 1)
            Expect.equal got want "the prod should be 8"

        testCase "it should return the car of an empty list" <| fun _ ->
            let inp = [Sexpr []]
            let got = Primitives.car inp
            let want = Atom Nil
            Expect.equal got want "the car should be nil"

        testCase "it should return the car of a pair" <| fun _ ->
            let inp = [Pair (Atom (Number 1),
                             Atom (Number 2))]
            let got = Primitives.car inp
            let want = Atom (Number 1)
            Expect.equal got want "the car should be 1"

        testCase "it should return the cdr of a list" <| fun _ ->
            let inp = [Sexpr [Atom (Number 1);
                              Atom (Number 2);
                              Atom (Number 3)]]
            let got = Primitives.cdr inp
            let want = Sexpr [Atom (Number 2);
                              Atom (Number 3)]
            Expect.equal got want "the cdr should be (2 3)"

        testCase "it should return the cdr of an empty list" <| fun _ ->
            let inp = [Sexpr []]
            let got = Primitives.cdr inp
            let want = Atom Nil
            Expect.equal got want "the cdr should be nil"

        testCase "it should return the cdr of a pair" <| fun _ ->
            let inp = [Pair (Atom (Number 1),
                             Atom (Number 2))]
            let got = Primitives.cdr inp
            let want = Atom (Number 2)
            Expect.equal got want "the cdr should be 2"
    ]
