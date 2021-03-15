module Tests.Evaluator

open Expecto
open Library
open Library.Lisp

[<Tests>]
let tests =
    testList "Evaluator" [
        testCase "it should eval sum" <| fun _ ->
            let inp = Sexpr [Atom (Symbol (Sym "+"));
                             Atom (Number 1);
                             Atom (Number 2);
                             Atom (Number 3)]
            let _, got = Evaluator.eval Evaluator.defaultGlobe (inp)
            let want = Atom (Number 6)
            Expect.equal got want "the sum should be 6"

        testCase "it should eval product" <| fun _ ->
            let inp = Sexpr [Atom (Symbol (Sym "*"));
                             Atom (Number 1);
                             Atom (Number 2);
                             Atom (Number 4)]
            let _, got = Evaluator.eval Evaluator.defaultGlobe (inp)
            let want = Atom (Number 8)
            Expect.equal got want "the product should be 8"

        testCase "it should eval nested expressions" <| fun _ ->
            let inp = Sexpr [Atom (Symbol (Sym "*"));
                             Atom (Number 10);
                             Sexpr [Atom (Symbol (Sym "+"));
                                    Atom (Number 1);
                                    Atom (Number 2)]]
            let _, got = Evaluator.eval Evaluator.defaultGlobe (inp)
            let want = Atom (Number 30)
            Expect.equal got want "the sum should be 30"

        testCase "it should eval quote" <| fun _ ->
            let inp = Sexpr [Atom (Symbol (Sym "quote"));
                             Sexpr [Atom (Number 1);
                                    Atom (Number 2);
                                    Atom (Number 3)]]
            let _, got = Evaluator.eval Evaluator.defaultGlobe (inp)
            let want = Sexpr [Atom (Number 1);
                              Atom (Number 2);
                              Atom (Number 3)]
            Expect.equal got want "it should prevent evaluation"

        testCase "it should eval car" <| fun _ ->
            let inp = Sexpr [Atom (Symbol (Sym "car"));
                             Sexpr [Atom (Symbol (Sym ("quote")));
                                    Sexpr [Atom (Number 1);
                                           Atom (Number 2);
                                           Atom (Number 3)]]]
            let _, got = Evaluator.eval Evaluator.defaultGlobe (inp)
            let want = Atom (Number 1)
            Expect.equal got want "it should eval car"

        testCase "it should eval cdr" <| fun _ ->
            let inp = Sexpr [Atom (Symbol (Sym "cdr"));
                             Sexpr [Atom (Symbol (Sym "quote"));
                                    Sexpr [Atom (Number 1);
                                           Atom (Number 2);
                                           Atom (Number 3)]]]
            let _, got = Evaluator.eval Evaluator.defaultGlobe (inp)
            let want = Sexpr [Atom (Number 2);
                              Atom (Number 3)]
            Expect.equal got want "it should eval cdr"

        testCase "it should eval literal function" <| fun _ ->
            let inp = Sexpr [Sexpr [Atom (Symbol (Sym "lit"))
                                    Atom (Symbol (Sym "clo"))
                                    Atom (Symbol (Sym "nil"))
                                    Sexpr [Atom (Symbol (Sym "x"));
                                           Atom (Symbol (Sym "y"))]
                                    Sexpr [Atom (Symbol (Sym "+"))
                                           Atom (Symbol (Sym "x"))
                                           Atom (Symbol (Sym "y"))]];
                             Atom (Number 1);
                             Atom (Number 2)]
            let _, got = Evaluator.eval Evaluator.defaultGlobe (inp)
            let want = Atom (Number 3)
            Expect.equal got want "the sum should be 3"
    ]
