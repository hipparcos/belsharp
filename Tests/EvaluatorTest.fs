module Tests.Evaluator

open Library
open NUnit.Framework

[<SetUp>]
let Setup () =
    ()

[<Test>]
let it_should_eval_sum () =
    let got = Evaluator.Eval Evaluator.DefaultScope (
                  Lisp.Sexpr [Lisp.Atom (Lisp.Symbol "+");
                              Lisp.Atom (Lisp.Number 1);
                              Lisp.Atom (Lisp.Number 2);
                              Lisp.Atom (Lisp.Number 3)]
    )
    match got with
        | Lisp.Value.Sexpr v ->
            Assert.That(
                v,
                Is.EqualTo(
                    Lisp.Atom (Lisp.Number 6)
                )
            )
        | v -> Assert.Fail($"got {v}")

[<Test>]
let it_should_eval_quote () =
    let got = Evaluator.Eval Evaluator.DefaultScope (
                  Lisp.Sexpr [Lisp.Atom (Lisp.Symbol "quote");
                              Lisp.Sexpr [Lisp.Atom (Lisp.Number 1);
                                          Lisp.Atom (Lisp.Number 2);
                                          Lisp.Atom (Lisp.Number 3)]]
    )
    match got with
        | Lisp.Value.Sexpr v ->
            Assert.That(
                v,
                Is.EqualTo(
                    Lisp.Sexpr [Lisp.Atom (Lisp.Number 1);
                                Lisp.Atom (Lisp.Number 2);
                                Lisp.Atom (Lisp.Number 3)]
                )
            )
        | v -> Assert.Fail($"got {v}")
