module Tests.EndToEndT

open Expecto
open Library
open Library.Lisp
open Library.Evaluator
open Library.Printer
open Library.Reader

let evalFromString globe =
       read
    >> Result.map (eval globe)
    >> function
       | Result.Ok (globe, result) -> globe, print result
       | Result.Error err -> globe, err

let evalFromStrings globe =
    List.fold (fun (globe, _) -> evalFromString globe) (globe, "")

[<Tests>]
let tests =
    testList "End-to-end tests" [
        testCase "it should eval sum" <| fun _ ->
            let input = "(+ 1 2 3)"
            let _, got = evalFromString defaultGlobe input
            let want = "6"
            Expect.equal got want "the sum should be 6"

        testCase "it should oppose" <| fun _ ->
            let input = "(- 42)"
            let _, got = evalFromString defaultGlobe input
            let want = "-42"
            Expect.equal got want "the opposite should be -42"

        testCase "it should eval subtraction" <| fun _ ->
            let input = "(- 10 100 1000)"
            let _, got = evalFromString defaultGlobe input
            let want = "-1090"
            Expect.equal got want "the total should be -1090"

        testCase "it should set x then return it" <| fun _ ->
            let input =
                [ "(set 'x 42)"
                  "x" ]
            let _, got = evalFromStrings defaultGlobe input
            let want = "42"
            Expect.equal got want "the result should be 42"

        testCase "it should allow to access shadowed primitives" <| fun _ ->
            let input =
                [ "(set '+ 42)"
                  "(set 'lit nil)"
                  "((lit prim +) + +)" ]
            let _, got = evalFromStrings defaultGlobe input
            let want = "84"
            Expect.equal got want "the result should be 84"

        testCase "it should lookup the dynamic variable" <| fun _ ->
            let input =
                [ "(set 'double (lit clo nil (x) (* 2 x)))"
                  "(dyn x 21 (double 101))" ]
            let _, got = evalFromStrings defaultGlobe input
            let want = "42"
            Expect.equal got want "the result should be 42, not 202"

        testCase "it should nest dyn scopes" <| fun _ ->
            let input = "(dyn x 1 (dyn x 2 x))"
            let _, got = evalFromString defaultGlobe input
            let want = "2"
            Expect.equal got want "the result should be 2, not 1"

        testCase "it should unnest dyn scopes" <| fun _ ->
            let input = "(dyn x 1 (do (dyn x 2 x) x))"
            let _, got = evalFromString defaultGlobe input
            let want = "1"
            Expect.equal got want "the result should be 1, not 2"

        testCase "it should do, evaluates in order" <| fun _ ->
            let input = "(do (set 'x 1) (set 'x (+ x 1)) (set 'x (+ x 1)))"
            let _, got = evalFromString defaultGlobe input
            let want = "3"
            Expect.equal got want "the result should be 3, not 1"

        testCase "it should def then eval a function" <| fun _ ->
            let input =
                [ "(def double (x) (* 2 x))"
                  "(double 21)" ]
            let _, got = evalFromStrings (StdLib.loadInUnsafe defaultGlobe) input
            let want = "42"
            Expect.equal got want "the result should be 42"

        testCase "it should def then eval a closure" <| fun _ ->
            let input =
                [ "(def counter (x) (fn () (set 'x (+ 1 x))))"
                  "(set 'c1 (counter 39))"
                  "(c1)(c1)(c1)" ]
            let _, got = evalFromStrings (StdLib.loadInUnsafe defaultGlobe) input
            let want = "42"
            Expect.equal got want "the result should be 42 not 40"

        testCase "it should def then eval a macro" <| fun _ ->
            let input =
                [ "(mac thrice (e) (list 'do e e e))"
                  "(thrice (set 'x (+ x 1)))" ]
            let _, got = evalFromStrings (StdLib.loadInUnsafe defaultGlobe) input
            let want = "3"
            Expect.equal got want "the result should be 3 as the set form is executed multiple times"

        testCase "it should nest lexical scopes with let" <| fun _ ->
            let input = "(let x 1 (let y 2 (join x y)))"
            let _, got = evalFromString (StdLib.loadInUnsafe defaultGlobe) input
            let want = "(1 . 2)"
            Expect.equal got want "the result should be (1 . 2)"

        testCase "it should shadow binding in lexical scopes with let" <| fun _ ->
            let input = "(let x 1 (let x 2 x))"
            let _, got = evalFromString (StdLib.loadInUnsafe defaultGlobe) input
            let want = "2"
            Expect.equal got want "the result should be 2"

        testCase "it should let over lambda" <| fun _ ->
            let input =
                [ "(let x 0
                     (do (def inc () (set 'x (+ x 1)))
                         (def sqr () (set 'x (* x x)))))"
                  "(inc)(inc)(sqr)(inc)(inc)(sqr)" ]
            let _, got = evalFromStrings (StdLib.loadInUnsafe defaultGlobe) input
            let want = "36"
            Expect.equal got want "the result should be 36"

        testCase "it should eval conditionally to true" <| fun _ ->
            let input = "(if t 'true 'false)"
            let _, got = evalFromString (StdLib.loadInUnsafe defaultGlobe) input
            let want = "true"
            Expect.equal got want "the result should be the symbol true"

        testCase "it should eval conditionally to false" <| fun _ ->
            let input = "(if nil 'true 'false)"
            let _, got = evalFromString (StdLib.loadInUnsafe defaultGlobe) input
            let want = "false"
            Expect.equal got want "the result should be the symbol false"

        testCase "it should eval conditionally to nil when false and missing else" <| fun _ ->
            let input = "(if nil 'true)"
            let _, got = evalFromString (StdLib.loadInUnsafe defaultGlobe) input
            let want = "nil"
            Expect.equal got want "the result should be nil"

        testCase "it should eval conditionally to true when true and missing else" <| fun _ ->
            let input = "(if t 'true)"
            let _, got = evalFromString (StdLib.loadInUnsafe defaultGlobe) input
            let want = "true"
            Expect.equal got want "the result should be the symbol true"

        testCase "it should eval conditionally preventing eval of else branch" <| fun _ ->
            let input =
                [ "(set 'x 40)"
                  "(if t (set 'x (+ x 2)) (set 'x (+ x 20)))" ]
            let _, got = evalFromStrings (StdLib.loadInUnsafe defaultGlobe) input
            let want = "42"
            Expect.equal got want "the result should be 42 not 62"

        testCase "it should eval conditionally preventing eval of if branch" <| fun _ ->
            let input =
                [ "(set 'x 40)"
                  "(if nil (set 'x (+ x 20))) (set 'x (+ x 2))" ]
            let _, got = evalFromStrings (StdLib.loadInUnsafe defaultGlobe) input
            let want = "42"
            Expect.equal got want "the result should be 42 not 62"

        testCase "it should return the length of the list" <| fun _ ->
            let input = "(len '(1 2 3))"
            let _, got = evalFromString (StdLib.loadInUnsafe defaultGlobe) input
            let want = "3"
            Expect.equal got want "the result should be 3"

        testCase "it should return the length of the empty list" <| fun _ ->
            let input = "(len '())"
            let _, got = evalFromString (StdLib.loadInUnsafe defaultGlobe) input
            let want = "0"
            Expect.equal got want "the result should be 0"
    ]