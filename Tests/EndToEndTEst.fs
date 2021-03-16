module Tests.EndToEndT

open Expecto
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
    ]