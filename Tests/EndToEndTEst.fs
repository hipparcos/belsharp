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
    ]