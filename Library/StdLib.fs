module Library.StdLib

let lib = """
(set 'def (lit mac nil (n p e) (list 'set (list 'quote n) (list 'lit 'clo 'scope p e))))
(set 'mac (lit mac nil (n p e) (list 'set (list 'quote n) (list 'lit 'mac 'scope p e))))
"""

let loadIn globe = Reader.read >> Result.map (Evaluator.eval globe) <| lib

let loadInUnsafe = loadIn >> (function | Ok (globe,_) -> globe | Error err -> failwith err)