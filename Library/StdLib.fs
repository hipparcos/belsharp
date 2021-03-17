﻿module Library.StdLib

let lib = """
(set 'def (lit mac nil (n p e) (list 'set (list 'quote n) (list 'lit 'clo 'scope p e))))
(set 'fn (lit mac nil (p e) (list 'lit 'clo 'scope p e)))
(set 'mac (lit mac nil (n p e) (list 'set (list 'quote n) (list 'lit 'mac 'scope p e))))

(mac let (parms val body) (list (list 'fn (list parms) body) val))

(def len (lst n)
  (if (car lst)
      (len (cdr lst) (+ n 1))
      (if n n 0)))
"""

let loadIn globe = Reader.read >> Result.map (Evaluator.eval globe) <| lib

let loadInUnsafe = loadIn >> (function | Ok (globe,_) -> globe | Error err -> failwith err)