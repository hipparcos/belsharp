module Library.StdLib

let lib = """
(set 'def (lit mac nil (n p e) (list 'set (list 'quote n) (list 'lit 'clo 'scope p e))))
(set 'fn (lit mac nil (p e) (list 'lit 'clo 'scope p e)))
(set 'mac (lit mac nil (n p e) (list 'set (list 'quote n) (list 'lit 'mac 'scope p e))))

(mac let (parameter val body)
  (list (list 'fn (list parameter) body) val))

(def len (lst n)
  (if (car lst)
      (len (cdr lst) (+ n 1))
      (if n n 0)))

(def rev (lst acc)
  (if (car lst)
      (rev (cdr lst) (join (car lst) acc))
      acc))

(def map (f lst acc)
  (if (car lst)
      (map f (cdr lst) (join (f (car lst)) acc))
      (rev acc)))

(def reduce (f lst acc)
  (if (car lst)
      (reduce f (cdr lst) (f acc (car lst)))
      acc))

(def range (n acc)
     (if (= n 0)
         acc
         (range (- n 1) (join n acc))))
"""

let loadIn globe = Reader.read >> Result.map (Evaluator.eval globe) <| lib

let loadInUnsafe = loadIn >> (function | Ok (globe,_) -> globe | Error err -> failwith err)