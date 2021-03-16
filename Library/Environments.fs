namespace Library

module Environments =

    open Lisp

    let rec internal findInDynamic f s dyn =
        let (Dynamic (env,prev)) = !dyn
        match Map.tryFind s env with
        | Some v -> Some (f dyn v)
        | None ->
            match prev with
            | Some p -> findInDynamic f s p
            | None -> None

    let rec internal findInLexical f s lex =
        let (Lexical (env,prev)) = !lex
        match Map.tryFind s env with
        | Some v -> Some (f lex v)
        | None ->
            match prev with
            | Some p -> findInLexical f s p
            | None -> None

    let lookupDynamic = findInDynamic (fun _ -> id)

    let lookupGlobal s (Global g) = Map.tryFind s g

    let lookupLexical = findInLexical (fun _ -> id)

    // litRef is an early declaration of the lit special form.
    let litRef : SpecialForm ref =
        ref { Name = "litRef"
              Func = fun _ _ -> failwith "litRef must be defined in SpecialForms.fs"
              EvalArgs = false }

    /// lookup: retrieve the value bound to a symbol in the current scope.
    let lookup (sym: Symbol) (scope: Scope): Sexpr =
        match sym with
        | Sym "globe" -> globeToAlist !scope.Global
        | Sym "scope" -> scopeToAlist !scope.Lexical
        | Sym "lit" -> Atom (SpecialForm !litRef)
        | _ ->
            match (lookupDynamic sym scope.Dynamic) with
            | Some sexpr -> sexpr
            | _ ->
                match (lookupLexical sym scope.Lexical) with
                | Some sexpr -> sexpr
                | None ->
                    match (lookupGlobal sym !scope.Global) with
                    | Some sexpr -> sexpr
                    | None -> Atom Nil

    let setGlobal (env:Global ref) (sym:Symbol) (value:Sexpr) =
        let set (Global g) = Global (g.Add(sym, value))
        env := set !env

    let setDynamic (env:Dynamic ref) (sym:Symbol) (value:Sexpr) =
        let set (Dynamic (env,prev)) = Dynamic (env.Add(sym, value), prev)
        findInDynamic (fun dyn _ -> env := set !dyn) sym env

    let setLexical (env:Lexical ref) (sym:Symbol) (value:Sexpr) =
        let set (Lexical (env,prev)) = Lexical (env.Add(sym, value), prev)
        findInLexical (fun lex _ -> env := set !lex) sym env