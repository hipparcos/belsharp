namespace Library

open FParsec
open Library

/// Special forms are forns that does not evaluate as per function
/// evaluation rules. An example is the `if` form which does not
/// evaluate all of its branches.
module SpecialForms =

    open Lisp
    open Environments

    let quote (scope : Scope) (args : DataStack) : SpecialFormResult =
        scope.Global, [], args

    let dyn (scope:Scope) (args:DataStack) : SpecialFormResult =
        match args with
        | (Atom (Symbol s))::v::[body] ->
            let dyn = ref (Dynamic (Map.empty, Some scope.Dynamic))
            let setDyn (scope:Scope) (args:DataStack) =
                match !dyn with
                | Dynamic (env, prev) -> dyn := Dynamic (env.Add(s, args.Head), prev)
                scope.Global, [], []
            let form =
                { SpecialForm.Name = "dyn-internal"
                  SpecialForm.Func = setDyn
                  SpecialForm.EvalArgs = false }
            scope.Global, [
                EvalSexpr (v, scope.Dynamic, scope.Lexical)
                CallSpecialForm (form, 1, scope.Dynamic, scope.Lexical)
                EvalSexpr (body, dyn, scope.Lexical)
            ], []
        | _ ->
            scope.Global, [], [Atom Nil]

    let ifElse (scope:Scope) (args:DataStack) : SpecialFormResult =
        match args with
        | [] ->
            scope.Global, [], [Atom (Error "malformed if")]
        | test::args ->
            let thenBranch, elseBranch = match args with
                                         | i::e::_ -> i, e
                                         | [i] -> i, Atom Nil
                                         | _ -> Atom Nil, Atom Nil
            let innerIf scope cond : SpecialFormResult =
                match cond with
                | [Atom Nil] ->
                    scope.Global, [
                        EvalSexpr (elseBranch, scope.Dynamic, scope.Lexical)
                    ], []
                | _ ->
                    scope.Global, [
                        EvalSexpr (thenBranch, scope.Dynamic, scope.Lexical)
                    ], []
            let form =
                { SpecialForm.Name = "if-internal"
                  SpecialForm.Func = innerIf
                  SpecialForm.EvalArgs = false }
            scope.Global, [
                EvalSexpr (test, scope.Dynamic, scope.Lexical)
                CallSpecialForm (form, 1, scope.Dynamic, scope.Lexical)
            ], []

    let set (scope:Scope) (args:DataStack) : SpecialFormResult =
        match args with
        | (Atom (Symbol s))::v::_ ->
            match setDynamic scope.Dynamic s v with
            | Some _ -> scope.Global
            | None ->
                match setLexical scope.Lexical s v with
                | Some _ -> scope.Global
                | None -> setGlobal scope.Global s v
            , [], [v]
        | _ ->
            scope.Global, [], [Atom Nil]

    let defForm n p e =
        { SpecialForm.Name = n
          SpecialForm.Func = p
          SpecialForm.EvalArgs = e }

    let internal specialFormsWithoutLit : Map<Symbol, SpecialForm> =
        let forms = [
            defForm "dyn" dyn false
            defForm "if" ifElse false
            defForm "quote" quote false
            defForm "set" set true
        ]
        List.fold (fun forms (f:SpecialForm) -> forms.Add(Sym f.Name, f)) Map.empty forms

    let stringAsSymbol = Sym >> Symbol >> Atom
    let internal symClo = stringAsSymbol "clo"
    let internal symLit = stringAsSymbol "lit"
    let internal symMac = stringAsSymbol "mac"

    let rec lit (scope : Scope) (args : DataStack) : SpecialFormResult =
        match args with
            | (Atom (Symbol (Sym "prim")))::[(Atom (Symbol sym))] ->
                let v = match Primitives.primitives.TryFind sym with
                        | Some v -> Atom (Primitive v)
                        | None ->
                            let (Sym sym) = sym
                            let err = sprintf "no primitives names '%s'" sym
                            Atom (Error err)
                scope.Global, [], [v]
            | (Atom (Symbol (Sym "form")))::[(Atom (Symbol (Sym "lit")))] ->
                let lit = !litRef |> SpecialForm |> Atom
                scope.Global, [], [lit]
            | (Atom (Symbol (Sym "form")))::[(Atom (Symbol sym))] ->
                let v = match specialFormsWithoutLit.TryFind sym with
                        | Some v -> Atom (SpecialForm v)
                        | None ->
                            let (Sym sym) = sym
                            let err = sprintf "no special forms names '%s'" sym
                            Atom (Error err)
                scope.Global, [], [v]
            | typ::env::parameters::[body] when typ = symClo || typ = symMac ->
                let env = match env with
                          | Atom (Symbol (Sym "scope")) ->
                              scope.Lexical
                          | Sexpr env ->
                              ref (Lexical (alistToEnvironment env, Some scope.Lexical))
                          | _ ->
                              ref (Lexical (Map.empty, Some scope.Lexical))
                let parameters = match parameters with
                                 | Sexpr parameters ->
                                     parameters
                                     |> List.filter (fun it -> match it with
                                                               | Atom (Symbol _) -> true
                                                               | _ -> false)
                                     |> List.map (fun it -> match it with
                                                            | Atom (Symbol s) -> s
                                                            | _ -> Sym "")
                                     |> ListOfSymbols
                                 | Atom (Symbol it) -> SingleList it
                                 | _ -> ListOfSymbols []
                let cons = if typ = symMac then Macro else Function
                scope.Global, [], [
                    Atom (cons {
                        Environment = env
                        Parameters = parameters
                        Body = body
                    })
                ]
            | _ ->
                scope.Global, [], [Sexpr (symLit::args)]

    do litRef := defForm "lit" lit false
    let specialForms = specialFormsWithoutLit.Add(Sym "lit", !litRef)