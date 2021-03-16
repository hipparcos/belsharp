namespace Library

/// Special forms are forns that does not evaluate as per function
/// evaluation rules. An example is the `if` form which does not
/// evaluate all of its branches.
module SpecialForms =

    open Lisp
    open Environments

    let quote (scope : Scope) (args : DataStack) : SpecialFormResult =
        scope, [], args

    let stringAsSymbol = Sym >> Symbol >> Atom
    let internal symClo = stringAsSymbol "clo"
    let internal symLit = stringAsSymbol "lit"
    let internal symMac = stringAsSymbol "mac"

    let lit (scope : Scope) (args : DataStack) : SpecialFormResult =
        match args with
            | typ::env::(Sexpr parameters)::[body] when typ = symClo || typ = symMac ->
                let env = match env with
                          | Atom (Symbol (Sym "scope")) ->
                              scope.Lexical
                          | Sexpr env ->
                              ref (Lexical (alistToEnvironment env, Some scope.Lexical))
                          | _ ->
                              ref (Lexical (Map.empty, Some scope.Lexical))
                let parameters = parameters
                                 |> List.filter (fun it -> match it with
                                                           | Atom (Symbol _) -> true
                                                           | _ -> false)
                                 |> List.map (fun it -> match it with
                                                        | Atom (Symbol s) -> s
                                                        | _ -> Sym "")
                let cons = if typ = symMac then Macro else Function
                scope, [], [
                    Atom (cons {
                        Environment = env
                        Parameters = parameters
                        Body = body
                    })
                ]
            | _ ->
                scope, [], [Sexpr (symLit::args)]

    let set (scope:Scope) (args:DataStack) : SpecialFormResult =
        match args with
        | (Atom (Symbol s))::v::_ ->
            match setDynamic scope.Dynamic s v with
            | Some _ -> ()
            | None ->
                match setLexical scope.Lexical s v with
                | Some _ -> ()
                | None -> setGlobal scope.Global s v
            scope, [], [v]
        | _ ->
            scope, [], [Atom Nil]