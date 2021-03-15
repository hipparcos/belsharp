namespace Library

/// Evaluator provides the eval function.
module Evaluator =

    open Lisp

    let defPrim n p =
        { Primitive.Name = n
          Primitive.Func = p }
        |> Primitive
        |> Atom

    let defForm n f =
        { SpecialForm.Name = n
          SpecialForm.Func = f }
        |> SpecialForm
        |> Atom

    /// DefaultScope: a scope with primitives and special forms defined
    /// in the global environment.
    let defaultGlobe =
        Map.empty
           .Add(Sym "+", defPrim "+" Primitives.add)
           .Add(Sym "*", defPrim "*" Primitives.mul)
           .Add(Sym "car", defPrim "car" Primitives.car)
           .Add(Sym "cdr", defPrim "cdr" Primitives.cdr)
           .Add(Sym "lit", defForm "lit" SpecialForms.lit)
           .Add(Sym "quote", defForm "quote" SpecialForms.quote)
        |> Global

    let internal emptyScope =
        { Dynamic = ref (Dynamic (Map.empty, None))
          Global = ref (Global Map.empty)
          Lexical = ref (Lexical (Map.empty, None)) }

    let rec internal lookupDynamic s (Dynamic (d,prev)) =
        match Map.tryFind s d with
        | Some v -> Some v
        | None ->
            match prev with
            | Some p -> lookupDynamic s !p
            | None -> None

    let internal lookupGlobal s (Global g) = Map.tryFind s g
    
    let rec internal lookupLexical s (Lexical (d,prev)) =
        match Map.tryFind s d with
        | Some v -> Some v
        | None ->
            match prev with
            | Some p -> lookupLexical s !p
            | None -> None

    /// lookup: retrieve the value bound to a symbol in the current scope.
    let lookup (sym: Symbol) (scope: Scope): Sexpr =
        match sym with
        | Sym "globe" -> globeToAlist !scope.Global
        | Sym "scope" -> scopeToAlist !scope.Lexical
        | _ ->
            match (lookupDynamic sym !scope.Dynamic) with
            | Some sexpr -> sexpr
            | _ ->
                match (lookupLexical sym !scope.Lexical) with
                | Some sexpr -> sexpr
                | None ->
                    match (lookupGlobal sym !scope.Global) with
                    | Some sexpr -> sexpr
                    | None -> Atom Nil

    let internal evalSexpr scope sexpr: EvalStack * DataStack =
        match sexpr with
        | Atom (Symbol s) ->
            [], [lookup s scope]
        | Pair (car, cdr) ->
            [ EvalSexpr (car, scope.Lexical)
              EvalTop (1, scope.Lexical) ]
            , [cdr]
        | Sexpr (car :: cdr) ->
            [ EvalSexpr (car, scope.Lexical)
              EvalTop (List.length cdr, scope.Lexical) ]
            , cdr
        | _ -> [], [sexpr]

    let internal splitStack stack n = List.splitAt n stack

    let internal evalTop scope top nargs stack: EvalStack * DataStack =
        let evalSexprInScope init args =
            List.fold (fun acc it -> (EvalSexpr (it, scope.Lexical))::acc) init args
        match top with
        | Atom (Function f) ->
            let args, rest = splitStack stack nargs
            evalSexprInScope [CallFunction (f, nargs)] args, rest
        | Atom (Macro f) ->
            [ CallFunction (f, nargs)
            ], stack
        | Atom (Primitive p) ->
            let args, rest = splitStack stack nargs
            evalSexprInScope [CallPrimitive (p, nargs)] args, rest
        | Atom (SpecialForm f) ->
            [ CallSpecialForm (f, nargs, scope.Lexical)
            ], stack
        | _ ->
            let err = sprintf "'%s' is not a function or special form" (Printer.print top)
            [], (Atom (Error err))::stack

    let internal callFunction (func:Function) args: Instruction =
        let rec setArgsInScope (env:Environment) parameters values =
            match parameters with
            | [] -> env
            | p::ps ->
                let v, vs = match values with
                            | [] -> Atom Nil, []
                            | v::vs -> v, vs
                setArgsInScope (env.Add(p, v)) ps vs
        let scope = setArgsInScope Map.empty func.Parameters args
        EvalSexpr (func.Body, ref (Lexical (scope,Some func.Environment)))

    let internal callPrimitive (prim:Primitive) args: Sexpr =
        prim.Func args

    let internal callSpecialForm (form:SpecialForm) args scope: Scope * EvalStack * DataStack =
        form.Func scope args

    let internal evalInstruction scope instr (data:DataStack): Scope * EvalStack * DataStack =
        match instr with
        | EvalSexpr (sexpr, lexical) ->
            let newI, newD = evalSexpr {scope with Lexical = lexical} sexpr
            scope, newI, List.append newD data
        | EvalTop (nargs, lexical) ->
            match data with
            | [] ->
                let err = sprintf "evaluation stack is empty"
                scope, [], [Atom (Error err)]
            | top::rest ->
                let newI, newD = evalTop {scope with Lexical = lexical} top nargs rest
                scope, newI, List.append newD rest
        | CallFunction (f, nargs) ->
            let args, rest = splitStack data nargs
            let newI = callFunction f args
            scope, [newI], rest
        | CallPrimitive (p, nargs) ->
            let args, rest = splitStack data nargs
            let result = callPrimitive p args
            scope, [], result::rest
        | CallSpecialForm (f, nargs, lexical) ->
            let args, rest = splitStack data nargs
            let newS, newI, newD = callSpecialForm f args {scope with Lexical = lexical}
            newS, newI, List.append newD rest

    /// Eval: eval SEXPR in SCOPE.
    /// Use stacks of instructions and values to be tail recursive.
    let eval (globe: Global) (sexpr: Sexpr): Global * Sexpr =
        let rec loop (scope : Scope) instructions data =
            match instructions with
                | [] -> match data with
                        | result::_ -> !scope.Global, result
                        | [] -> !scope.Global, Atom (Error "Nothing to return")
                | instr::rest ->
                    let scope, instructions, data = evalInstruction scope instr data
                    loop scope (List.append instructions rest) data
        loop {emptyScope with Global = ref globe}
             [EvalSexpr (sexpr, emptyScope.Lexical)]
             []