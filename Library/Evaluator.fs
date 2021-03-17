namespace Library

/// Evaluator provides the eval function.
module Evaluator =

    open Lisp
    open Environments

    /// DefaultScope: a scope with primitives and special forms defined
    /// in the global environment.
    let defaultGlobe =
        Map.empty<Symbol, Sexpr>
        |> fun globe ->
               Map.fold (fun (g:Environment) s p -> g.Add(s, p |> Primitive |> Atom))
                        globe
                        Primitives.primitives
        |> fun globe ->
               Map.fold (fun (g:Environment) s f -> g.Add(s, f |> SpecialForm |> Atom))
                        globe
                        SpecialForms.specialForms
        |> Global

    let emptyScope =
        { Dynamic = ref (Dynamic (Map.empty, None))
          Global = Global Map.empty
          Lexical = ref (Lexical (Map.empty, None)) }

    let internal evalSexpr scope sexpr: EvalStack * DataStack =
        match sexpr with
        | Atom (Symbol s) ->
            [], [lookup s scope]
        | Pair (car, cdr) ->
            [ EvalSexpr (car, scope.Dynamic, scope.Lexical)
              EvalTop (1, scope.Dynamic, scope.Lexical) ]
            , [cdr]
        | Sexpr (car :: cdr) ->
            [ EvalSexpr (car, scope.Dynamic, scope.Lexical)
              EvalTop (List.length cdr, scope.Dynamic, scope.Lexical) ]
            , cdr
        | _ -> [], [sexpr]

    let internal splitStack stack n = List.splitAt n stack

    let internal evalTop scope top nargs stack: EvalStack * DataStack =
        let evalSexprInScope init args =
            List.fold (fun acc it -> (EvalSexpr (it, scope.Dynamic, scope.Lexical))::acc)
                init
                (List.rev args)
        match top with
        | Atom (Function f) ->
            let args, rest = splitStack stack nargs
            evalSexprInScope [CallFunction (f, nargs, scope.Dynamic)] args, rest
        | Atom (Macro f) ->
            [ CallMacro (f, nargs, scope.Dynamic)
              EvalTop (0, scope.Dynamic, scope.Lexical)
            ], stack
        | Atom (Primitive p) ->
            let args, rest = splitStack stack nargs
            evalSexprInScope [CallPrimitive (p, nargs)] args, rest
        | Atom (SpecialForm f) ->
            if f.EvalArgs then
                let args, rest = splitStack stack nargs
                evalSexprInScope [CallSpecialForm (f, nargs, scope.Dynamic, scope.Lexical)] args, rest
            else
                [CallSpecialForm (f, nargs, scope.Dynamic, scope.Lexical)], stack
        | Sexpr _ ->
            [EvalSexpr (top, scope.Dynamic, scope.Lexical)], stack
        | _ ->
            let err = sprintf "'%s' is not a function or special form" (Printer.print top)
            [], (Atom (Error err))::stack

    let internal callFunction scope (func:Function) args: Instruction =
        let rec setArgsInScope (env:Environment) parameters values =
            match parameters with
            | [] -> env
            | p::ps ->
                let v, vs = match values with
                            | [] -> Atom Nil, []
                            | v::vs -> v, vs
                setArgsInScope (env.Add(p, v)) ps vs
        let lex = setArgsInScope Map.empty func.Parameters args
        EvalSexpr (func.Body, scope.Dynamic, ref (Lexical (lex,Some func.Environment)))

    let internal callPrimitive (prim:Primitive) args: Sexpr =
        prim.Func (List.rev args)

    let internal callSpecialForm (form:SpecialForm) args scope: Global * EvalStack * DataStack =
        form.Func scope (if form.EvalArgs then List.rev args else args)

    let internal evalInstruction globe instr (data:DataStack): Global * EvalStack * DataStack =
        match instr with
        | EvalSexpr (sexpr, dynamic, lexical) ->
            let newI, newD = evalSexpr {Global = globe; Dynamic = dynamic; Lexical = lexical} sexpr
            globe, newI, List.append newD data
        | EvalTop (nargs, dynamic, lexical) ->
            match data with
            | [] ->
                let err = sprintf "evaluation stack is empty"
                globe, [], [Atom (Error err)]
            | top::rest ->
                let newI, newD = evalTop {Global = globe; Dynamic = dynamic; Lexical = lexical} top nargs rest
                globe, newI, newD
        | CallFunction (f, nargs, dynamic) ->
            let args, rest = splitStack data nargs
            let newI = callFunction {Global = globe; Dynamic = dynamic; Lexical = f.Environment} f (List.rev args)
            globe, [newI], rest
        | CallMacro (f, nargs, dynamic) ->
            let args, rest = splitStack data nargs
            let newI = callFunction {Global = globe; Dynamic = dynamic; Lexical = f.Environment} f args
            globe, [newI], rest
        | CallPrimitive (p, nargs) ->
            let args, rest = splitStack data nargs
            let result = callPrimitive p args
            globe, [], result::rest
        | CallSpecialForm (f, nargs, dynamic, lexical) ->
            let args, rest = splitStack data nargs
            let globe, newI, newD = callSpecialForm f args {Global = globe; Dynamic = dynamic; Lexical = lexical}
            globe, newI, List.append newD rest

    /// Eval: eval SEXPR in SCOPE.
    /// Use stacks of instructions and values to be tail recursive.
    let eval (globe: Global) (sexpr: Sexpr): Global * Sexpr =
        let rec loop globe instructions data =
            match instructions with
                | [] -> match data with
                        | result::_ -> globe, result
                        | [] -> globe, Atom (Error "Nothing to return")
                | instr::rest ->
                    let globe, instructions, data = evalInstruction globe instr data
                    loop globe (List.append instructions rest) data
        loop globe [EvalSexpr (sexpr, emptyScope.Dynamic, emptyScope.Lexical)] []