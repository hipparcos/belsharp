namespace Library

/// Evaluator provides the eval function.
module Evaluator =

    let defPrim n p =
        { Lisp.Primitive.Name = n
          Lisp.Primitive.Func = p }
        |> Lisp.Primitive
        |> Lisp.Atom

    let defForm n f =
        { Lisp.SpecialForm.Name = n
          Lisp.SpecialForm.Func = f }
        |> Lisp.SpecialForm
        |> Lisp.Atom

    /// DefaultScope: a scope with primitives and special forms defined
    /// in the global environment.
    let DefaultScope =
        { Lisp.Dynamic = Map.empty
          Lisp.Global =
              Map
                  .empty
                  .Add("+", defPrim "+" Primitives.add)
                  .Add("*", defPrim "*" Primitives.mul)
                  .Add("car", defPrim "car" Primitives.car)
                  .Add("cdr", defPrim "cdr" Primitives.cdr)
                  .Add("quote", defForm "quote" SpecialForms.quote)
          Lisp.Lexical = Map.empty }

    /// lookup: retrieve the value bound to a symbol in the current scope.
    let lookup (sym: Lisp.Symbol) (scope: Lisp.Scope): Lisp.Sexpr =
        match (scope.Dynamic.TryFind(sym)) with
            | Some (sexpr :: _) -> sexpr
            | _ ->
            match (scope.Lexical.TryFind sym) with
                | Some sexpr -> sexpr
                | None ->
                match (scope.Global.TryFind sym) with
                    | Some sexpr -> sexpr
                    | None -> Lisp.Atom Lisp.Nil

    let internal evalSexpr (scope : Lisp.Scope) (sexpr : Lisp.Sexpr): Lisp.EvalStack * Lisp.DataStack =
        match sexpr with
            | Lisp.Atom (Lisp.Symbol "globe") ->
                [Lisp.EvalSpecialForm({ Lisp.SpecialForm.Name = "globe"
                                        Lisp.SpecialForm.Func = SpecialForms.globe }, 0)]
                , []
            | Lisp.Atom (Lisp.Symbol "scope") ->
                [Lisp.EvalSpecialForm({ Lisp.SpecialForm.Name = "scope"
                                        Lisp.SpecialForm.Func = SpecialForms.scope }, 0)]
                , []
            | Lisp.Atom (Lisp.Symbol s) ->
                [], [lookup s scope]
            | Lisp.Pair (car, cdr) ->
                [ Lisp.EvalSexpr car
                  Lisp.EvalTop (1, scope) ]
                , [cdr]
            | Lisp.Sexpr (car :: cdr) ->
                [ Lisp.EvalSexpr car
                  Lisp.EvalTop (List.length cdr, scope) ]
                , cdr
            | _ -> [], [sexpr]

    let internal evalInstruction (scope : Lisp.Scope) (instr : Lisp.Instruction) (data : Lisp.DataStack): Lisp.Scope * Lisp.EvalStack * Lisp.DataStack =
        match instr with
            | Lisp.EvalSexpr sexpr ->
                let (newInstr, newData) = evalSexpr scope sexpr
                scope, newInstr, List.append newData data
            | Lisp.EvalSpecialForm (form, nargs) ->
                let (args, rest) = List.splitAt nargs data
                let (scope, newInstr, newData) = form.Func scope args
                scope, newInstr, List.append newData rest
            | Lisp.EvalPrimitive (prim, nargs) ->
                let (args, rest) = List.splitAt nargs data
                let result = prim.Func args
                scope, [], result::rest
            | Lisp.EvalTop (nargs, scope) ->
                match List.tryHead data with
                    | Some (Lisp.Atom (Lisp.Primitive p)) ->
                        let (args, rest) = List.splitAt nargs data.Tail
                        let instr = List.fold (fun acc it -> (Lisp.EvalSexpr it)::acc) [Lisp.EvalPrimitive (p, nargs)] args
                        scope, instr, rest
                    | Some (Lisp.Atom (Lisp.SpecialForm f)) ->
                        scope, [Lisp.EvalSpecialForm(f, nargs)], data.Tail
                    | Some i ->
                        scope, [], [Lisp.Atom (Lisp.Error $"{i} is not a primitive nor a special form")]
                    | _ ->
                        scope, [], [Lisp.Atom(Lisp.Error "eval called on an empty stack")]

    /// Eval: eval SEXPR in SCOPE.
    /// Use stacks of instructions and values to be tail recursive.
    let Eval (scope: Lisp.Scope) (sexpr: Lisp.Sexpr): Lisp.Sexpr =
        let rec loop (scope : Lisp.Scope) (instructions : Lisp.EvalStack) (data : Lisp.DataStack) : Lisp.Sexpr =
            match instructions with
                | [] -> match data with
                        | result::_ -> result
                        | [] -> Lisp.Atom Lisp.Nil
                | instr::rest ->
                    let (newScope, newInstructions, newData) = evalInstruction scope instr data
                    loop newScope (List.append newInstructions rest) newData
        loop scope [Lisp.EvalSexpr sexpr] []