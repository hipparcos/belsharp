namespace Library

open System

/// Evaluator provides the eval function.
module Evaluator =

    let defPrim n p =
          (n, p) |> (Lisp.Prim >> Lisp.Primitive >> Lisp.Atom)

    let defForm n f =
        (n, f) |> (Lisp.Form >> Lisp.SpecialForm >> Lisp.Atom)

    /// DefaultScope: a scope with primitives and special forms defined
    /// in the global environment.
    let DefaultScope =
        { Lisp.Dynamic = Map.empty
          Lisp.Global = Map.empty.
              Add("+", defPrim "+" Primitives.add).
              Add("*", defPrim "*" Primitives.mul).
              Add("car", defPrim "car" Primitives.car).
              Add("cdr", defPrim "cdr" Primitives.cdr).
              Add("quote", defForm "quote" SpecialForms.quote);
          Lisp.Lexical = Map.empty }

    /// popDataPushInstr: pop value from data stack, push to instr stack.
    let rec internal popDataPushInstr (ctx : Lisp.Context) n : unit =
        if n > 0 then
            let instr = match (ctx.PopData()) with
                            | Some v -> v
                            | _ -> Lisp.Atom Lisp.Nil
            popDataPushInstr (ctx.PushInstr(Lisp.EvalSexpr instr)) (n - 1)

    /// lookup: retrieve the value bound to a symbol in the current scope.
    let lookup (sym : Lisp.Symbol) (scope : Lisp.Scope) : Lisp.Sexpr =
        match (scope.Dynamic.TryFind(sym)) with
            | Some (sexpr::_) -> sexpr
            | _ -> match (scope.Lexical.TryFind sym) with
                       | Some sexpr -> sexpr
                       | None -> match (scope.Global.TryFind sym) with
                                     | Some sexpr -> sexpr
                                     | None -> Lisp.Atom Lisp.Nil

    let internal evalPrimitive (prim : Lisp.Primitive) (stack : Lisp.SexprStack) (nargs : int) : Lisp.SexprStack =
        let (args, rest) = List.splitAt nargs stack
        let result = (Lisp.primitiveFun prim) args nargs
        result::rest

    /// Eval: eval SEXPR in SCOPE.
    /// Use stacks of instructions and values to be tail recursive.
    let Eval (scope : Lisp.Scope) (sexpr : Lisp.Sexpr) : Lisp.Sexpr =
        let context = { Lisp.Instr = [] ; Lisp.Data = [] ; Lisp.Scope = scope }
        let rec loop (context : Lisp.Context) : Lisp.Sexpr =
            match context.PopInstr() with
                | None -> match context.PopData() with
                              | Some v -> v
                              | None -> Lisp.Atom Lisp.Nil
                | Some (Lisp.EvalSpecialForm (form, nargs)) ->
                    (Lisp.specialFromFun form) context nargs
                    loop context
                | Some (Lisp.EvalPrimitive (prim, nargs)) ->
                    context.Data <- evalPrimitive prim context.Data nargs
                    loop context
                | Some (Lisp.EvalTop nargs) ->
                    let instr = context.PopData()
                    match instr with
                        | Some (Lisp.Atom (Lisp.Primitive p)) ->
                            context.PushInstr (Lisp.EvalPrimitive (p, nargs)) |> ignore
                            popDataPushInstr context nargs
                            loop context
                        | Some (Lisp.Atom (Lisp.SpecialForm f)) ->
                            context.PushInstr (Lisp.EvalSpecialForm (f, nargs)) |> ignore
                            loop context
                        | Some i -> Lisp.Atom (Lisp.Error $"{i} is not a primitive nor a special form")
                        | _ -> Lisp.Atom (Lisp.Error "eval called on an empty stack")
                | Some (Lisp.EvalSexpr sexpr) ->
                    match sexpr with
                        | Lisp.Atom (Lisp.Symbol "globe") ->
                            context.PushInstr (Lisp.EvalSpecialForm (Lisp.Form ("globe", SpecialForms.globe), 0)) |> ignore
                        | Lisp.Atom (Lisp.Symbol "scope") ->
                            context.PushInstr (Lisp.EvalSpecialForm (Lisp.Form ("scope", SpecialForms.scope), 0)) |> ignore
                        | Lisp.Atom (Lisp.Symbol s) ->
                            context.PushData(lookup s context.Scope) |> ignore
                        | Lisp.Pair (car, cdr) ->
                            context.PushData cdr |> ignore
                            context.PushInstr (Lisp.EvalTop 1) |> ignore
                            context.PushInstr (Lisp.EvalSexpr car) |> ignore
                        | Lisp.Sexpr (car::cdr) ->
                            let mutable nargs = 0
                            for it in cdr do
                                context.PushData it |> ignore
                                nargs <- nargs + 1
                            context.PushInstr (Lisp.EvalTop nargs) |> ignore
                            context.PushInstr (Lisp.EvalSexpr car) |> ignore
                        | _ ->
                            context.PushData sexpr |> ignore
                    loop context
        loop (context.PushInstr(Lisp.EvalSexpr sexpr))
