namespace Library

/// Lisp data structures.
module Lisp =

    /// Symbol: it's just a string really.
    type Symbol = Sym of string

    /// Atom: the leaf of S-expressions.
    type Atom =
        // Readable:
        | Nil
        | Number of int
        | Symbol of Symbol
        // Not readable:
        | Function of Function
        | Macro of Function
        | Primitive of Primitive
        | SpecialForm of SpecialForm
        | Error of string

    /// Sexpr: lists, pairs the node of S-expressions and their atoms.
    /// Lists are represented as F# lists instead of cons Pair so
    /// F# list functions can be used.
    and Sexpr =
        | Atom of Atom
        | Pair of Sexpr * Sexpr
        | Sexpr of Sexpr list

    /// Environment: a set of bindings.
    and Environment = Map<Symbol, Sexpr>

    and Dynamic = Dynamic of Environment * option<Dynamic ref>
    and Global = Global of Environment
    and Lexical = Lexical of Environment * option<Lexical ref>

    and Scope =
        { Dynamic: Dynamic ref
          Global: Global ref
          Lexical: Lexical ref }

    /// Instruction: the instructions of the VM.
    /// Declared here because of cyclic dependencies.
    and Instruction =
        | EvalSexpr of Sexpr * Lexical ref
        | EvalTop of nargs:int * Lexical ref
        | CallFunction of Function * nargs:int
        | CallPrimitive of Primitive * nargs:int
        | CallSpecialForm of SpecialForm * nargs:int * Lexical ref

    and DataStack = Sexpr list

    and EvalStack = Instruction list

    and Function =
        { Environment: Lexical ref
          Parameters: Symbol list
          Body: Sexpr }

        member this.parameterList =
            List.map (Symbol >> Atom) this.Parameters
            |> Sexpr

    and PrimitiveName = string

    and PrimitiveFunc = DataStack -> Sexpr

    /// Primitive: functions that evaluate as per function evaluation
    /// rules (left to right, depth first) but can not be defined in
    /// Bel itself.    
    and [<CustomEquality; NoComparison>] Primitive =
        { Name: PrimitiveName
          Func: PrimitiveFunc }
        
        override x.Equals(y) =
            match y with
                | :? Primitive as y -> x.Name = y.Name
                | _ -> false

        override x.GetHashCode() = hash (x.Name)
        
    and SpecialFormName = string
    
    and SpecialFormFunc = Scope -> DataStack -> SpecialFormResult

    and SpecialFormResult = Scope * EvalStack * DataStack
    
    /// SpecialForm: forms that does not evaluate as per function
    /// evaluation rules. An example is the `if` form which does not
    /// evaluate all of its branches.
    and [<CustomEquality; NoComparison>] SpecialForm =
        { Name: SpecialFormName
          Func: SpecialFormFunc }
        
        override x.Equals(y) =
            match y with
                | :? SpecialForm as y -> x.Name = y.Name
                | _ -> false

        override x.GetHashCode() = hash (x.Name)

    let environmentToAList (env : Environment) : Sexpr list =
        Map.fold (fun acc s v ->
                      (Pair (Symbol s |> Atom, v))::acc)
                 [] env

    let alistToEnvironment (env : Sexpr list) : Environment =
        List.fold (fun env it -> match it with
                                 | Pair (Atom (Symbol s), v) -> env.Add(s, v)
                                 | _ -> env)
                  Map.empty env

    let internal globeToAlist (Global g) = (environmentToAList >> Sexpr) g

    let rec internal scopeToAlist env =
        let rec toAlist (Lexical (l,prev)) =
            List.append (environmentToAList l)
                        (match prev with
                         | Some p -> toAlist !p
                         | None -> [])
        toAlist env |> Sexpr