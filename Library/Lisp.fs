namespace Library

/// Lisp data structures.
module Lisp =

    /// Symbol: it's just a string really.
    type Symbol = Sym of string

    /// Atom: the leaf of S-expressions.
    [<CustomEquality;CustomComparison>]
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
        override x.Equals(y) =
            match y with
            | :? Atom as y ->
                match x,y with
                | Nil, Nil -> true
                | Number x, Number y -> x = y
                | Symbol x, Symbol y -> x = y
                | Function x, Function y -> x = y
                | Macro x, Macro y -> x = y
                | Primitive x, Primitive y -> x = y
                | SpecialForm x, SpecialForm y -> x = y
                | Error x, Error y -> x = y
                | _ -> false
            | _ -> invalidArg "y" "cannot compare value of different types"

        override x.GetHashCode() = hash x

        interface System.IComparable with
            member x.CompareTo y =
                match y with
                | :? Atom as y ->
                    match x,y with
                    | Nil, Nil -> 0
                    | Number x, Number y -> compare x y
                    | Symbol x, Symbol y -> compare x y
                    | _ -> -1
                | _ -> invalidArg "y" "cannot compare value of different types"

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
          Global: Global
          Lexical: Lexical ref }

    /// Instruction: the instructions of the VM.
    /// Declared here because of cyclic dependencies.
    and Instruction =
        | EvalSexpr of Sexpr * Dynamic ref * Lexical ref
        | EvalTop of nargs:int * Dynamic ref * Lexical ref
        | CallFunction of Function * nargs:int * Dynamic ref
        | CallMacro of Function * nargs:int * Dynamic ref
        | CallPrimitive of Primitive * nargs:int
        | CallSpecialForm of SpecialForm * nargs:int * Dynamic ref * Lexical ref

    and DataStack = Sexpr list

    and EvalStack = Instruction list

    and FunctionParameters =
        | ListOfSymbols of Symbol list
        | SingleList of Symbol
    and Function =
        { Environment: Lexical ref
          Parameters: FunctionParameters
          Body: Sexpr }

        member this.parameterList =
            match this.Parameters with
            | ListOfSymbols symbols ->
                List.map (Symbol >> Atom) symbols
                |> Sexpr
            | SingleList symbol -> Atom (Symbol symbol)

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

    and SpecialFormResult = Global * EvalStack * DataStack
    
    /// SpecialForm: forms that does not evaluate as per function
    /// evaluation rules. An example is the `if` form which does not
    /// evaluate all of its branches.
    and [<CustomEquality; NoComparison>] SpecialForm =
        { Name: SpecialFormName
          Func: SpecialFormFunc
          EvalArgs: bool }
        
        override x.Equals(y) =
            match y with
                | :? SpecialForm as y -> x.Name = y.Name
                | _ -> false

        override x.GetHashCode() = hash (x.Name)

    let t = Atom (Symbol (Sym "t"))
    let nil = Atom Nil

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