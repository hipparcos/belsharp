namespace Library

/// Lisp data structures.
module Lisp =

    /// Symbol: it's just a string really.
    type Symbol = string

    /// Atom: the leaf of S-expressions.
    type Atom =
        // Readable:
        | Nil
        | Number of int
        | Symbol of Symbol
        // Not readable:
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

    /// Environment: a collection of bindings.
    and Environment = Map<Symbol, Sexpr>

    /// Bindings: a collection of stacks of bindings for dynamic variables.
    and Bindings = Map<Symbol, Sexpr list>

    /// Scope: a set of bindings.
    and Scope =
        { Dynamic: Bindings
          Global: Environment
          Lexical: Environment }

    /// Instruction: the instructions of the VM.
    /// Declared here because of cyclic dependencies.
    and Instruction =
        | EvalSexpr of Sexpr
        | EvalTop of nargs:int * Scope
        | EvalPrimitive of Primitive * nargs:int
        | EvalSpecialForm of SpecialForm * nargs:int

    and DataStack = Sexpr list

    and EvalStack = Instruction list

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