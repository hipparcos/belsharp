namespace Library

open System

/// Lisp data structures.
module Lisp =

    /// Symbol: it's just a string really.
    type Symbol = string

    /// Atom: the leaf of S-expressions.
    [<CustomEquality; CustomComparison>]
    type Atom =
        // Readable:
        | Nil
        | Number of int
        | Symbol of Symbol
        // Not readable:
        | Primitive of Primitive
        | SpecialForm of SpecialForm
        | Error of string

        override x.Equals(y) =
            match y with
                | :? Atom as y ->
                    match (x,y) with
                        | (Nil, Nil) -> true
                        | (Number x, Number y) -> x = y
                        | (Symbol x, Symbol y) -> x = y
                        | _ -> false
                | _ -> false

        override x.GetHashCode() = hash (x)

        interface IComparable with
            member x.CompareTo(y) =
                match y with
                    | :? Atom as y ->
                        match (x,y) with
                            | (Nil, Nil) -> 0
                            | (Number x, Number y) -> compare x y
                            | (Symbol x, Symbol y) -> compare x y
                            | _ -> -1
                    | _ -> failwith "can not compare different types"

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

    /// Primitive: functions that evaluate as per function evaluation
    /// rules (left to right, depth first) but can not be defined in
    /// Bel itself.
    and Primitive = Prim of string * (Sexpr list -> int -> Sexpr)

    /// SexprStack: a stack of Sexprs.
    and SexprStack = Sexpr list

    /// Instruction: the instructions of the VM.
    /// Declared here because of cyclic dependencies.
    and Instruction =
        | EvalSexpr of Sexpr
        | EvalTop of int
        | EvalPrimitive of Primitive * int
        | EvalSpecialForm of SpecialForm * int

    /// InstructionStach: a stack of instructions.
    and InstructionStack = Instruction list

    /// Context: a context of evaluation.
    and Context =
        { mutable Instr: InstructionStack
          mutable Data: SexprStack
          mutable Scope: Scope }

        member this.PushInstr (v : Instruction) : Context =
            this.Instr <- v::this.Instr
            this
        member this.PopInstr() : Instruction option =
            match this.Instr with
                | v::rest -> this.Instr <- rest; Some v
                | _ -> None

        member this.PushData (v : Sexpr) : Context =
            this.Data <- v::this.Data
            this
        member this.PopData() : Sexpr option =
            match this.Data with
                | v::rest -> this.Data <- rest; Some v
                | _ -> None

    /// SpecialForm: forms that does not evaluate as per function
    /// evaluation rules. An example is the `if` form which does not
    /// evaluate all of its branches.
    and SpecialForm = Form of string * (Context -> int -> unit)

    let primitiveFun (Prim (_, p)) = p
    let specialFromFun (Form (_, f)) = f
