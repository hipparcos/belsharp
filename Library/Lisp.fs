namespace Library

open System

/// Lisp data structures.
module Lisp =

    /// Symbol: it's just a string really.
    type Symbol = string

    /// Atom: the leaf of S-expressions.
    type Atom =
        | Nil
        | Number of int
        | Symbol of Symbol

    /// Sexpr: lists, pairs the node of S-expressions and their atoms.
    /// Lists are represented as F# lists instead of cons Pair so
    /// F# list functions can be used.
    type Sexpr =
        | Atom of Atom
        | Pair of Sexpr * Sexpr
        | Sexpr of Sexpr list

    /// Value: a Bel value in the VM.
    type Value =
        | Sexpr of Sexpr
        | Primitive of Primitive
        | SpecialForm of SpecialForm
        | Error of string

    /// Environment: a collection of bindings.
    and Environment = Map<Symbol, Value>

    /// Bindings: a collection of stacks of bindings for dynamic variables.
    and Bindings = Map<Symbol, Value list>

    /// Scope: a set of bindings.
    and Scope =
        { Dynamic: Bindings
          Global: Environment
          Lexical: Environment }

    /// Primitive: functions that evaluate as per function evaluation
    /// rules (left to right, depth first) but can not be defined in
    /// Bel itself.
    and Primitive = Value list -> int -> Value list

    /// ValueStack: a stack of Values.
    and ValueStack = Value list

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
          mutable Data: ValueStack
          mutable Scope: Scope }

        member this.PushInstr (v : Instruction) : Context =
            this.Instr <- v::this.Instr
            this
        member this.PopInstr() : Instruction option =
            match this.Instr with
                | v::rest -> this.Instr <- rest; Some v
                | _ -> None

        member this.PushData (v : Value) : Context =
            this.Data <- v::this.Data
            this
        member this.PopData() : Value option =
            match this.Data with
                | v::rest -> this.Data <- rest; Some v
                | _ -> None

    /// SpecialForm: forms that does not evaluate as per function
    /// evaluation rules. An example is the `if` form which does not
    /// evaluate all of its branches.
    and SpecialForm = Context -> int -> unit
