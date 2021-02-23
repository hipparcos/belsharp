namespace Library

open System

// Parser is a tail recursive parser implementation.
// The public interface is Atom, Sexpr and Parse.
module Parser =

    /// Atom: the leaf of the AST.
    type Atom =
        | Nil
        | Number of int
        | Symbol of string

    /// Sexpr: the nodes of the AST.
    type Sexpr =
        | Atom of Atom
        | Pair of Sexpr * Sexpr
        | Sexpr of Sexpr list

    /// State: the parsing state.
    type internal ParsingState =
        | ParsingEOF
        | ParsingList
        | ParsingPairRightParen
        | ParsingPairCDR
        | ParsingTopLevel

    /// ParsingStateAction: operations on the stack of states.
    and internal ParsingStateAction =
        | KeepState
        | PopState
        | PushState of ParsingState

    /// SexprAction: operations on the AST, associated to state changes.
    and internal SexprAction =
        | AccumulateSexpr of Sexpr * ParsingStateAction list
        | FillupSexpr of Sexpr
        | NestSexpr of ParsingStateAction list
        | None of ParsingStateAction list

    /// Parser: parsing state implementation.
    and internal Parser = Lexer.Token -> Sexpr list -> Result<SexprAction, string>

    /// ParsingResult: result of Parse, used to explicitly cast in tests.
    type ParsingResult = Result<Sexpr, string>

    let AtomToString = function
        | Nil -> "nil"
        | Number n -> string n
        | Symbol s -> s

    // SexprRoString: for debugging, will be implemented by the printer.
    let rec SexprToString sexpr =
        let concat l = String.concat " " (l
                                          |> List.rev
                                          |> List.tail
                                          |> List.rev
                                          |> List.map SexprToString)
        match sexpr with
            | Atom a -> AtomToString a
            | Pair (car, cdr) -> $"( {SexprToString car} . {SexprToString cdr} )"
            | Sexpr l -> $"( {concat l} )"

    let ResultToString = function
        | Ok sexpr -> SexprToString sexpr
        | Error err -> err

    let internal ParseAtom = function
        | Lexer.Number n -> Ok (Number n)
        | Lexer.Symbol s when s = "nil" -> Ok Nil
        | Lexer.Symbol s -> Ok (Symbol s)
        | _ -> Error "not an atom"

    // ParseEOF: terminal parsing state.
    let internal ParseEOF tok acc =
        match tok with
            | Lexer.EOF -> Ok (None [PopState])
            | _ -> Error "trailing objects, EOF expected"

    // ParsePairRightParen: parse the right parenthesis of a dotted pair.
    // Transition from ParsePairCDR.
    let internal ParsePairRightParen tok acc =
        match tok with
            | Lexer.EOF -> Error "unclosed pair"
            | Lexer.ParenL -> Ok (NestSexpr [PushState ParsingList])
            | Lexer.ParenR ->
                match acc with
                    | [f; s] -> Ok (FillupSexpr (Pair (s, f)))
                    | _ -> Error "malformed pair"
            | _ -> Error "trailing object in pair"

    // ParsePairCDR: parse the CDR of a pair.
    // Transition from dot symbol in 2nd position of a list.
    let internal ParsePairCDR tok acc =
        match tok with
            | Lexer.EOF -> Error "unclosed pair"
            | Lexer.ParenR -> Error "malformed pair"
            | Lexer.ParenL -> Ok (NestSexpr [PopState; PushState ParsingPairRightParen; PushState ParsingList])
            | Lexer.Number _ | Lexer.Symbol _ ->
                let atom = ParseAtom tok
                match atom with
                    | Ok a ->
                        Ok (AccumulateSexpr (Atom a, [PopState; PushState ParsingPairRightParen]))
                    | Error err -> Error err

    // ParseList: parse a proper list, may transition to ParsePairCDR.
    // Transition from left parenthesis.
    let internal ParseList tok acc =
        match tok with
            | Lexer.EOF -> Error "unclosed pair"
            | Lexer.ParenR ->
                match acc with
                    | [] -> Ok (FillupSexpr (Atom Nil))
                    | _ -> Ok (FillupSexpr (Sexpr (List.rev ((Atom Nil)::acc))))
            | Lexer.ParenL -> Ok (NestSexpr [PushState ParsingList])
            | Lexer.Symbol s when s = "."  ->
                match acc with
                    | [ _ ] -> Ok (None [PopState; PushState ParsingPairCDR])
                    | _ -> Ok (AccumulateSexpr (Atom (Symbol "."), [KeepState]))
            | Lexer.Number _ | Lexer.Symbol _ ->
                let atom = ParseAtom tok
                match atom with
                    | Ok a -> Ok (AccumulateSexpr (Atom a, [KeepState]))
                    | Error err -> Error err

    // ParseTopLevel: initial parsing state.
    let internal ParseTopLevel tok acc =
        match tok with
            | Lexer.EOF -> Ok (None [PopState; PushState ParsingEOF])
            | Lexer.ParenR -> Error "unexpected closing parenthesis"
            | Lexer.ParenL -> Ok (NestSexpr [PushState ParsingList])
            | Lexer.Number _ | Lexer.Symbol _ ->
                match (ParseAtom tok) with
                    | Ok a -> Ok (AccumulateSexpr (Atom a, [PopState; PushState ParsingEOF]))
                    | Error err -> Error err

    let internal ParsingStateToParser (s : ParsingState) : Parser =
        match s with
            | ParsingEOF -> ParseEOF
            | ParsingList -> ParseList
            | ParsingPairRightParen -> ParsePairRightParen
            | ParsingPairCDR -> ParsePairCDR
            | ParsingTopLevel -> ParseTopLevel

    // getParsingStateAction: translate SexprAction to action of stack of states.
    let internal getParsingStateAction = function
        | NestSexpr actions -> actions
        | AccumulateSexpr (_, actions) -> actions
        | FillupSexpr _ -> [PopState]
        | None actions -> actions

    // updateParsingStates: apply actions on states.
    // Allow NestSexprInPair to return a 2 actions.
    let rec internal updateParsingStates (actions : ParsingStateAction list) (states : ParsingState list) =
        if actions.IsEmpty then states
        else
            match actions.Head with
                | KeepState -> updateParsingStates actions.Tail states
                | PopState -> updateParsingStates actions.Tail states.Tail
                | PushState s -> updateParsingStates actions.Tail (s::states)

    // Parse: parse stream of TOKENS, return a Sexpr.
    // This is a tail recursive function.
    // It manages the stack of Sexp and the stack of states.
    let Parse (tokens : seq<Lexer.Token>) : ParsingResult =
        let rec loop tokens (states : ParsingState list) (acc : list<Sexpr list>) =
            let state = List.head states
            match state with
                | ParsingEOF -> Ok(match acc.Head with
                                         | [] -> Atom Nil
                                         | l -> l.Head
                                     )
                | _ ->
                    let parser = ParsingStateToParser state
                    if Seq.isEmpty tokens then Error "unexcepted end of stream"
                    else
                        let tok = Seq.head tokens
                        match (parser tok acc.Head) with
                            | Error err -> Error err
                            | Ok action ->
                                loop (Seq.tail tokens)
                                     (updateParsingStates (getParsingStateAction action) states)
                                     (match action with
                                          | AccumulateSexpr (sexpr, _) ->
                                              (sexpr::acc.Head)::acc.Tail
                                          | FillupSexpr sexpr ->
                                              (sexpr::acc.Tail.Head)::acc.Tail.Tail
                                          | NestSexpr _ -> []::acc
                                          | None _ -> acc
                                      )
        if Seq.isEmpty tokens then Error "empty stream of tokens"
        else loop tokens [ParsingTopLevel; ParsingEOF] [[]]
