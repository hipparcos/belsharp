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

    /// SexprAction: operations on the AST.
    type internal SexprAction =
        | CollectSexpr of Sexpr
        | PopThenCollectSexpr of Sexpr
        | PushList
        | PreserveStack

    and internal ParserResult = Result<SexprAction * ParsingStateAction list, string>

    /// Parser: parsing state implementation.
    and internal Parser = Lexer.Token -> Sexpr list -> ParserResult

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
            | Lexer.EOF -> Ok (PreserveStack, [PopState])
            | _ -> Error "trailing objects, EOF expected"

    // ParsePairRightParen: parse the right parenthesis of a dotted pair.
    // Transition from ParsePairCDR.
    let internal ParsePairRightParen tok acc =
        match tok with
            | Lexer.EOF -> Error "unclosed pair"
            | Lexer.ParenL -> Ok (PushList, [PushState ParsingList])
            | Lexer.ParenR ->
                match acc with
                    | [f; s] -> Ok (PopThenCollectSexpr (Pair (s, f)), [PopState])
                    | _ -> Error "malformed pair"
            | _ -> Error "trailing object in pair"

    // ParsePairCDR: parse the CDR of a pair.
    // Transition from dot symbol in 2nd position of a list.
    let internal ParsePairCDR tok acc =
        match tok with
            | Lexer.EOF -> Error "unclosed pair"
            | Lexer.ParenR -> Error "malformed pair"
            | Lexer.ParenL ->
                Ok (PushList, [PopState; PushState ParsingPairRightParen; PushState ParsingList])
            | Lexer.Number _ | Lexer.Symbol _ ->
                let atom = ParseAtom tok
                match atom with
                    | Ok a ->
                        Ok (CollectSexpr (Atom a), [PopState; PushState ParsingPairRightParen])
                    | Error err -> Error err

    // ParseList: parse a proper list, may transition to ParsePairCDR.
    // Transition from left parenthesis.
    let internal ParseList tok acc =
        match tok with
            | Lexer.EOF -> Error "unclosed pair"
            | Lexer.ParenR ->
                match acc with
                    | [] -> Ok (PopThenCollectSexpr (Atom Nil), [PopState])
                    | _ -> Ok (PopThenCollectSexpr (Sexpr (List.rev ((Atom Nil)::acc))), [PopState])
            | Lexer.ParenL -> Ok (PushList, [PushState ParsingList])
            | Lexer.Symbol s when s = "."  ->
                match acc with
                    | [ _ ] -> Ok (PreserveStack, [PopState; PushState ParsingPairCDR])
                    | _ -> Ok (CollectSexpr (Atom (Symbol ".")), [KeepState])
            | Lexer.Number _ | Lexer.Symbol _ ->
                let atom = ParseAtom tok
                match atom with
                    | Ok a -> Ok (CollectSexpr (Atom a), [KeepState])
                    | Error err -> Error err

    // ParseTopLevel: initial parsing state.
    let internal ParseTopLevel tok acc =
        match tok with
            | Lexer.EOF -> Ok (PreserveStack, [PopState; PushState ParsingEOF])
            | Lexer.ParenR -> Error "unexpected closing parenthesis"
            | Lexer.ParenL -> Ok (PushList, [PushState ParsingList])
            | Lexer.Number _ | Lexer.Symbol _ ->
                match (ParseAtom tok) with
                    | Ok a -> Ok (CollectSexpr (Atom a), [PopState; PushState ParsingEOF])
                    | Error err -> Error err

    let internal ParsingStateToParser (s : ParsingState) : Parser =
        match s with
            | ParsingEOF -> ParseEOF
            | ParsingList -> ParseList
            | ParsingPairRightParen -> ParsePairRightParen
            | ParsingPairCDR -> ParsePairCDR
            | ParsingTopLevel -> ParseTopLevel

    // updateParsingStates: apply actions on states.
    // Allow PushListInPair to return a 2 actions.
    let rec internal updateParsingStates (actions : ParsingStateAction list) (states : ParsingState list) =
        if actions.IsEmpty then states
        else
            match actions.Head with
                | KeepState -> updateParsingStates actions.Tail states
                | PopState -> updateParsingStates actions.Tail states.Tail
                | PushState s -> updateParsingStates actions.Tail (s::states)

    let internal updateSexprStack (action : SexprAction) (stack : list<Sexpr list>) : list<Sexpr list> =
        match action with
            | CollectSexpr sexpr -> (sexpr::stack.Head)::stack.Tail
            | PopThenCollectSexpr sexpr -> (sexpr::stack.Tail.Head)::stack.Tail.Tail
            | PushList -> []::stack
            | PreserveStack -> stack

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
                            | Ok (sexprAction, stateActions) ->
                                loop (Seq.tail tokens)
                                     (updateParsingStates stateActions states)
                                     (updateSexprStack sexprAction acc)
        if Seq.isEmpty tokens then Error "empty stream of tokens"
        else loop tokens [ParsingTopLevel; ParsingEOF] [[]]
