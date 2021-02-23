namespace Library

open System

// Parser is a tail recursive parser implementation.
// The public interface is Parse.
// The output is a S-expression thus a Lisp data structure.
module Parser =

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
        | CollectSexpr of Lisp.Sexpr
        | PopThenCollectSexpr of Lisp.Sexpr
        | PushList
        | PreserveSexprStack

    /// ParserResult: alias to simplify the following signature.
    and internal ParserResult = Result<SexprAction * ParsingStateAction list, string>

    /// Parser: parsing state implementation.
    and internal Parser = Lexer.Token -> Lisp.Sexpr list -> ParserResult

    /// ParsingResult: result of Parse, used to explicitly cast in tests.
    type ParsingResult = Result<Lisp.Sexpr, string>

    /// ResultToString: for debugging.
    let ResultToString = function
        | Ok sexpr -> Lisp.SexprToString sexpr
        | Error err -> err

    // ParserAtom: parse a single atom, not a parsing state.
    let internal ParseAtom = function
        | Lexer.Number n -> Ok (Lisp.Number n)
        | Lexer.Symbol s when s = "nil" -> Ok Lisp.Nil
        | Lexer.Symbol s -> Ok (Lisp.Symbol s)
        | _ -> Error "not an atom"

    // ParseEOF: terminal parsing state.
    let internal ParseEOF tok acc =
        match tok with
            | Lexer.EOF -> Ok (PreserveSexprStack, [PopState])
            | _ -> Error "trailing objects, EOF expected"

    // ParsePairRightParen: parse the right parenthesis of a dotted pair.
    // Transition from ParsePairCDR.
    let internal ParsePairRightParen tok acc =
        match tok with
            | Lexer.EOF -> Error "unclosed pair"
            | Lexer.ParenL -> Ok (PushList, [PushState ParsingList])
            | Lexer.ParenR ->
                match acc with
                    | [f; s] -> Ok (PopThenCollectSexpr (Lisp.Pair (s, f)), [PopState])
                    | _ -> Error "malformed pair"
            | _ -> Error "trailing object in pair"

    // ParsePairCDR: parse the CDR of a pair.
    // Transition from dot symbol in 2nd position of a list.
    let internal ParsePairCDR tok acc =
        match tok with
            | Lexer.EOF -> Error "unclosed pair"
            | Lexer.ParenR -> Error "malformed pair"
            | Lexer.ParenL ->
                Ok (PushList,
                    [PopState; PushState ParsingPairRightParen; PushState ParsingList])
            | Lexer.Number _ | Lexer.Symbol _ ->
                let atom = ParseAtom tok
                match atom with
                    | Ok a ->
                        Ok (CollectSexpr (Lisp.Atom a),
                            [PopState; PushState ParsingPairRightParen])
                    | Error err -> Error err

    // ParseList: parse a proper list, may transition to ParsePairCDR.
    // Transition from left parenthesis.
    let internal ParseList tok acc =
        match tok with
            | Lexer.EOF -> Error "unclosed pair"
            | Lexer.ParenR ->
                match acc with
                    | [] -> Ok (PopThenCollectSexpr (Lisp.Atom Lisp.Nil), [PopState])
                    | _ -> Ok (PopThenCollectSexpr (Lisp.Sexpr (List.rev ((Lisp.Atom Lisp.Nil)::acc))), [PopState])
            | Lexer.ParenL -> Ok (PushList, [PushState ParsingList])
            | Lexer.Symbol s when s = "."  ->
                match acc with
                    | [ _ ] -> Ok (PreserveSexprStack, [PopState; PushState ParsingPairCDR])
                    | _ -> Ok (CollectSexpr (Lisp.Atom (Lisp.Symbol ".")), [KeepState])
            | Lexer.Number _ | Lexer.Symbol _ ->
                let atom = ParseAtom tok
                match atom with
                    | Ok a -> Ok (CollectSexpr (Lisp.Atom a), [KeepState])
                    | Error err -> Error err

    // ParseTopLevel: initial parsing state.
    let internal ParseTopLevel tok acc =
        match tok with
            | Lexer.EOF -> Ok (PreserveSexprStack, [PopState; PushState ParsingEOF])
            | Lexer.ParenR -> Error "unexpected closing parenthesis"
            | Lexer.ParenL -> Ok (PushList, [PushState ParsingList])
            | Lexer.Number _ | Lexer.Symbol _ ->
                match (ParseAtom tok) with
                    | Ok a -> Ok (CollectSexpr (Lisp.Atom a), [PopState; PushState ParsingEOF])
                    | Error err -> Error err

    // ParsingStateToParser: pipe a ParsingState to its Parser.
    let internal ParsingStateToParser (s : ParsingState) : Parser =
        match s with
            | ParsingEOF -> ParseEOF
            | ParsingList -> ParseList
            | ParsingPairRightParen -> ParsePairRightParen
            | ParsingPairCDR -> ParsePairCDR
            | ParsingTopLevel -> ParseTopLevel

    /// Parse: parse stream of TOKENS, return a Sexpr.
    /// This is a tail recursive function.
    /// TOKENS are parsed one at a time by the current ParsingState.
    /// A parsing operation may require an operation on a stack of
    /// Sexpr accumulators and multiple operations on the stack of
    /// parsing states.
    /// The depth of the Sexpr accumulator stack represents the level
    /// of nesting of the current list/pair being parsed.
    /// Operations on the stack of ParsingStates represents transitions
    /// between ParsingState. The depth of this stack is linked to the
    /// level of current nesting of the list/pair being parsed.
    let Parse (tokens : seq<Lexer.Token>) : ParsingResult =
        // Each time a Parser is called, it can decide what the next
        // state should be. Multiple operations can be chained as
        // replacing the current state requires to pop the stack then
        // to push a new state.
        let rec updateParsingStateStack (actions : ParsingStateAction list) (states : ParsingState list) =
            if actions.IsEmpty then states
            else
                let action = actions.Head
                updateParsingStateStack actions.Tail
                                        (match action with
                                             | KeepState -> states
                                             | PopState -> states.Tail
                                             | PushState s -> s::states)
        // Each time a Parser is called, it can act on the sexpr accumulator stack.
        let updateSexprListStack (action : SexprAction) (sexprs : list<Lisp.Sexpr list>) =
            match action with
                | CollectSexpr s -> (s::sexprs.Head)::sexprs.Tail
                | PopThenCollectSexpr s -> (s::sexprs.Tail.Head)::sexprs.Tail.Tail
                | PushList -> []::sexprs
                | PreserveSexprStack -> sexprs
        // loop is the tail recursive parsing routine.
        let rec loop tokens (states : ParsingState list) (sexprs : list<Lisp.Sexpr list>) =
            let state = List.head states
            match state with
                | ParsingEOF ->
                    Ok(match sexprs.Head with
                           | [] -> Lisp.Atom Lisp.Nil
                           | l -> l.Head)
                | _ ->
                    if Seq.isEmpty tokens then Error "unexcepted end of stream"
                    else
                        let parser = ParsingStateToParser state
                        let tok = Seq.head tokens
                        match (parser tok sexprs.Head) with
                            | Error err -> Error err
                            | Ok (sexprAction, stateActions) ->
                                loop (Seq.tail tokens)
                                     (updateParsingStateStack stateActions states)
                                     (updateSexprListStack sexprAction sexprs)
        // Bootstrap parsing starting at top-level at depth 1 with no accumulated sexprs.
        if Seq.isEmpty tokens then Error "empty stream of tokens"
        else loop tokens [ParsingTopLevel; ParsingEOF] [[]]
