module Tests.Parser

open Library
open NUnit.Framework

[<SetUp>]
let Setup () =
    ()

[<Test>]
let it_should_return_nil () =
    let got = Parser.Parse (seq {
        Lexer.EOF
    })
    printfn "%s" (Parser.ResultToString got)
    Assert.That(
        got,
        Is.EqualTo(
            Parser.ParsingResult.Ok(Parser.Atom Parser.Nil)
        )
    )

[<Test>]
let it_should_parse_number () =
    let got = Parser.Parse (seq {
        Lexer.Number 100
        Lexer.EOF
    })
    printfn "%s" (Parser.ResultToString got)
    Assert.That(
        got,
        Is.EqualTo(
            Parser.ParsingResult.Ok(Parser.Atom (Parser.Number 100))
        )
    )

[<Test>]
let it_should_parse_symbol () =
    let got = Parser.Parse (seq {
        Lexer.Symbol "symbol"
        Lexer.EOF
    })
    printfn "%s" (Parser.ResultToString got)
    Assert.That(
        got,
        Is.EqualTo(
            Parser.ParsingResult.Ok(Parser.Atom (Parser.Symbol "symbol"))
        )
    )

[<Test>]
let it_should_parse_nil_symbol_as_nil () =
    let got = Parser.Parse (seq {
        Lexer.Symbol "nil"
        Lexer.EOF
    })
    printfn "%s" (Parser.ResultToString got)
    Assert.That(
        got,
        Is.EqualTo(
            Parser.ParsingResult.Ok(Parser.Atom (Parser.Nil))
        )
    )

[<Test>]
let it_should_parse_empty_list_as_nil () =
    let got = Parser.Parse (seq {
        Lexer.ParenL
        Lexer.ParenR
        Lexer.EOF
    })
    printfn "%s" (Parser.ResultToString got)
    Assert.That(
        got,
        Is.EqualTo(
            Parser.ParsingResult.Ok(Parser.Atom (Parser.Nil))
        )
    )

[<Test>]
let it_should_parse_pair () =
    let got = Parser.Parse (seq {
        Lexer.ParenL
        Lexer.Symbol "symbol1"
        Lexer.Symbol "."
        Lexer.Symbol "symbol2"
        Lexer.ParenR
        Lexer.EOF
    })
    printfn "%s" (Parser.ResultToString got)
    Assert.That(
        got,
        Is.EqualTo(
            Parser.ParsingResult.Ok(
                Parser.Pair ((Parser.Atom (Parser.Symbol "symbol1")),
                             (Parser.Atom (Parser.Symbol "symbol2")))
            )
        )
    )

[<Test>]
let it_should_parse_pair_of_dots () =
    let got = Parser.Parse (seq {
        Lexer.ParenL
        Lexer.Symbol "."
        Lexer.Symbol "."
        Lexer.Symbol "."
        Lexer.ParenR
        Lexer.EOF
    })
    printfn "%s" (Parser.ResultToString got)
    Assert.That(
        got,
        Is.EqualTo(
            Parser.ParsingResult.Ok(
                Parser.Pair ((Parser.Atom (Parser.Symbol ".")),
                             (Parser.Atom (Parser.Symbol ".")))
            )
        )
    )

[<Test>]
let it_should_parse_cdr_nested_pairs () =
    let got = Parser.Parse (seq {
        Lexer.ParenL        // ( sym1 . ( sym2 . ( sym3 ) ) )
        Lexer.Symbol "sym1"
        Lexer.Symbol "."
        Lexer.ParenL        // ( sym2 . ( sym3 ) )
        Lexer.Symbol "sym2"
        Lexer.Symbol "."
        Lexer.ParenL        // ( sym3 )
        Lexer.Symbol "sym3"
        Lexer.ParenR
        Lexer.ParenR
        Lexer.ParenR
        Lexer.EOF
    })
    printfn "%s" (Parser.ResultToString got)
    Assert.That(
        got,
        Is.EqualTo(
            Parser.ParsingResult.Ok (
                Parser.Pair ((Parser.Atom (Parser.Symbol "sym1")),
                             (Parser.Pair ((Parser.Atom (Parser.Symbol "sym2")),
                                           (Parser.Sexpr [Parser.Atom (Parser.Symbol "sym3");
                                                          Parser.Atom Parser.Nil]))))
            )
        )
    )

[<Test>]
let it_should_parse_list () =
    let got = Parser.Parse (seq {
        Lexer.ParenL
        Lexer.Symbol "sym1"
        Lexer.Symbol "sym2"
        Lexer.Symbol "sym3"
        Lexer.ParenR
        Lexer.EOF
    })
    printfn "%s" (Parser.ResultToString got)
    Assert.That(
        got,
        Is.EqualTo(
            Parser.ParsingResult.Ok (
                Parser.Sexpr [Parser.Atom (Parser.Symbol "sym1");
                              Parser.Atom (Parser.Symbol "sym2");
                              Parser.Atom (Parser.Symbol "sym3");
                              Parser.Atom Parser.Nil]
            )
        )
    )

[<Test>]
let it_should_parse_car_nested_lists () =
    let got = Parser.Parse (seq {
        Lexer.ParenL        // ( ( ( sym1 ) sym2 ) sym3 )
        Lexer.ParenL        // ( ( sym1 ) sym2 )
        Lexer.ParenL        // ( sym1 )
        Lexer.Symbol "sym1"
        Lexer.ParenR
        Lexer.Symbol "sym2"
        Lexer.ParenR
        Lexer.Symbol "sym3"
        Lexer.ParenR
        Lexer.EOF
    })
    printfn "%s" (Parser.ResultToString got)
    Assert.That(
        got,
        Is.EqualTo(
            Parser.ParsingResult.Ok (
                Parser.Sexpr [Parser.Sexpr [Parser.Sexpr [Parser.Atom (Parser.Symbol "sym1");
                                                          Parser.Atom Parser.Nil];
                                            Parser.Atom (Parser.Symbol "sym2");
                                            Parser.Atom Parser.Nil];
                              Parser.Atom (Parser.Symbol "sym3");
                              Parser.Atom Parser.Nil]
            )
        )
    )

[<Test>]
let it_should_parse_cadr_nested_lists () =
    let got = Parser.Parse (seq {
        Lexer.ParenL        // ( sym1 ( sym2 ( sym3 ) ) )
        Lexer.Symbol "sym1"
        Lexer.ParenL        // ( sym2 ( sym3 ) )
        Lexer.Symbol "sym2"
        Lexer.ParenL        // ( sym3 )
        Lexer.Symbol "sym3"
        Lexer.ParenR
        Lexer.ParenR
        Lexer.ParenR
        Lexer.EOF
    })
    printfn "%s" (Parser.ResultToString got)
    Assert.That(
        got,
        Is.EqualTo(
            Parser.ParsingResult.Ok (
                Parser.Sexpr [Parser.Atom (Parser.Symbol "sym1");
                              Parser.Sexpr [Parser.Atom (Parser.Symbol "sym2");
                                            Parser.Sexpr [Parser.Atom (Parser.Symbol "sym3");
                                                          Parser.Atom Parser.Nil];
                                            Parser.Atom Parser.Nil];
                              Parser.Atom Parser.Nil]
            )
        )
    )

[<Test>]
let it_should_throw_unexpected_closing_parenthesis_error () =
    let got = Parser.Parse (seq {
        Lexer.ParenR
        Lexer.EOF
    })
    printfn "%s" (Parser.ResultToString got)
    Assert.That(
        got,
        Is.EqualTo(
            Parser.ParsingResult.Error "unexpected closing parenthesis"
        )
    )

[<Test>]
let it_should_throw_unclosed_list_error () =
    let got = Parser.Parse (seq {
        Lexer.ParenL
        Lexer.Symbol "symbol"
        Lexer.EOF
    })
    printfn "%s" (Parser.ResultToString got)
    Assert.That(
        got,
        Is.EqualTo(
            Parser.ParsingResult.Error "unclosed pair"
        )
    )

[<Test>]
let it_should_throw_unclosed_pair_error () =
    let got = Parser.Parse (seq {
        Lexer.ParenL
        Lexer.Symbol "symbol"
        Lexer.Symbol "."
        Lexer.EOF
    })
    printfn "%s" (Parser.ResultToString got)
    Assert.That(
        got,
        Is.EqualTo(
            Parser.ParsingResult.Error "unclosed pair"
        )
    )

[<Test>]
let it_should_throw_trailing_element_in_pair () =
    let got = Parser.Parse (seq {
        Lexer.ParenL
        Lexer.Symbol "sym1"
        Lexer.Symbol "."
        Lexer.Symbol "sym2"
        Lexer.Symbol "sym3"
        Lexer.EOF
    })
    printfn "%s" (Parser.ResultToString got)
    Assert.That(
        got,
        Is.EqualTo(
            Parser.ParsingResult.Error "trailing object in pair"
        )
    )
