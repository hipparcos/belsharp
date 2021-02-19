module Tests.Lexer

open Library
open NUnit.Framework

[<SetUp>]
let Setup () =
    ()

[<Test>]
let it_should_accept_empty_input () =
    Assert.That(
        Lexer.Lex "",
        Is.EqualTo(seq {
            Lexer.EOF
        })
    )

[<Test>]
let it_should_skip_whitespaces () =
    Assert.That(
        Lexer.Lex "  \t\n\r\v",
        Is.EqualTo(seq {
            Lexer.EOF
        })
    )

[<Test>]
let it_should_lex_numbers () =
    Assert.That(
        Lexer.Lex "1 10 100",
        Is.EqualTo(seq {
            Lexer.Number 1;
            Lexer.Number 10;
            Lexer.Number 100;
            Lexer.EOF
        })
    )

[<Test>]
let it_should_lex_symbols () =
    Assert.That(
        Lexer.Lex "+ symbol i-am-a-symbol symbol0",
        Is.EqualTo(seq {
            Lexer.Symbol "+";
            Lexer.Symbol "symbol";
            Lexer.Symbol "i-am-a-symbol";
            Lexer.Symbol "symbol0";
            Lexer.EOF
        })
    )

[<Test>]
let it_should_lex_parenthesis () =
    Assert.That(
        Lexer.Lex "())()",
        Is.EqualTo(seq {
            Lexer.ParenL
            Lexer.ParenR
            Lexer.ParenR
            Lexer.ParenL
            Lexer.ParenR
            Lexer.EOF
        })
    )
