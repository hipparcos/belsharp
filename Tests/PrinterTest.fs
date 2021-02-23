module Tests.Printer

open Library
open NUnit.Framework

[<SetUp>]
let Setup () =
    ()

[<Test>]
let it_should_print_nil () =
    Assert.That(
        Printer.Print(Lisp.Atom Lisp.Nil),
        Is.EqualTo("nil")
    )

[<Test>]
let it_should_print_number () =
    Assert.That(
        Printer.Print (Lisp.Atom (Lisp.Number 100)),
        Is.EqualTo("100")
    )

[<Test>]
let it_should_print_symbol () =
    Assert.That(
        Printer.Print (Lisp.Atom (Lisp.Symbol "symbol")),
        Is.EqualTo("symbol")
    )

[<Test>]
let it_should_print_empty_list_as_nil () =
    Assert.That(
        Printer.Print (Lisp.Sexpr [Lisp.Atom Lisp.Nil]),
        Is.EqualTo("nil")
    )

[<Test>]
let it_should_print_pair () =
    Assert.That(
        Printer.Print (
            Lisp.Pair ((Lisp.Atom (Lisp.Symbol "symbol1")),
                       (Lisp.Atom (Lisp.Symbol "symbol2")))
        ),
        Is.EqualTo("(symbol1 . symbol2)")
    )

[<Test>]
let it_should_print_cdr_nested_pairs () =
    Assert.That(
        Printer.Print (
            Lisp.Pair ((Lisp.Atom (Lisp.Symbol "sym1")),
                       (Lisp.Pair ((Lisp.Atom (Lisp.Symbol "sym2")),
                                   (Lisp.Sexpr [Lisp.Atom (Lisp.Symbol "sym3");
                                                Lisp.Atom Lisp.Nil]))))
        ),
        Is.EqualTo("(sym1 . (sym2 . (sym3)))")
    )

[<Test>]
let it_should_print_list () =
    Assert.That(
        Printer.Print (
            Lisp.Sexpr [Lisp.Atom (Lisp.Symbol "sym1");
                        Lisp.Atom (Lisp.Symbol "sym2");
                        Lisp.Atom (Lisp.Symbol "sym3");
                        Lisp.Atom Lisp.Nil]
        ),
        Is.EqualTo("(sym1 sym2 sym3)")
    )

[<Test>]
let it_should_print_car_nested_lists () =
    Assert.That(
        Printer.Print (
            Lisp.Sexpr [Lisp.Sexpr [Lisp.Sexpr [Lisp.Atom (Lisp.Symbol "sym1");
                                                Lisp.Atom Lisp.Nil];
                                    Lisp.Atom (Lisp.Symbol "sym2");
                                    Lisp.Atom Lisp.Nil];
                        Lisp.Atom (Lisp.Symbol "sym3");
                        Lisp.Atom Lisp.Nil]
        ),
        Is.EqualTo("(((sym1) sym2) sym3)")
    )

[<Test>]
let it_should_print_cadr_nested_lists () =
    Assert.That(
        Printer.Print (
            Lisp.Sexpr [Lisp.Atom (Lisp.Symbol "sym1");
                        Lisp.Sexpr [Lisp.Atom (Lisp.Symbol "sym2");
                                    Lisp.Sexpr [Lisp.Atom (Lisp.Symbol "sym3");
                                                Lisp.Atom Lisp.Nil];
                                    Lisp.Atom Lisp.Nil];
                        Lisp.Atom Lisp.Nil]
        ),
        Is.EqualTo("(sym1 (sym2 (sym3)))")
    )
