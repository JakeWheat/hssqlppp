
todo:
quickcheck:
generate random symbols
print then parse, check equal
generate random strings

try to parse, if parses, print and check equal to original string

> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.Parsing.LexerTests where

> import Database.HsSqlPpp.LexicalSyntax
> --import Database.HsSqlPpp.Internals.Dialect
> import Database.HsSqlPpp.Tests.TestTypes
> --import Database.HsSqlPpp.Tests.Parsing.Utils

> lexerTests :: Item
> lexerTests = Group "lexing"
>     [Lex PostgreSQL "'string'" [SqlString "'" "string"]
>     ,Lex PostgreSQL "E'string\\n'" [SqlString "E'" "string\\n"] -- the \\n is to put a literal \ and n in the string
>     ,Lex PostgreSQL "E'bsquoteend\\''" [SqlString "E'" "bsquoteend\\'"]
>     ,Lex PostgreSQL "E'bsquote\\'xx'" [SqlString "E'" "bsquote\\'xx"]
>     ,Lex PostgreSQL "E'quoteend'''" [SqlString "E'" "quoteend''"]
>     ,Lex PostgreSQL "E'quote''x'" [SqlString "E'" "quote''x"]
>     ,Lex PostgreSQL "'normal '' quote'" [SqlString "'" "normal '' quote"]
>     ,Lex PostgreSQL "'normalendquote '''" [SqlString "'" "normalendquote ''"]
>     ,Lex PostgreSQL "$$dollar quoting$$" [SqlString "$$" "dollar quoting"]
>     ,Lex PostgreSQL "$x$dollar $$ quoting$x$" [SqlString "$x$" "dollar $$ quoting"]

identifiers

>     ,Lex PostgreSQL "test" [Identifier Nothing "test"]
>     ,Lex PostgreSQL "_test" [Identifier Nothing "_test"]
>     ,Lex PostgreSQL "\"test test\"" [Identifier (Just ('"','"')) "test test"]
>     ,Lex PostgreSQL "test123" [Identifier Nothing "test123"]
>     ,Lex SQLServer "[test \"]" [Identifier (Just ('[',']')) "test \""]
>     ,Lex SQLServer "@test" [Identifier Nothing "@test"]
>     ,Lex SQLServer "#test" [Identifier Nothing "#test"]
>     ,Lex Oracle ":test" [Identifier Nothing ":test"]

symbols

>     ,Lex PostgreSQL "+" [Symbol "+"]
>     ,Lex PostgreSQL "*" [Symbol "*"]

numbers

>     ,Lex PostgreSQL "10" [SqlNumber "10"]
>     ,Lex PostgreSQL ".1" [SqlNumber ".1"]
>     ,Lex PostgreSQL "5e3" [SqlNumber "5e3"]
>     ,Lex PostgreSQL "5e+3" [SqlNumber "5e+3"]
>     ,Lex PostgreSQL "5e-3" [SqlNumber "5e-3"]
>     ,Lex PostgreSQL "10.2" [SqlNumber "10.2"]
>     ,Lex PostgreSQL "10.2e7" [SqlNumber "10.2e7"]

whitespace

>     ,Lex PostgreSQL " " [Whitespace " "]
>     ,Lex PostgreSQL "  " [Whitespace "  "]
>     ,Lex PostgreSQL "\n" [Whitespace "\n"]
>     ,Lex PostgreSQL "\t" [Whitespace "\t"]

check dots

>     ,Lex PostgreSQL "a.b" [Identifier Nothing "a"
>                                  ,Symbol "."
>                                  ,Identifier Nothing "b"]

positional arg

>     ,Lex PostgreSQL "$1" [PositionalArg 1]

line comment

>     ,Lex PostgreSQL "-- this is a comment\n" [LineComment "-- this is a comment\n"]
>     -- check eof with no trailing newline
>     ,Lex PostgreSQL "-- this is a comment" [LineComment "-- this is a comment"]

block comment

>     ,Lex PostgreSQL "/* block\ncomment */" [BlockComment "/* block\ncomment */"]
>     ,Lex PostgreSQL "/* nested /*block*/ comment */" [BlockComment "/* nested /*block*/ comment */"]

splice

>     ,Lex PostgreSQL "$c(splice)" [Splice 'c' "splice"]

>     ,Lex PostgreSQL "1 .. 2"
>        [SqlNumber "1", Whitespace " ", Symbol ".."
>        ,Whitespace " ", SqlNumber "2"]

>     ,Lex PostgreSQL "1..2"
>        [SqlNumber "1", Symbol "..", SqlNumber "2"]


>     ]
