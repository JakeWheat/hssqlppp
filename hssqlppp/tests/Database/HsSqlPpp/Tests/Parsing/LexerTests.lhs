
todo:
quickcheck:
generate random symbols
print then parse, check equal
generate random strings

try to parse, if parses, print and check equal to original string

> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.Parsing.LexerTests where

> import Database.HsSqlPpp.Lex
> --import Database.HsSqlPpp.Internals.Dialect
> import Database.HsSqlPpp.Tests.TestTypes
> import Database.HsSqlPpp.Dialect
> --import Database.HsSqlPpp.Tests.Parsing.Utils

> lexerTests :: Item
> lexerTests = Group "lexing"
>     [Lex postgresDialect "'string'" [SqlString "'" "'" "string"]
>     ,Lex postgresDialect "E'string\\n'" [SqlString "E'" "'" "string\\n"] -- the \\n is to put a literal \ and n in the string
>     ,Lex postgresDialect "E'bsquoteend\\''" [SqlString "E'" "'" "bsquoteend\\'"]
>     ,Lex postgresDialect "E'bsquote\\'xx'" [SqlString "E'" "'" "bsquote\\'xx"]
>     ,Lex postgresDialect "E'quoteend'''" [SqlString "E'" "'" "quoteend''"]
>     ,Lex postgresDialect "E'quote''x'" [SqlString "E'" "'" "quote''x"]
>     ,Lex postgresDialect "'normal '' quote'" [SqlString "'" "'" "normal '' quote"]
>     ,Lex postgresDialect "'normalendquote '''" [SqlString "'" "'" "normalendquote ''"]
>     ,Lex postgresDialect "$$dollar quoting$$" [SqlString "$$" "$$" "dollar quoting"]
>     ,Lex postgresDialect "$x$dollar $$ quoting$x$" [SqlString "$x$" "$x$" "dollar $$ quoting"]

identifiers

>     ,Lex postgresDialect "test" [Identifier Nothing "test"]
>     ,Lex postgresDialect "_test" [Identifier Nothing "_test"]
>     ,Lex postgresDialect "\"test test\"" [Identifier (Just ("\"","\"")) "test test"]
>     ,Lex postgresDialect "test123" [Identifier Nothing "test123"]
>     ,Lex sqlServerDialect "[test \"]" [Identifier (Just ("[","]")) "test \""]
>     ,Lex sqlServerDialect "@test" [PrefixedVariable '@' "test"]
>     ,Lex sqlServerDialect "#test" [PrefixedVariable '#' "test"]
>     ,Lex oracleDialect ":test" [PrefixedVariable ':' "test"]

symbols

>     ,Lex postgresDialect "+" [Symbol "+"]
>     ,Lex postgresDialect "*" [Symbol "*"]

numbers

>     ,Lex postgresDialect "10" [SqlNumber "10"]
>     ,Lex postgresDialect ".1" [SqlNumber ".1"]
>     ,Lex postgresDialect "5e3" [SqlNumber "5e3"]
>     ,Lex postgresDialect "5e+3" [SqlNumber "5e+3"]
>     ,Lex postgresDialect "5e-3" [SqlNumber "5e-3"]
>     ,Lex postgresDialect "10.2" [SqlNumber "10.2"]
>     ,Lex postgresDialect "10.2e7" [SqlNumber "10.2e7"]

whitespace

>     ,Lex postgresDialect " " [Whitespace " "]
>     ,Lex postgresDialect "  " [Whitespace "  "]
>     ,Lex postgresDialect "\n" [Whitespace "\n"]
>     ,Lex postgresDialect "\t" [Whitespace "\t"]

check dots

>     ,Lex postgresDialect "a.b" [Identifier Nothing "a"
>                                  ,Symbol "."
>                                  ,Identifier Nothing "b"]

positional arg

>     ,Lex postgresDialect "$1" [PositionalArg 1]

line comment

>     ,Lex postgresDialect "-- this is a comment\n" [LineComment "-- this is a comment\n"]
>     -- check eof with no trailing newline
>     ,Lex postgresDialect "-- this is a comment" [LineComment "-- this is a comment"]

block comment

>     ,Lex postgresDialect "/* block\ncomment */" [BlockComment "/* block\ncomment */"]
>     ,Lex postgresDialect "/* nested /*block*/ comment */" [BlockComment "/* nested /*block*/ comment */"]

splice

>     ,Lex postgresDialect "$c(splice)" [Splice 'c' "splice"]

>     ,Lex postgresDialect "1 .. 2"
>        [SqlNumber "1", Whitespace " ", Symbol ".."
>        ,Whitespace " ", SqlNumber "2"]

>     ,Lex postgresDialect "1..2"
>        [SqlNumber "1", Symbol "..", SqlNumber "2"]


>     ]
