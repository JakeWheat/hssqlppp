
todo:
quickcheck:
generate random symbols
print then parse, check equal
generate random strings

try to parse, if parses, print and check equal to original string

> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.Parsing.LexerTests where

> import Database.HsSqlPpp.LexicalSyntax
> import Database.HsSqlPpp.SqlDialect
> import Database.HsSqlPpp.Tests.Parsing.Utils

> lexerTests :: Item
> lexerTests = Group "lexing"
>     [Lex PostgreSQLDialect "'string'" [SqlString "'" "string"]
>     ,Lex PostgreSQLDialect "E'string\\n'" [SqlString "E'" "string\\n"] -- the \\n is to put a literal \ and n in the string
>     ,Lex PostgreSQLDialect "E'bsquoteend\\''" [SqlString "E'" "bsquoteend\\'"]
>     ,Lex PostgreSQLDialect "E'bsquote\\'xx'" [SqlString "E'" "bsquote\\'xx"]
>     ,Lex PostgreSQLDialect "E'quoteend'''" [SqlString "E'" "quoteend''"]
>     ,Lex PostgreSQLDialect "E'quote''x'" [SqlString "E'" "quote''x"]
>     ,Lex PostgreSQLDialect "'normal '' quote'" [SqlString "'" "normal '' quote"]
>     ,Lex PostgreSQLDialect "'normalendquote '''" [SqlString "'" "normalendquote ''"]
>     ,Lex PostgreSQLDialect "$$dollar quoting$$" [SqlString "$$" "dollar quoting"]
>     ,Lex PostgreSQLDialect "$x$dollar $$ quoting$x$" [SqlString "$x$" "dollar $$ quoting"]

identifiers

>     ,Lex PostgreSQLDialect "test" [Identifier Nothing "test"]
>     ,Lex PostgreSQLDialect "_test" [Identifier Nothing "_test"]
>     ,Lex PostgreSQLDialect "\"test test\"" [Identifier (Just ('"','"')) "test test"]
>     ,Lex PostgreSQLDialect "test123" [Identifier Nothing "test123"]
>     ,Lex SQLServerDialect "[test \"]" [Identifier (Just ('[',']')) "test \""]
>     ,Lex SQLServerDialect "@test" [Identifier Nothing "@test"]
>     ,Lex SQLServerDialect "#test" [Identifier Nothing "#test"]
>     ,Lex OracleDialect ":test" [Identifier Nothing ":test"]

symbols

>     ,Lex PostgreSQLDialect "+" [Symbol "+"]
>     ,Lex PostgreSQLDialect "*" [Symbol "*"]

numbers

>     ,Lex PostgreSQLDialect "10" [SqlNumber "10"]
>     ,Lex PostgreSQLDialect ".1" [SqlNumber ".1"]
>     ,Lex PostgreSQLDialect "5e3" [SqlNumber "5e3"]
>     ,Lex PostgreSQLDialect "5e+3" [SqlNumber "5e+3"]
>     ,Lex PostgreSQLDialect "5e-3" [SqlNumber "5e-3"]
>     ,Lex PostgreSQLDialect "10.2" [SqlNumber "10.2"]
>     ,Lex PostgreSQLDialect "10.2e7" [SqlNumber "10.2e7"]

whitespace

>     ,Lex PostgreSQLDialect " " [Whitespace " "]
>     ,Lex PostgreSQLDialect "  " [Whitespace "  "]
>     ,Lex PostgreSQLDialect "\n" [Whitespace "\n"]
>     ,Lex PostgreSQLDialect "\t" [Whitespace "\t"]

positional arg

>     ,Lex PostgreSQLDialect "$1" [PositionalArg 1]

line comment

>     ,Lex PostgreSQLDialect "-- this is a comment\n" [LineComment "-- this is a comment\n"]
>     -- check eof with no trailing newline
>     ,Lex PostgreSQLDialect "-- this is a comment" [LineComment "-- this is a comment"]

block comment

>     ,Lex PostgreSQLDialect "/* block\ncomment */" [BlockComment "/* block\ncomment */"]
>     ,Lex PostgreSQLDialect "/* nested /*block*/ comment */" [BlockComment "/* nested /*block*/ comment */"]

splice

>     ,Lex PostgreSQLDialect "$c(splice)" [Splice 'c' "splice"]

>     ]
