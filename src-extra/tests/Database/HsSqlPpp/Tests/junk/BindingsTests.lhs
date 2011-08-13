
Test the bindings code is working.

LBIds|LBTref|LBJoinTref|LocalBindingsUpdate|emptyBindings|lbUpdate|lbExpandStar|lbLookupID|lbUpdateDot

Where are local bindings set, and where are lookups done?

== insert

--lookup: table
set: col names for targetCols
--lookup: 'implicit assigment' for select expression -> cols
--lookup: returning
set: record type in returning

== update
--lookup: table
set: col names for assigns, where, returning
what about from joins?
--lookup: where
--lookup: returning
set: record type in returning

== delete
--lookup: table
set: col names for where and returning
what about using?

= copy
--lookup: table
set: col names for targetcols + lookup

= create table:
set atts into constraints

= create function:
params

= create domain
check expression

= assignment
record type

= execute into
record type

= for select
record type

= for integer
implicitly declared integer
record type

= select expression
tref -> select list, where, group, order

= trefs
tref cols up, into on exprs

etc.
etc.

New approach:
test the hairy bits first:
trefs + aliases, ambiguous, correlation names, on exprs
contexts for *: selectlist, returning, aggregates
hack for member with same name as composite in select list
record type setting
implicit int in for loop



> module Database.HsSqlPpp.Tests.BindingsTests
>     (bindingsTests) where
>
> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> import Data.Generics.Uniplate.Data

> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.TypeChecker
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.SqlTypes
> import Database.HsSqlPpp.Utils.PPExpr
> import Database.HsSqlPpp.Tests.TestUtils

>
> data Item = Group String [Item]
>           | StmtType [CatalogUpdate] String (Either [TypeError] [Maybe StatementType])
>
> bindingsTests :: Test.Framework.Test
> bindingsTests = itemToTft bindingsTestData
>
> bindingsTestData :: Item
> bindingsTestData =
>   Group "bind tests"
>    [trefTests]

> trefTests :: Item
> trefTests = Group "tref" [simpleSingleTrefs
>                          ,simpleSingleAliasedTrefs]


= simple selects with a single unaliased table:

> simpleSingleTrefs :: Item
> simpleSingleTrefs = Group "simpleSingleTrefs"

single columns, including system columns

>     [StmtType ct "select a from t;"
>      $ Right [Just ([], [("a",typeInt)])]
>     ,StmtType ct "select b from t;"
>      $ Right [Just ([], [("b", ScalarType "text")])]
>     ,StmtType ct "select oid from t;"
>      $ Right [Just ([], [("oid", typeInt)])]

multiple columns

>     ,StmtType ct "select a,b from t;"
>      $ Right [Just ([], cols)]
>     ,StmtType ct "select a,b,oid from t;"
>      $ Right [Just ([], cols ++ [("oid", typeInt)])]

aliased columns

>     ,StmtType ct "select a as c, b as d from t;"
>      $ Right [Just ([], [("c",typeInt)
>                         ,("d", ScalarType "text")])]
>     ,StmtType ct "select a as x,b as y,oid as z from t;"
>      $ Right [Just ([], [("x",typeInt)
>                         ,("y", ScalarType "text")
>                         ,("z", typeInt)])]

qualified column references

>     ,StmtType ct "select t.a from t;"
>      $ Right [Just ([], [("a",typeInt)])]
>     ,StmtType ct "select t.b from t;"
>      $ Right [Just ([], [("b", ScalarType "text")])]
>     ,StmtType ct "select t.oid from t;"
>      $ Right [Just ([], [("oid", typeInt)])]

star variations

>     ,StmtType ct "/*cunt*/select * from t;"
>      $ Right [Just ([], cols)]
>     ,StmtType ct "/*cunt*/select t.* from t;"
>      $ Right [Just ([], cols)]
>     ,StmtType ct "select *, a as c from t;"
>      $ Right [Just ([], [("a",typeInt)
>                         ,("b", ScalarType "text")
>                         ,("c",typeInt)])]

errors: unrecognised column, unrecognised correlation name, unrecognised table

>     ,StmtType ct "select c from t;"
>      $ Left [UnrecognisedIdentifier "c"]
>     ,StmtType ct "select c.* from t;"
>      $ Left [UnrecognisedCorrelationName "c"]
>     ,StmtType ct "select * from u;"
>      $ Left [BadStarExpand,UnrecognisedRelation "u"]

>     ]

>    where cols = [("a",typeInt)
>                 ,("b", ScalarType "text")]
>          ct = [CatCreateTable "t" cols [("oid", typeInt)]]

= simple selects with a single aliased table:

> simpleSingleAliasedTrefs :: Item
> simpleSingleAliasedTrefs = Group "simpleSingleAliasedTrefs"

qualified column references

>     [StmtType ct "select u.a from t u;"
>      $ Right [Just ([], [("a",typeInt)])]
>     ,StmtType ct "select t.a from t u;"
>      $ Left [UnrecognisedCorrelationName "t"]
>     ,StmtType ct "select u.oid from t u;"
>      $ Right [Just ([], [("oid", typeInt)])]
>     ,StmtType ct "select t.oid from t u;"
>      $ Left [UnrecognisedCorrelationName "t"]

star variations

>     ,StmtType ct "select t.* from t;"
>      $ Right [Just ([], cols)]
>     ,StmtType ct "select u.* from t;"
>      $ Left [UnrecognisedCorrelationName "u"]
>     ]

>    where cols = [("a",typeInt)
>                 ,("b", ScalarType "text")]
>          ct = [CatCreateTable "t" cols [("oid", typeInt)]]


= simple selects with a single function:

> {-simpleSingleFunTrefs :: Item
> simpleSingleFunTrefs = Group "simpleSingleFunTrefs"

single columns, including system columns

>     [StmtType ct "select a from t;"
>      $ Right [Just $ ([], [("a",typeInt)])]
>     ,StmtType ct "select b from t;"
>      $ Right [Just $ ([], [("b", ScalarType "text")])]

multiple columns

>     ,StmtType ct "select a,b from t;"
>      $ Right [Just $ ([], cols)]
>     ,StmtType ct "select a,b,oid from t;"
>      $ Right [Just $ ([], cols ++ [("oid", typeInt)])]

aliased columns

>     ,StmtType ct "select a as c, b as d from t;"
>      $ Right [Just $ ([], [("c",typeInt)
>                           ,("d", ScalarType "text")])]
>     ,StmtType ct "select a as x,b as y,oid as z from t;"
>      $ Right [Just $ ([], [("x",typeInt)
>                           ,("y", ScalarType "text")
>                           ,("z", typeInt)])]

qualified column references

>     ,StmtType ct "select t.a from t;"
>      $ Right [Just $ ([], [("a",typeInt)])]
>     ,StmtType ct "select t.b from t;"
>      $ Right [Just $ ([], [("b", ScalarType "text")])]
>     ,StmtType ct "select t.oid from t;"
>      $ Right [Just $ ([], [("oid", typeInt)])]

star variations

>     ,StmtType ct "select * from t;"
>      $ Right [Just $ ([], cols)]
>     ,StmtType ct "select t.* from t;"
>      $ Right [Just $ ([], cols)]
>     ,StmtType ct "select *, a as c from t;"
>      $ Right [Just $ ([], [("a",typeInt)
>                           ,("b", ScalarType "text")
>                           ,("c",typeInt)])]

errors: unrecognised column, unrecognised correlation name, unrecognised table

>     ,StmtType ct "select c from t;"
>      $ Left [UnrecognisedIdentifier "c"]
>     ,StmtType ct "select c.* from t;"
>      $ Left [UnrecognisedIdentifier "c"]
>     ,StmtType ct "select * from u;"
>      $ Left [UnrecognisedRelation "u"]

>     ]

>    where cols = [("a",typeInt)
>                 ,("b", ScalarType "text")]
>          ct = [CatCreateTable "t" cols [("oid", typeInt)]]

= simple selects with a single aliased table:

> simpleSingleAliasedTrefs :: Item
> simpleSingleAliasedTrefs = Group "simpleSingleAliasedTrefs"

qualified column references

>     [StmtType ct "select u.a from t u;"
>      $ Right [Just $ ([], [("a",typeInt)])]
>     ,StmtType ct "select t.a from t u;"
>      $ Left []
>     ,StmtType ct "select u.oid from t u;"
>      $ Right [Just $ ([], [("oid", typeInt)])]
>     ,StmtType ct "select t.oid from t u;"
>      $ Left []

star variations

>     ,StmtType ct "select t.* from t;"
>      $ Left []
>     ,StmtType ct "select u.* from t;"
>      $ Right [Just $ ([], cols)]
>     ]

>    where cols = [("a",typeInt)
>                 ,("b", ScalarType "text")]
>          ct = [CatCreateTable "t" cols [("oid", typeInt)]] -}



plain references
bad references
qualified refs
star on own
qualified star
star with extras
qualified star with extras
system columns

same with aliases

above for simpletref, funtref, subselecttref



composite/member hack for select lists check

simple joins - two tables
cross
implicit cross
using
natural

do: ambiguous errors where possible
    list columns out
    list qualified columns
    star + qualified combos
degenerate natural
bad using: no col, incompatible types
on expression check, and implicit cross with on in where variant

then: big space - aliased, three way joins, on exprs, etc.

----------------------

update: tref -> whr, assigns, returning
select: tref -? select list, where, group, order by
tref: subtref, vanilla, fun, join
onexpr
assignment: lkp + adjust record type
for loop: check type + implicit integer decl + adjust rec type
delete tref -> whr, returning
block decl
create table -> table constraints
att def -> constraint
function params + pos arg ref
select into: assign checks + adjust rec type
insert returning, values list?
"." expansion
identifier ref: todo - list all contexts
domain constraint
trigger old and new hack





------------------------

> itemToTft :: Item -> Test.Framework.Test
> itemToTft (StmtType c s r) = testStatementType c s r
> itemToTft (Group s is) = testGroup s $ map itemToTft is

> testStatementType :: [CatalogUpdate] -> String -> Either [TypeError] [Maybe StatementType] -> Test.Framework.Test
> testStatementType eu src sis = testCase ("typecheck " ++ src) $
>   let ast = case parseStatements "" src of
>                               Left e -> error $ show e
>                               Right l -> l
>       aast = snd $ typeCheckStatements makeCat ast
>       is = map (stType . getAnnotation) aast
>       er :: [TypeError]
>       er = x <- universeBi aast
>   in case (length er, length is) of
>        (0,0) -> assertFailure "didn't get any infos?"
>        (0,_) -> assertTrace (ppExpr is) ("typecheck " ++ src) sis $ Right is
>        _ -> assertEqual ("typecheck " ++ src) sis $ Left er
>   where
>     makeCat = case updateCatalog defaultTemplate1Catalog eu of
>                         Left x -> error $ show x
>                         Right e -> e
