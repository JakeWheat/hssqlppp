Copyright 2010 Jake Wheat

Tests for the infrastructure to create type safe access to databases -
the information needed is gathered during typechecking and exposed
in the StatementType annotation.

> module Database.HsSqlPpp.Tests.ParameterizedStatementTests (parameterizedStatementTests) where

> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> import Data.Char
> --import Text.Show.Pretty
> --import Debug.Trace


> import Database.HsSqlPpp.Ast.SqlTypes
> import Database.HsSqlPpp.Ast.Annotation
> import Database.HsSqlPpp.Parsing.Parser
> import Database.HsSqlPpp.Ast.TypeChecker
> import Database.HsSqlPpp.Ast.Catalog

> data Item = Group String [Item]
>           | Statements [(String, [CatalogUpdate], StatementType)]

> parameterizedStatementTests :: [Test.Framework.Test]
> parameterizedStatementTests = itemToTft testData

> testData :: Item
> testData =
>   Group "parameterized statement tests" [
>     Group "simple selects" [ Statements [
>        ("select test();"
>        ,[CatCreateFunction FunName "test" [] (Pseudo Void) False]
>        ,StatementType [] [])
>       ,("select adnum,adbin from pg_attrdef;"
>        ,[]
>        ,StatementType [] [("adnum", ScalarType "int2")
>                          ,("adbin", ScalarType "text")])
>       ,("select adnum,adbin from pg_attrdef where oid= ?;"
>        ,[]
>        ,StatementType [ScalarType "oid"] [("adnum", ScalarType "int2")
>                                           ,("adbin", ScalarType "text")])
>       ,("select count(1) from pg_attrdef;"
>        ,[]
>        ,StatementType [] [("count", ScalarType "int8")])
>        {-,("select test($1);"
>        ,[CatCreateFunction FunName "test" [ScalarType "int4"] (ScalarType "text") False]
>        ,StatementType [ScalarType "int4"] [("test",ScalarType "text")])
>       ,-}
>       ,("select test(?);"
>        ,[CatCreateFunction FunName "test" [ScalarType "int4"] (ScalarType "text") False]
>        ,StatementType [ScalarType "int4"] [("test",ScalarType "text")])
>        ]
>     ]
>    ,Group "simple dml" [ Statements [
>        ("insert into testt values (1, 'test');"
>        ,[CatCreateTable "testt" [("c1", typeInt)
>                                 ,("c2", ScalarType "text")] []]
>        ,StatementType [] [])
>       ,("insert into testt (c1,c2) values (?, ?);"
>        ,[CatCreateTable "testt" [("c1", typeInt)
>                                 ,("c2", ScalarType "text")] []]
>        ,StatementType [typeInt, ScalarType "text"] [])
>       ,("insert into testt (c1,c2) values (1, 'test') returning c1;"
>        ,[CatCreateTable "testt" [("c1", typeInt)
>                                 ,("c2", ScalarType "text")] []]
>        ,StatementType [] [("c1",typeInt)])
>       ,("insert into testt (c1,c2) values (?, ?) returning c1 as d1, c2;"
>        ,[CatCreateTable "testt" [("c1", typeInt)
>                                 ,("c2", ScalarType "text")] []]
>        ,StatementType [typeInt, ScalarType "text"] [("d1", typeInt)
>                                                    ,("c2", ScalarType "text")])
>       {-,("insert into testt (c1,c2) values (?, ?) returning *;"
>        ,[CatCreateTable "testt" [("c1", typeInt)
>                                 ,("c2", ScalarType "text")] []]
>        ,StatementType [typeInt, ScalarType "text"] [("c1", typeInt)
>                                                    ,("c2", ScalarType "text")])-}
>       ,("update testt set c1= ?,c2= ? where c1= ? returning c2;"
>        ,[CatCreateTable "testt" [("c1", typeInt)
>                                 ,("c2", ScalarType "text")] []]
>        ,StatementType [typeInt, ScalarType "text", typeInt] [("c2", ScalarType "text")])
>       ,("update testt set (c1,c2) = (?,?);"
>        ,[CatCreateTable "testt" [("c1", typeInt)
>                                 ,("c2", ScalarType "text")] []]
>        ,StatementType [typeInt, ScalarType "text"] [])
>       ,("delete from blah where c1= ? returning c2;"
>        ,[CatCreateTable "blah" [("c1", typeInt)
>                                 ,("c2", ScalarType "text")] []]
>        ,StatementType [typeInt] [("c2", ScalarType "text")])
>       ]
>     ]
>    ]

select a,b from c where d=e group by f having  g=h orderby i limit j offset k;
select function(a,b);
insert into a (b,c) values (d,e) returning f,g
update a set b=c,d=e where f=g returning h,i
delete from t where a=b returning c,d

rough list of grammar elements to possibly add inference support to:

where top level - >maybeboolexpr
having
limit, offset
values for table literal or insert
set clause
rowsetclause
onexpr, joinon?
windowfn
liftoperator
selectitem?
castexpression
caseexpression
funcall
inpredicate

  TODO: add support in each of these places for position args only when
        doing typecheckPS
        do typecheck error if come across ? when not doing typecheckPS
        make sure ? fails type check if not in a valid place in the ast

================================================================================


> testStatementType :: String -> [CatalogUpdate] -> StatementType -> Test.Framework.Test
> testStatementType src eu st = testCase ("typecheck " ++ src) $
>   let ast = case parseSql "" src of
>                               Left e -> error $ show e
>                               Right l -> l
>   in case typeCheckPS makeCat (head ast) of
>        Left e -> error $ show e
>        Right aast -> --trace (ppShow aast) $
>                      let is = getTopLevelInfos [aast]
>                          er = concatMap snd $ getTypeErrors [aast]
>                      in case is of
>                                 _ | not (null er) -> assertFailure $ show er
>                                 [Just is1] -> assertEqual ("typecheck " ++ src) st is1
>                                 _ -> assertFailure ("expected onne statementinfo, got " ++ show is)
>   where
>     makeCat = case updateCatalog defaultTemplate1Catalog eu of
>                         Left x -> error $ show x
>                         Right e -> e

> itemToTft :: Item -> [Test.Framework.Test]
> itemToTft (Statements es) = map (\(a,b,c) -> testStatementType a b c) es
> itemToTft (Group s is) = [testGroup s $ concatMap itemToTft is]

