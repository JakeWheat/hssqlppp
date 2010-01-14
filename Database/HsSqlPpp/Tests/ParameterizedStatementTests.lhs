Copyright 2010 Jake Wheat

Tests for the infrastructure to create type safe access to databases -
the information needed is gathered during typechecking and exposed
in the StatementType annotation.

> module Database.HsSqlPpp.Tests.ParameterizedStatementTests (parameterizedStatementTests) where

> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> import Data.Char

> import Database.HsSqlPpp.Ast.SqlTypes
> import Database.HsSqlPpp.Ast.Annotation
> import Database.HsSqlPpp.Parsing.Parser
> import Database.HsSqlPpp.Ast.TypeChecker
> import Database.HsSqlPpp.Ast.Environment

> data Item = Group String [Item]
>           | Statements [(String, [EnvironmentUpdate], StatementType)]

> parameterizedStatementTests :: [Test.Framework.Test]
> parameterizedStatementTests = itemToTft testData

> testData :: Item
> testData =
>   Group "parameterized statement tests" [
>     Group "simple selects" [ Statements [
>        ("select test();"
>        ,[EnvCreateFunction FunName "test" [] (Pseudo Void) False]
>        ,StatementType [] [])
>       ,("select adnum,adbin from pg_attrdef;"
>        ,[]
>        ,StatementType [] [("adnum", ScalarType "int2")
>                          ,("adbin", ScalarType "text")])
>       ,("select adnum,adbin from pg_attrdef where oid= ?;"
>        ,[]
>        ,StatementType [ScalarType "oid"] [("adnum", ScalarType "int2")
>                                           ,("adbin", ScalarType "text")])
>        {-,("select test($1);"
>        ,[EnvCreateFunction FunName "test" [ScalarType "int4"] (ScalarType "text") False]
>        ,StatementType [ScalarType "int4"] [("test",ScalarType "text")])
>       ,-}
>       ,("select test(?);"
>        ,[EnvCreateFunction FunName "test" [ScalarType "int4"] (ScalarType "text") False]
>        ,StatementType [ScalarType "int4"] [("test",ScalarType "text")])
>        ]
>     ]
>    ,Group "dml relying only on funcall inference" [ Statements [
>        ("insert into testt values (1, 'test');"
>        ,[EnvCreateTable "testt" [("c1", typeInt)
>                                 ,("c2", ScalarType "text")] []]
>        ,StatementType [] [])
>       ,("insert into testt (c1,c2) values (1, 'test') returning c1;"
>        ,[EnvCreateTable "testt" [("c1", typeInt)
>                                 ,("c2", ScalarType "text")] []]
>        ,StatementType [] [("c1",typeInt)])
>       ,("insert into testt (c1,c2) values (?, ?) returning c1 as d1, c2;"
>        ,[EnvCreateTable "testt" [("c1", typeInt)
>                                 ,("c2", ScalarType "text")] []]
>        ,StatementType [typeInt, ScalarType "text"] [("d1", typeInt)
>                                                    ,("c2", ScalarType "text")])
>       ,("insert into testt (c1,c2) values (?, ?) returning *;"
>        ,[EnvCreateTable "testt" [("c1", typeInt)
>                                 ,("c2", ScalarType "text")] []]
>        ,StatementType [typeInt, ScalarType "text"] [("c1", typeInt)
>                                                    ,("c2", ScalarType "text")])
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


> testStatementType :: String -> [EnvironmentUpdate] -> StatementType -> Test.Framework.Test
> testStatementType src eu st = testCase ("typecheck " ++ src) $
>   let ast = case parseSql "" src of
>                               Left e -> error $ show e
>                               Right l -> l
>   in case typeCheckPS makeEnv (head ast) of
>        Left e -> error $ show e
>        Right aast -> let is = getTopLevelInfos [aast]
>                          er = concatMap snd $ getTypeErrors [aast]
>                      in case is of
>                                 _ | not (null er) -> assertFailure $ show er
>                                 [Just is1] -> assertEqual ("typecheck " ++ src) st is1
>                                 _ -> assertFailure ("expected onne statementinfo, got " ++ show is)
>   where
>     makeEnv = case updateEnvironment defaultTemplate1Environment eu of
>                         Left x -> error $ show x
>                         Right e -> e

> itemToTft :: Item -> [Test.Framework.Test]
> itemToTft (Statements es) = map (\(a,b,c) -> testStatementType a b c) es
> itemToTft (Group s is) = [testGroup s $ concatMap itemToTft is]

