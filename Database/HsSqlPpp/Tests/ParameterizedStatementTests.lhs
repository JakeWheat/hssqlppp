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
>                          ,("adbin", ScalarType "text")])]
>     ]
>    ,Group "simple fn calls" [ Statements [
>        ("select test($1);"
>        ,[EnvCreateFunction FunName "test" [ScalarType "int4"] (ScalarType "text") False]
>        ,StatementType [ScalarType "int4"] [("test",ScalarType "text")])
>       ,("select test(?);"
>        ,[EnvCreateFunction FunName "test" [ScalarType "int4"] (ScalarType "text") False]
>        ,StatementType [ScalarType "int4"] [("test",ScalarType "text")])
>        ]
>     ]
>   ]

select a,b from c where d=e group by f having  g=h orderby i limit j offset k;
select function(a,b);
insert into a (b,c) values (d,e) returning f,g
update a set b=c,d=e where f=g returning h,i
delete from t where a=b returning c,d

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

