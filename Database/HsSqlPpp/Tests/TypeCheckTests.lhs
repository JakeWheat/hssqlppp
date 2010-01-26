Copyright 2009 Jake Wheat

Set of tests to check the type checking code. Includes some tests for
sql which doesn't type check.


> module Database.HsSqlPpp.Tests.TypeCheckTests (typeCheckTests) where

> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> import Data.List

> import Database.HsSqlPpp.Parsing.Parser
> import Database.HsSqlPpp.Ast.TypeChecker
> import Database.HsSqlPpp.Ast.Annotation
> import Database.HsSqlPpp.Ast.Catalog
> import Database.HsSqlPpp.Ast.SqlTypes

> data Item = Expressions [(String, Either [TypeError] Type)]
>           | StatementTypes [(String, Either [TypeError] [Maybe StatementType])]
>           | CatUpStatementTypes [(String
>                                  ,[CatalogUpdate]
>                                  ,Either [TypeError] [Maybe StatementType])]
>           | DdlStatements [(String, [[CatalogUpdate]])]
>           | DdlStatementsCat [(String, [CatalogUpdate])]
>           | Group String [Item]

> typeCheckTests :: [Test.Framework.Test]
> typeCheckTests = itemToTft typeCheckTestData

> typeCheckTestData :: Item
> typeCheckTestData =
>   Group "astCheckTests" [

>    Group "basic literal types" [ Expressions [
>       p "1" $ Right typeInt
>      ,p "1.0" $ Right typeNumeric
>      ,p "'test'" $ Right UnknownType
>      ,p "true" $ Right typeBool
>      ,p "array[1,2,3]" $ Right (ArrayType typeInt)
>      ,p "array['a','b']" $ Right (ArrayType UnknownType)
>      ,p "array['a'::text,'b']" $ Right (ArrayType (ScalarType "text"))
>      ,p "array['a','b'::text]" $ Right (ArrayType (ScalarType "text"))
>      ,p "array[1,'b']" $ Right (ArrayType typeInt)
>      ,p "array[1,true]" $ Left [NoMatchingOperator "!arrayctor" [ScalarType "int4",ScalarType "bool"]]
>      ]]

>   ,Group "some expressions" [ Expressions [
>       p "1=1" $ Right typeBool
>      ,p "1=true" $ Left [NoMatchingOperator "=" [typeInt,typeBool]]
>      ,p "substring('aqbc' from 2 for 2)" $ Right (ScalarType "text")

>      ,p "substring(3 from 2 for 2)" $ Left
>                                      [NoMatchingOperator "!substring"
>                                       [ScalarType "int4"
>                                       ,ScalarType "int4"
>                                       ,ScalarType "int4"]]
>      ,p "substring('aqbc' from 2 for true)" $ Left
>                     [NoMatchingOperator "!substring"
>                      [UnknownType
>                      ,ScalarType "int4"
>                      ,ScalarType "bool"]]

>      ,p "3 between 2 and 4" $ Right typeBool
>      ,p "3 between true and 4" $ Left
>                                [NoMatchingOperator ">="
>                                 [typeInt
>                                 ,typeBool]]

>      ,p "array[1,2,3][2]" $ Right typeInt
>      ,p "array['a','b'][1]" $ Right UnknownType
>      ,p "array['a'::text,'b'][1]" $ Right (ScalarType "text")

 >      ,p "array['a','b'][true]" (TypeError ("",0,0)
 >                                   (WrongType
 >                                    typeInt
 >                                    UnknownType))

>      ,p "not true" $ Right typeBool
>      ,p "not 1" $ Left
>                  [NoMatchingOperator "!not" [typeInt]]

>      ,p "@ 3" $ Right typeInt
>      ,p "@ true" $ Left
>                  [NoMatchingOperator "@" [ScalarType "bool"]]

>      ,p "-3" $ Right typeInt
>      ,p "-'a'" $ Left
>                  [NoMatchingOperator "-" [UnknownType]]

>      ,p "4-3" $ Right typeInt

>      ,p "1 is null" $ Right typeBool
>      ,p "1 is not null" $ Right typeBool

>      ,p "1+1" $ Right typeInt
>      ,p "1+1" $ Right typeInt
>      ,p "31*511" $ Right typeInt
>      ,p "5/2" $ Right typeInt
>      ,p "2^10" $ Right typeFloat8
>      ,p "17%5" $ Right typeInt

>      ,p "3 and 4" $ Left
>                   [NoMatchingOperator "!and" [typeInt,typeInt]]

>      ,p "True and False" $ Right typeBool
>      ,p "false or true" $ Right typeBool

>      ,p "lower('TEST')" $ Right (ScalarType "text")
>      ,p "lower(1)" $ Left [NoMatchingOperator "lower" [typeInt]]
>      ]]

>   ,Group "special functions" [ Expressions [
>       p "coalesce(null,1,2,null)" $ Right typeInt
>      ,p "coalesce('3',1,2,null)" $ Right typeInt
>      ,p "coalesce('3',1,true,null)"
>             $ Left [NoMatchingOperator "coalesce" [UnknownType
>                                                   ,ScalarType "int4"
>                                                   ,ScalarType "bool"
>                                                   ,UnknownType]]
>      ,p "nullif('hello','hello')" $ Right UnknownType
>      ,p "nullif('hello'::text,'hello')" $ Right (ScalarType "text")
>      ,p "nullif(3,4)" $ Right typeInt
>      ,p "nullif(true,3)"
>             $ Left [NoMatchingOperator "nullif" [ScalarType "bool"
>                                           ,ScalarType "int4"]]
>      ,p "greatest(3,5,6,4,3)" $ Right typeInt
>      ,p "least(3,5,6,4,3)" $ Right typeInt
>      ,p "least(5,true)"
>             $ Left [NoMatchingOperator "least" [ScalarType "int4",ScalarType "bool"]]


>      ]]

implicit casting and function/operator choice tests:
check when multiple implicit and one exact match on num args
check multiple matches with num args, only one with implicit conversions
check multiple implicit matches with one preferred
check multiple implicit matches with one preferred highest count
check casts from unknown string lits

>   ,Group "some expressions" [ Expressions [
>       p "3 + '4'" $ Right typeInt
>      ,p "3.0 + '4'" $ Right typeNumeric
>      ,p "'3' + '4'" $ Left [NoMatchingOperator "+" [UnknownType
>                                               ,UnknownType]]

>      ]]

>   ,Group "exists expressions" [ Expressions [
>       p "exists (select 1 from pg_type)" $ Right typeBool
>      ,p "exists (select testit from pg_type)"
>        $ Left [UnrecognisedIdentifier "testit"]
>      ]]

>   ,Group "row comparison expressions" [ Expressions [

rows different lengths
rows match types pairwise, same and different types
rows implicit cast from unknown
rows don't match types

>       p "row(1)" $ Right (AnonymousRecordType [typeInt])
>      ,p "row(1,2)" $ Right (AnonymousRecordType [typeInt,typeInt])
>      ,p "row('t1','t2')" $ Right (AnonymousRecordType [UnknownType,UnknownType])
>      ,p "row(true,1,'t3')" $ Right (AnonymousRecordType [typeBool, typeInt,UnknownType])
>      ,p "(true,1,'t3',75.3)" $ Right (AnonymousRecordType [typeBool,typeInt
>                                       ,UnknownType,typeNumeric])
>      ,p "row(1) = row(2)" $ Right typeBool
>      ,p "row(1,2) = row(2,1)" $ Right typeBool
>      ,p "(1,2) = (2,1)" $ Right typeBool
>      ,p "(1,2,3) = (2,1)" $ Left [NoMatchingOperator "="
>                                   [AnonymousRecordType [ScalarType "int4"
>                                                        ,ScalarType "int4"
>                                                        ,ScalarType "int4"]
>                                    ,AnonymousRecordType [ScalarType "int4"
>                                                         ,ScalarType "int4"]]]
>      ,p "('1',2) = (2,'1')" $ Right typeBool
>      ,p "(1,true) = (2,1)" $ Left [NoMatchingOperator "="
>                                    [AnonymousRecordType [ScalarType "int4"
>                                                         ,ScalarType "bool"]
>                                    ,AnonymousRecordType [ScalarType "int4"
>                                                         ,ScalarType "int4"]]]
>      ,p "(1,2) <> (2,1)" $ Right typeBool
>      ,p "(1,2) >= (2,1)" $ Right typeBool
>      ,p "(1,2) <= (2,1)" $ Right typeBool
>      ,p "(1,2) > (2,1)" $ Right typeBool
>      ,p "(1,2) < (2,1)" $ Right typeBool
>      ]]

>   ,Group "case expressions" [ Expressions [
>       p "case\n\
>         \ when true then 1\n\
>         \end" $ Right typeInt
>      ,p "case\n\
>         \ when 1=2 then 'stuff'\n\
>         \ when 2=3 then 'blah'\n\
>         \ else 'test'\n\
>         \end" $ Right UnknownType
>      ,p "case\n\
>         \ when 1=2 then 'stuff'\n\
>         \ when 2=3 then 'blah'\n\
>         \ else 'test'::text\n\
>         \end" $ Right $ ScalarType "text"
>      ,p "case\n\
>         \ when 1=2 then 'stuff'\n\
>         \ when true=3 then 'blah'\n\
>         \ else 'test'\n\
>         \end" $ Left [NoMatchingOperator "=" [typeBool,typeInt]]
>      ,p "case\n\
>         \ when 1=2 then true\n\
>         \ when 2=3 then false\n\
>         \ else 1\n\
>         \end" $ Left [IncompatibleTypeSet [typeBool
>                                         ,typeBool
>                                         ,typeInt]]
>      ,p "case\n\
>         \ when 1=2 then false\n\
>         \ when 2=3 then 1\n\
>         \ else true\n\
>         \end" $ Left [IncompatibleTypeSet [typeBool
>                                           ,typeInt
>                                           ,typeBool]]

>      ,p "case 1 when 2 then 3 else 4 end" $ Right typeInt
>      ,p "case 1 when true then 3 else 4 end"
>             $ Left [IncompatibleTypeSet [ScalarType "int4"
>                                          ,ScalarType "bool"]]
>      ,p "case 1 when 2 then true else false end" $ Right typeBool
>      ,p "case 1 when 2 then 3 else false end"
>             $ Left [IncompatibleTypeSet [ScalarType "int4"
>                                          ,ScalarType "bool"]]
>      ]]

>   ,Group "polymorphic functions" [ Expressions [
>       p "array_append(ARRAY[1,2], 3)"
>         $ Right (ArrayType typeInt)
>      ,p "array_append(ARRAY['a','b'], 'c')"
>         $ Right (ArrayType UnknownType)
>      ,p "array_append(ARRAY['a','b'], 'c'::text)"
>         $ Right (ArrayType $ ScalarType "text")
>      ,p "array_append(ARRAY['a','b'::text], 'c')"
>         $ Right (ArrayType $ ScalarType "text")
>      ,p "array_append(ARRAY['a'::int,'b'], 'c')"
>         $ Right (ArrayType typeInt)
>      ]]

>   ,Group "cast expressions" [ Expressions [
>       p "cast ('1' as integer)"
>         $ Right typeInt
>      ,p "cast ('1' as baz)"
>         $ Left [UnknownTypeName "baz"]
>      ,p "array[]" -- this isn't quite right but not sure how to do it atm
>                   -- no point fixing this case since need a load of other
>                   -- test cases where the behaviour is different
>         $ Right (Pseudo AnyArray) -- Left [TypelessEmptyArray]
>      ,p "array[] :: text[]"
>         $ Right (ArrayType (ScalarType "text"))
>      ]]

>   ,Group "simple selects" [ StatementTypes [
>       p "select 1;" $ Right [Just $ StatementType [] [("?column?", typeInt)]]
>      ,p "select 1 as a;" $
>         Right [Just $ StatementType [] [("a", typeInt)]]
>      ,p "select 1,2;" $
>         Right [Just $ StatementType [] [("?column?", typeInt)
>                                                 ,("?column?", typeInt)]]
>      ,p "select 1 as a, 2 as b;" $
>         Right [Just $ StatementType [] [("a", typeInt)
>                                                 ,("b", typeInt)]]
>      ,p "select 1+2 as a, 'a' || 'b';" $
>         Right [Just $ StatementType [] [("a", typeInt)
>                                        ,("?column?", ScalarType "text")]]
>      ,p "values (1,2);" $ Right [Just $ StatementType []
>                                           [("column1", typeInt)
>                                           ,("column2", typeInt)]]
>      ,p "values (1,2),('3', '4');" $ Right [Just $ StatementType []
>                                                      [("column1", typeInt)
>                                                      ,("column2", typeInt)]]
>      ,p "values (1,2),('a', true);" $ Left [IncompatibleTypeSet [typeInt
>                                                                 ,typeBool]]
>      ,p "values ('3', '4'),(1,2);" $ Right [Just $ StatementType []
>                                                      [("column1", typeInt)
>                                                      ,("column2", typeInt)]]
>      ,p "values ('a', true),(1,2);" $ Left [IncompatibleTypeSet [typeBool
>                                                                 ,typeInt]]
>      ,p "values ('a'::text, '2'::int2),('1','2');" $ Right [Just $ StatementType []
>                                      [("column1", ScalarType "text")
>                                      ,("column2", typeSmallInt)]]
>      ,p "values (1,2,3),(1,2);" $ Left [ValuesListsMustBeSameLength]

>      ]]

>   ,Group "simple combine selects" [ StatementTypes [
>      p "select 1,2  union select '3', '4';" $ Right [Just $ StatementType []
>                                      [("?column?", typeInt)
>                                      ,("?column?", typeInt)]]
>      ,p "select 1,2 intersect select 'a', true;" $ Left [IncompatibleTypeSet [typeInt
>                                                         ,typeBool]]
>      ,p "select '3', '4' except select 1,2;" $ Right [Just $ StatementType []
>                                      [("?column?", typeInt)
>                                      ,("?column?", typeInt)]]
>      ,p "select 'a', true union select 1,2;"
>                                      $ Left [IncompatibleTypeSet [typeBool
>                                                         ,typeInt]]
>      ,p "select 'a'::text, '2'::int2 intersect select '1','2';" $ Right [Just $ StatementType []
>                                      [("text", ScalarType "text")
>                                      ,("int2", typeSmallInt)]]
>      ,p "select 1,2,3 except select 1,2;" $ Left [ValuesListsMustBeSameLength]
>      ,p "select '3' as a, '4' as b except select 1,2;" $ Right [Just $ StatementType []
>                                      [("a", typeInt)
>                                      ,("b", typeInt)]]

>      ]]

>   ,Group "simple selects from" [ StatementTypes [
>       p "select a from (select 1 as a, 2 as b) x;"
>         $ Right [Just $ StatementType [] [("a", typeInt)]]
>      ,p "select b from (select 1 as a, 2 as b) x;"
>         $ Right [Just $ StatementType [] [("b", typeInt)]]
>      ,p "select c from (select 1 as a, 2 as b) x;"
>         $ Left [UnrecognisedIdentifier "c"]
>      ,p "select typlen from pg_type;"
>         $ Right [Just $ StatementType [] [("typlen", typeSmallInt)]]
>      ,p "select oid from pg_type;"
>         $ Right [Just $ StatementType [] [("oid", ScalarType "oid")]]
>      ,p "select p.oid from pg_type p;"
>         $ Right [Just $ StatementType [] [("oid", ScalarType "oid")]]
>      ,p "select typlen from nope;"
>         $ Left [UnrecognisedIdentifier "typlen",UnrecognisedRelation "nope"]
>      ,p "select generate_series from generate_series(1,7);"
>         $ Right [Just $ StatementType [] [("generate_series", typeInt)]]

check aliasing

>      ,p "select generate_series.generate_series from generate_series(1,7);"
>         $ Right [Just $ StatementType [] [("generate_series", typeInt)]]
>      ,p "select g from generate_series(1,7) g;"
>         $ Right [Just $ StatementType [] [("g", typeInt)]]
>      ,p "select g.g from generate_series(1,7) g;"
>         $ Right [Just $ StatementType [] [("g", typeInt)]]
>      ,p "select generate_series.g from generate_series(1,7) g;"
>         $ Left [UnrecognisedCorrelationName "generate_series"]
>      ,p "select g.generate_series from generate_series(1,7) g;"
>         $ Left [UnrecognisedIdentifier "g.generate_series"]


>      ,p "select * from pg_attrdef;"
>         $ Right [Just $ StatementType []
>          [("adrelid",ScalarType "oid")
>          ,("adnum",ScalarType "int2")
>          ,("adbin",ScalarType "text")
>          ,("adsrc",ScalarType "text")]]
>      ,p "select abs from abs(3);"
>         $ Right [Just $ StatementType [] [("abs", typeInt)]]
>         --todo: these are both valid,
>         --the second one means select 3+generate_series from generate_series(1,7)
>         --  select generate_series(1,7);
>         -- select 3 + generate_series(1,7);

>      ]]

>   ,Group "simple selects from 2" [ CatUpStatementTypes [
>       t "select a,b from testfunc();"
>         [CatCreateComposite "testType" [("a", ScalarType "text")
>                                        ,("b", typeInt)
>                                        ,("c", typeInt)]
>         ,CatCreateFunction FunName "testfunc" []
>          (SetOfType $ NamedCompositeType "testType") False]
>         $ Right [Just $ StatementType []
>                  [("a",ScalarType "text"),("b",ScalarType "int4")]]

>      ,t "select testfunc();"
>         [CatCreateFunction FunName "testfunc" [] (Pseudo Void) False]
>         $ Right [Just $ StatementType [] []]

>      ]]

>   ,Group "simple join selects" [ StatementTypes [
>       p "select * from (select 1 as a, 2 as b) a\n\
>         \  cross join (select true as c, 4.5 as d) b;"
>         $ Right [Just $ StatementType [] [("a", typeInt)
>                                           ,("b", typeInt)
>                                           ,("c", typeBool)
>                                           ,("d", typeNumeric)]]
>      ,p "select * from (select 1 as a, 2 as b) a\n\
>         \  inner join (select true as c, 4.5 as d) b on true;"
>         $ Right [Just $ StatementType [] [("a", typeInt)
>                                           ,("b", typeInt)
>                                           ,("c", typeBool)
>                                           ,("d", typeNumeric)]]
>      ,p "select * from (select 1 as a, 2 as b) a\n\
>         \  inner join (select 1 as a, 4.5 as d) b using(a);"
>         $ Right [Just $ StatementType [] [("a", typeInt)
>                                           ,("b", typeInt)
>                                           ,("d", typeNumeric)]]
>      ,p "select * from (select 1 as a, 2 as b) a\n\
>         \ natural inner join (select 1 as a, 4.5 as d) b;"
>         $ Right [Just $ StatementType [] [("a", typeInt)
>                                           ,("b", typeInt)
>                                           ,("d", typeNumeric)]]
>         --check the attribute order
>      ,p "select * from (select 2 as b, 1 as a) a\n\
>         \ natural inner join (select 4.5 as d, 1 as a) b;"
>         $ Right [Just $ StatementType [] [("a", typeInt)
>                                           ,("b", typeInt)
>                                           ,("d", typeNumeric)]]
>      ,p "select * from (select 1 as a1, 2 as b) a\n\
>         \ natural inner join (select true as a1, 4.5 as d) b;"
>         $ Left [IncompatibleTypeSet [ScalarType "int4"
>                                      ,ScalarType "bool"]]
>      ,p "select * from (select 1 as a1, 2 as b) a\n\
>         \ natural inner join (select true as a1, 4.5 as d) b;"
>         $ Left [IncompatibleTypeSet [ScalarType "int4"
>                                      ,ScalarType "bool"]]

>      ,p "select * from (select 1 as a1) a, (select 2 as a2) b;"
>         $ Right [Just $ StatementType [] [("a1", typeInt)
>                                                                ,("a2", typeInt)]]

>      ,p "select * from (select 1 as a1) a, (select 2 as a1) b;"
>         $ Right [Just $ StatementType [] [("a1", typeInt)
>                                                                ,("a1", typeInt)]]

>      ,p "select a1 from (select 1 as a1) a,  (select 2 as a1) b;"
>         $ Left [AmbiguousIdentifier "a1"]
>      ]]

>   ,Group "simple scalar identifier qualification" [ StatementTypes [
>       p "select a.* from \n\
>         \(select 1 as a, 2 as b) a \n\
>         \cross join (select 3 as c, 4 as d) b;"
>         $ Right [Just $ StatementType [] [("a", typeInt)
>                                           ,("b", typeInt)]]
>      ,p "select nothere.* from \n\
>         \(select 1 as a, 2 as b) a \n\
>         \cross join (select 3 as c, 4 as d) b;"
>         $ Left [UnrecognisedCorrelationName "nothere"]
>      ,p "select a.b,b.c from \n\
>         \(select 1 as a, 2 as b) a \n\
>         \natural inner join (select 3 as a, 4 as c) b;"
>         $ Right [Just $ StatementType [] [("b", typeInt)
>                                           ,("c", typeInt)]]
>      ,p "select a.a,b.a from \n\
>         \(select 1 as a, 2 as b) a \n\
>         \natural inner join (select 3 as a, 4 as c) b;"
>         $ Right [Just $ StatementType [] [("a", typeInt)
>                                           ,("a", typeInt)]]

>      ,p "select pg_attrdef.adsrc from pg_attrdef;"
>         $ Right [Just $ StatementType [] [("adsrc", ScalarType "text")]]

>      ,p "select a.adsrc from pg_attrdef a;"
>         $ Right [Just $ StatementType [] [("adsrc", ScalarType "text")]]

>      ,p "select pg_attrdef.adsrc from pg_attrdef a;"
>         $ Left [UnrecognisedCorrelationName "pg_attrdef"]

>      ,p "select a from (select 2 as b, 1 as a) a\n\
>         \natural inner join (select 4.5 as d, 1 as a) b;"
>         $ Right [Just $ StatementType [] [("a", typeInt)]]

select g.fn from fn() g

>      ]]

>   ,Group "aggregates" [ StatementTypes [
>        p "select max(prorettype::int) from pg_proc;"
>         $ Right [Just $ StatementType [] [("max", typeInt)]]
>      ]]

>   ,Group "simple wheres" [ StatementTypes [
>       p "select 1 from pg_type where true;"
>         $ Right [Just $ StatementType [] [("?column?", typeInt)]]
>      ,p "select 1 from pg_type where 1;"
>         $ Left [ExpressionMustBeBool]
>      ,p "select typname from pg_type where typbyval;"
>         $ Right [Just $ StatementType [] [("typname", ScalarType "name")]]
>      ,p "select typname from pg_type where typtype = 'b';"
>         $ Right [Just $ StatementType [] [("typname", ScalarType "name")]]
>      ,p "select typname from pg_type where what = 'b';"
>         $ Left [UnrecognisedIdentifier "what"]
>      ]]

TODO: check identifier stacking working, then remove the pg_namespace
qualifier before oid and this should still work

>   ,Group "subqueries" [ StatementTypes [
>       p "select relname as relvar_name\n\
>         \    from pg_class\n\
>         \    where ((relnamespace =\n\
>         \           (select oid\n\
>         \              from pg_namespace\n\
>         \              where (nspname = 'public'))) and (relkind = 'r'));"
>         $ Right [Just $ StatementType [] [("relvar_name",ScalarType "name")]]
>      ,p "select relname from pg_class where relkind in ('r', 'v');"
>         $ Right [Just $ StatementType [] [("relname",ScalarType "name")]]
>      ,p "select * from generate_series(1,7) g\n\
>         \where g not in (select * from generate_series(3,5));"
>         $ Right [Just $ StatementType [] [("g",typeInt)]]
>      ,p "select 3 = any(array[1,2,3]);"
>         $ Right [Just $ StatementType [] [("?column?",typeBool)]]
>      ]]

identifiers in select parts

>{-    ,testGroup "select part identifiers"
>     (mapStatementTypes [
>       p "select relname,attname from pg_class\n\
>         \inner join pg_attribute\n\
>         \on pg_attribute.attrelid = pg_class.oid;"
>         $ Right [Just $ StatementType [] [("relvar_name",ScalarType "name")]]
>      ])-}

>   ,Group "insert" [ StatementTypes [
>       p "insert into nope (a,b) values (c,d);"
>         $ Left [UnrecognisedRelation "nope",UnrecognisedIdentifier "c",UnrecognisedIdentifier "d"]
>      ,p "insert into pg_attrdef (adrelid,adnum,adbin,adsrc)\n\
>         \values (1,2, 'a', 'b');"
>         $ Right [Just $ StatementType [] [] {-InsertInfo "pg_attrdef"
>                           [("adrelid",ScalarType "oid")
>                           ,("adnum",ScalarType "int2")
>                           ,("adbin",ScalarType "text")
>                           ,("adsrc",ScalarType "text")]-}]
>      ,p "insert into pg_attrdef\n\
>         \values (1,2, 'a', 'b');"
>         $ Right [Just $ StatementType [] [] {-InsertInfo "pg_attrdef"
>                           [("adrelid",ScalarType "oid")
>                           ,("adnum",ScalarType "int2")
>                           ,("adbin",ScalarType "text")
>                           ,("adsrc",ScalarType "text")]-}]
>      ,p "insert into pg_attrdef (hello,adnum,adbin,adsrc)\n\
>         \values (1,2, 'a', 'b');"
>         $ Left [UnrecognisedIdentifier "hello"]
>      ,p "insert into pg_attrdef (adrelid,adnum,adbin,adsrc)\n\
>         \values (1,true, 'a', 'b');"
>         $ Left [IncompatibleTypes (ScalarType "int2") (ScalarType "bool")]
>      ,p "insert into pg_attrdef (adrelid,adnum,adbin,adsrc)\n\
>         \values (1,true, 'a', 'b','c');"
>         $ Left [WrongNumberOfColumns]
>      ]]

>   ,Group "update" [ StatementTypes [
>       p "update nope set a = 1;"
>         $ Left [UnrecognisedRelation "nope"]
>      ,p "update pg_attrdef set adsrc = '' where 1;"
>         $ Left [ExpressionMustBeBool]
>      ,p "update pg_attrdef set (adbin,adsrc) = ('a','b','c');"
>         $ Left [WrongNumberOfColumns]
>      ,p "update pg_attrdef set (adrelid,adsrc) = (true,'b');"
>         $ Left [IncompatibleTypes (ScalarType "oid") typeBool]
>      ,p "update pg_attrdef set (shmadrelid,adsrc) = ('a','b');"
>         $ Left [UnrecognisedIdentifier "shmadrelid"]
>      ,p "update pg_attrdef set adsrc='';"
>         $ Right [Just $ StatementType [] [] {-UpdateInfo "pg_attrdef" [("adsrc",ScalarType "text")]-}]
>      ,p "update pg_attrdef set adsrc='' where 1=2;"
>         $ Right [Just $ StatementType [] [] {-UpdateInfo "pg_attrdef" [("adsrc",ScalarType "text")]-}]
>       -- TODO: actually, pg doesn't support this so need to generate error instead
>      ,p "update pg_attrdef set (adbin,adsrc) = ((select 'a','b'));"
>         $ Right [Just $ StatementType [] [] {-UpdateInfo "pg_attrdef" [("adbin",ScalarType "text"),("adsrc",ScalarType "text")]-}]
>      --check where ids
>      ,p "update pg_attrdef set adsrc='' where adsrc='';"
>         $ Right [Just $ StatementType [] [] {-UpdateInfo "pg_attrdef" [("adsrc",ScalarType "text")]-}]
>      ,p "update pg_attrdef set adnum = adnum + 1;"
>         $ Right [Just $ StatementType [] [] {-UpdateInfo "pg_attrdef" [("adnum",ScalarType "int2")]-}]
>      ]]

>   ,Group "delete" [ StatementTypes [
>       p "delete from nope;"
>         $ Left [UnrecognisedRelation "nope"]
>      ,p "delete from pg_attrdef where 1=2;"
>         $ Right [Just $ StatementType [] []]
>      ,p "delete from pg_attrdef where 1;"
>         $ Left [ExpressionMustBeBool]
>      ,p "delete from pg_attrdef where adsrc='';"
>         $ Right [Just $ StatementType [] []]
>      ]]

================================================================================

test the catalog updates from creates, etc.

>   ,Group "creates" [DdlStatements [
>       p "create table t1 (\n\
>         \   a int,\n\
>         \   b text\n\
>         \);"
>         [[CatCreateTable "t1" [("a",ScalarType "int4")
>                               ,("b",ScalarType "text")]
>                               [("tableoid", ScalarType "oid")
>                               ,("cmax", ScalarType "cid")
>                               ,("xmax", ScalarType "xid")
>                               ,("cmin", ScalarType "cid")
>                               ,("xmin", ScalarType "xid")
>                               ,("ctid", ScalarType "tid")]]]
>      ,p "create type t1 as (\n\
>         \   a int,\n\
>         \   b text\n\
>         \);"
>         [[CatCreateComposite "t1" [("a",ScalarType "int4")
>                                   ,("b",ScalarType "text")]]]

>      ,p "create domain t1 as text;"
>         [[CatCreateDomain (DomainType "t1") (ScalarType "text")]]

>      ,p "create domain t1 as text check (value in ('a', 'b'));\n\
>         \select 'text'::t1;"
>         [[CatCreateDomain (DomainType "t1") (ScalarType "text")]]


>      ,p "create view v1 as select * from pg_attrdef;"
>         [[CatCreateView "v1" [("adrelid",ScalarType "oid")
>                              ,("adnum",ScalarType "int2")
>                              ,("adbin",ScalarType "text")
>                              ,("adsrc",ScalarType "text")]]]

>      ,p "create function t1(text) returns text as $$\n\
>         \null;\n\
>         \$$ language sql stable;"
>         [[CatCreateFunction FunName "t1" [ScalarType "text"]
>                             (ScalarType "text") False]]
>      ,p "create language plpgsql;"
>         [[CatCreateFunction FunName "plpgsql_call_handler" [] (Pseudo LanguageHandler) False
>          ,CatCreateFunction FunName "plpgsql_validator" [ScalarType "oid"] (Pseudo Void) False]]
>      ]]


================================================================================

check the identifier resolution for functions:
parameters
variable declarations
select expressions inside these:
refer to param
refer to variable
override var with var
override var with select iden
todo: override var with param, override select iden with param

check var defs:
check type exists
check type of initial values

>   ,Group "create function identifier resolution" [ StatementTypes [
>       p "create function t1(stuff text) returns text as $$\n\
>         \begin\n\
>         \  return stuff || ' and stuff';\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         (Right [Nothing])
>      ,p "create function t1(stuff text) returns text as $$\n\
>         \begin\n\
>         \  return badstuff || ' and stuff';\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         (Left [UnrecognisedIdentifier "badstuff"])
>      ,p "create function t1() returns text as $$\n\
>         \declare\n\
>         \  stuff text;\n\
>         \begin\n\
>         \  return stuff || ' and stuff';\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         (Right [Nothing])
>      ]]

================================================================================

>   ,Group "plpgsqlbits" [ StatementTypes [
>       p "create function t1(stuff text) returns text as $$\n\
>         \begin\n\
>         \  return stuff || ' and stuff';\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         (Right [Nothing])
>      ,p "create function t1(stuff text) returns text as $$\n\
>         \begin\n\
>         \  return badstuff || ' and stuff';\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         (Left [UnrecognisedIdentifier "badstuff"])
>      ,p "create function t1() returns text as $$\n\
>         \declare\n\
>         \  stuff text;\n\
>         \begin\n\
>         \  return stuff || ' and stuff';\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         (Right [Nothing])
>      ]]

================================================================================

>   ,Group "plpgsqlbits" [ StatementTypes [
>       p "create function t1() returns void as $$\n\
>         \declare\n\
>         \  a bool;\n\
>         \begin\n\
>         \  a := 3;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         (Left [IncompatibleTypes (ScalarType "bool") (ScalarType "int4")])
>      ,p "create function t1() returns void as $$\n\
>         \declare\n\
>         \  a boolean;\n\
>         \begin\n\
>         \  a := true;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         (Right [Nothing])
>      ]]

================================================================================

>   ,Group "for loops" [ StatementTypes [
>       p "create function t1() returns void as $$\n\
>         \declare\n\
>         \  r record;\n\
>         \  t int;\n\
>         \begin\n\
>         \  for r in select * from pg_attrdef loop\n\
>         \    t := r.adnum;\n\
>         \  end loop;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         (Right [Nothing])


>      ,p "create function t1() returns void as $$\n\
>         \declare\n\
>         \  r record;\n\
>         \  t int;\n\
>         \begin\n\
>         \  for r in select adnum from pg_attrdef loop\n\
>         \    t := r;\n\
>         \  end loop;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         (Right [Nothing])

loop var already declared

>      ,p "create function test() returns void as $$\n\
>         \declare\n\
>         \  i int;\n\
>         \  i1 int;\n\
>         \begin\n\
>         \  for i in 0 .. 10 loop\n\
>         \    i1 := i;\n\
>         \  end loop;\n\
>         \end;\n\
>         \$$ language plpgsql volatile;"
>        (Right [Nothing])

implicitly created loop var

>      ,p "create function test() returns void as $$\n\
>         \declare\n\
>         \  i1 int;\n\
>         \begin\n\
>         \  for i in 0 .. 10 loop\n\
>         \    i1 := i;\n\
>         \  end loop;\n\
>         \end;\n\
>         \$$ language plpgsql volatile;"
>        (Right [Nothing])

loop var already declared, wrong type

>      ,p "create function test() returns void as $$\n\
>         \declare\n\
>         \  i bool;\n\
>         \begin\n\
>         \  for i in 0 .. 10 loop\n\
>         \    null;\n\
>         \  end loop;\n\
>         \end;\n\
>         \$$ language plpgsql volatile;"
>        (Left [IncompatibleTypes (ScalarType "bool") (ScalarType "int4")])

loop var implicit check it's type

>      ,p "create function test() returns void as $$\n\
>         \declare\n\
>         \  t bool;\n\
>         \begin\n\
>         \  for i in 0 .. 10 loop\n\
>         \    t := i;\n\
>         \  end loop;\n\
>         \end;\n\
>         \       $$ language plpgsql volatile;"
>         (Left [IncompatibleTypes (ScalarType "bool") (ScalarType "int4")])
>      ]]

================================================================================

>   ,Group "check catalog chaining" [ StatementTypes [

create function then select
select then create function
then in two separate chained asts

>       p "create function t1() returns void as $$\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \$$ language plpgsql stable;\n\
>         \select t1();"
>         (Right [Nothing])
>      ,p "select t1();\n\
>         \create function t1() returns void as $$\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         (Left [NoMatchingOperator "t1" []])
>      ]]

>   {-,Group "check catalog chaining2" [ StatementTypes [
>       p ["create function t1() returns void as $$\n\
>          \begin\n\
>          \  null;\n\
>          \end;\n\
>          \$$ language plpgsql stable;"
>         ,"select t1();"]
>         (Right [Just (SelectInfo (Pseudo Void))])
>      ,p ["select t1();"
>         ,"create function t1() returns void as $$\n\
>          \begin\n\
>          \  null;\n\
>          \end;\n\
>          \$$ language plpgsql stable;"]
>         (Left [NoMatchingOperator "t1" []])
>      ]]-}

================================================================================

test some casts
assign composite to record
  then assign record to composite
assign row to composite
 : check wrong cols, wrong types
check read and write fields in composite->record
check read and write fields in composite
check domain <-> base assigns
check call function with compatible composite, compatible row ctor
assign comp to comp

todo for chaos sql
for loop var = scalar, select = setof composite with one scalar

select into
composite assignment and equality
autocast from rowctor to composite when calling function
assignment to composite fields
read fields of composite

array_contains -> match anyelement
createtable as cat update
window functions
assign domain <-> base
sql function not working

================================================================================

check insert returning, update returning, delete returning, one check each
check select into: multiple vars, record (then access fields to check),
  composite var
check errors: select into wrong number of vars, wrong types, and into
  composite wrong number and wrong type

>   ,Group "select into" [ StatementTypes [
>       p "insert into pg_attrdef (adrelid,adnum,adbin,adsrc)\n\
>         \values (1,2, 'a', 'b') returning adnum,adbin;"
>         $ Right [Just $ StatementType [] [("adnum", ScalarType "int2")
>                                          ,("adbin", ScalarType "text")]]
>      ,p "update pg_attrdef set adnum = adnum + 1 returning adnum;"
>         $ Right [Just $ StatementType [] [("adnum", ScalarType "int2")]]
>      ,p "delete from pg_attrdef returning adnum,adbin;"
>         $ Right [Just $ StatementType [] [("adnum", ScalarType "int2")
>                                          ,("adbin", ScalarType "text")]]
>      ,p "create function t1() returns void as $$\n\
>         \declare\n\
>         \  a int;\n\
>         \  b text;\n\
>         \begin\n\
>         \  select adnum,adbin into a,b from pg_attrdef;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         $ Right [Nothing]
>      ,p "create function t1() returns void as $$\n\
>         \declare\n\
>         \  a int;\n\
>         \  b text;\n\
>         \begin\n\
>         \  select adnum,adbin into b,a from pg_attrdef;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         $ Left [IncompatibleTypes (ScalarType "text") (ScalarType "int2")
>                ,IncompatibleTypes (ScalarType "int4") (ScalarType "text")]
>      ,p "create function t1() returns void as $$\n\
>         \declare\n\
>         \  a int;\n\
>         \  b text;\n\
>         \begin\n\
>         \  select adnum,adbin into a,c from pg_attrdef;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         $ Left [UnrecognisedIdentifier "c"]
>      ,p "create function t1() returns void as $$\n\
>         \declare\n\
>         \  a int;\n\
>         \  b text;\n\
>         \begin\n\
>         \  select adnum,adbin into a from pg_attrdef;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         $ Left [WrongNumberOfColumns]
>      ,p "create function t1() returns void as $$\n\
>         \declare\n\
>         \  r record;\n\
>         \  a int;\n\
>         \  b text;\n\
>         \begin\n\
>         \  select adnum,adbin into r from pg_attrdef;\n\
>         \  a := r.adnum;\n\
>         \  b := r.adbin;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         $ Right [Nothing]
>      ,p "create function t1() returns void as $$\n\
>         \declare\n\
>         \  r record;\n\
>         \  a int;\n\
>         \  b text;\n\
>         \begin\n\
>         \  select adnum,adbin into r from pg_attrdef;\n\
>         \  a := r.adnum;\n\
>         \  b := r.adsrc;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         $ Left [UnrecognisedIdentifier "r.adsrc"]
>      ,p "create function t1() returns void as $$\n\
>         \declare\n\
>         \  r pg_attrdef;\n\
>         \begin\n\
>         \  select * into r from pg_attrdef;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         $ Right [Nothing]
>      ,p "create function t1() returns void as $$\n\
>         \declare\n\
>         \  r pg_class;\n\
>         \begin\n\
>         \  select adnum,adbin into r from pg_attrdef;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         $ Left [IncompatibleTypes (NamedCompositeType "pg_class") (AnonymousRecordType [ScalarType "int2",ScalarType "text"])]
>      ,p "create function t1() returns void as $$\n\
>         \declare\n\
>         \  r record;\n\
>         \begin\n\
>         \  select adnum,adbin into r from pg_attrdef;\n\
>         \  select relname into r from pg_class;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         $ Right [Nothing]
>      ]]

>   ,Group "composite elements" [ StatementTypes [
>       p "create function t1() returns void as $$\n\
>         \declare\n\
>         \  r pg_attrdef;\n\
>         \  a int;\n\
>         \  b text;\n\
>         \begin\n\
>         \  b = r.adsrc;\n\
>         \  r.adnum := a;\n\
>         \  b = r.adsrc;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         $ Right [Nothing]
>      ]]

>   ,Group "positional args" [ StatementTypes [
>       p "create function distance(int, int, int, int) returns float(24) as $$\n\
>         \  select (point($1, $2) <-> point($3, $4))::float(24) as result;\n\
>         \$$ language sql immutable;"
>         $ Right [Nothing]
>      ,p "create function distance(int, int, int, int) returns float(24) as $$\n\
>         \  select (point($1, $2) <-> point($3, $5))::float(24) as result;\n\
>         \$$ language sql immutable;"
>         $ Left [UnrecognisedIdentifier "$5"]
>      ]]

>   ,Group "window fns" [ StatementTypes [
>       p "select *, row_number() over () from pg_attrdef;"
>         $ Right [Just $ StatementType []
>                   [("adrelid",ScalarType "oid")
>                   ,("adnum",ScalarType "int2")
>                   ,("adbin",ScalarType "text")
>                   ,("adsrc",ScalarType "text")
>                   ,("row_number",ScalarType "int8")]]
>      ]]

>   ,Group "drop stuff" [ DdlStatementsCat [
>       p "create function test(a int) returns void as $$\n\
>         \begin\n\
>         \  null;\n\
>         \end\n\
>         \$$ language plpgsql;"
>         [CatCreateFunction FunName "test" [typeInt] (Pseudo Void) False]
>      ,p "create function test(a int) returns void as $$\n\
>         \begin\n\
>         \  null;\n\
>         \end\n\
>         \$$ language plpgsql;\n\
>         \drop function test(int);"
>         []
>      ,p "drop function test(int);" -- this should fail but doesn't
>         []
>      ,p "drop function if exists test(int);"
>         []
>      ]]
>   ]

================================================================================

> p :: t -> t1 -> (t, t1)
> p a b = (a,b)

> t :: t -> u -> v -> (t,u,v)
> t a b c = (a,b,c)


> testExpressionType :: String -> Either [TypeError] Type -> Test.Framework.Test
> testExpressionType src typ = testCase ("typecheck " ++ src) $
>   let ast = case parseExpression "" src of
>                                      Left e -> error $ show e
>                                      Right l -> l
>       aast = typeCheckExpression defaultTemplate1Catalog ast
>       ty = getTopLevelTypes [aast]
>       er = concatMap snd $ getTypeErrors aast
>   in case (length er, length ty) of
>        (0,0) -> assertFailure "didn't get any types?"
>        (0,1) -> assertEqual ("typecheck " ++ src) typ $ Right $ head ty
>        (0,_) -> assertFailure "got too many types"
>        _ -> assertEqual ("typecheck " ++ src) typ $ Left er

> testStatementType :: String -> Either [TypeError] [Maybe StatementType] -> Test.Framework.Test
> testStatementType src sis = testCase ("typecheck " ++ src) $
>   let ast = case parseSql "" src of
>                               Left e -> error $ show e
>                               Right l -> l
>       aast = snd $ typeCheck defaultTemplate1Catalog ast
>       is = getTopLevelInfos aast
>       er = concatMap snd $ getTypeErrors aast
>   in {-trace (show aast) $-} case (length er, length is) of
>        (0,0) -> assertFailure "didn't get any infos?"
>        (0,_) -> assertEqual ("typecheck " ++ src) sis $ Right is
>        _ -> assertEqual ("typecheck " ++ src) sis $ Left er


> testCatUpStatementType :: String
>                        -> [CatalogUpdate]
>                        -> Either [TypeError] [Maybe StatementType]
>                        -> Test.Framework.Test
> testCatUpStatementType src eu sis = testCase ("typecheck " ++ src) $
>   let ast = case parseSql "" src of
>                               Left e -> error $ show e
>                               Right l -> l
>       aast = snd $ typeCheck makeCat ast
>       is = getTopLevelInfos aast
>       er = concatMap snd $ getTypeErrors aast
>   in {-trace (show aast) $-} case (length er, length is) of
>        (0,0) -> assertFailure "didn't get any infos?"
>        (0,_) -> assertEqual ("typecheck " ++ src) sis $ Right is
>        _ -> assertEqual ("typecheck " ++ src) sis $ Left er
>   where
>     makeCat = case updateCatalog defaultTemplate1Catalog eu of
>                         Left x -> error $ show x
>                         Right e -> e

> testCatUp :: String -> [[CatalogUpdate]] -> Test.Framework.Test
> testCatUp src eu = testCase ("typecheck " ++ src) $
>   let ast = case parseSql "" src of
>                               Left e -> error $ show e
>                               Right l -> l
>       aast = snd $ typeCheck defaultTemplate1Catalog ast
>       er = concatMap snd $ getTypeErrors aast
>       eu' = getTopLevelCatUpdates aast
>   in {-trace (show aast) $-} case (length er, length eu') of
>        (0,0) -> assertFailure "didn't get any infos or catupdates?"
>        (0,_) -> assertEqual ("eu " ++ src) eu eu'
>        (_,_) -> assertFailure $ show er


> testCat :: String -> [CatalogUpdate] -> Test.Framework.Test
> testCat src eu = testCase ("check catalog: " ++ src) $
>   let ast = case parseSql "" src of
>                               Left e -> error $ show e
>                               Right l -> l
>       (ncat,aast) = typeCheck defaultTemplate1Catalog ast
>       er = concatMap snd $ getTypeErrors aast
>       neu = deconstructCatalog ncat \\ deconstructCatalog defaultTemplate1Catalog
>   in if not (null er)
>        then assertFailure $ show er
>        else assertEqual "check eus" eu neu


> itemToTft :: Item -> [Test.Framework.Test]
> itemToTft (Expressions es) = map (uncurry testExpressionType) es
> itemToTft (StatementTypes es) = map (uncurry testStatementType) es
> itemToTft (CatUpStatementTypes es) = map (\(s,eu,si) -> testCatUpStatementType s eu si) es
> itemToTft (DdlStatements es) = map (uncurry testCatUp) es
> itemToTft (DdlStatementsCat es) = map (uncurry testCat) es

> itemToTft (Group s is) = [testGroup s $ concatMap itemToTft is]
