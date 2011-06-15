
Set of tests to check the type checking code. Includes tests for the
errors for sql which doesn't type check.

> {-# LANGUAGE QuasiQuotes #-}
> module Database.HsSqlPpp.Tests.TypeCheckTests
>     (typeCheckTests
>     ,typeCheckTestData
>     ,Item(..)) where
>
> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> import Data.List
> import Data.Generics.Uniplate.Data
>
> import Database.HsSqlPpp.Utils.Here
> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.TypeChecker
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.SqlTypes
> import Text.Groom
> import Database.HsSqlPpp.Tests.TestUtils
>
> data Item = Group String [Item]
>           | Expr String (Either [TypeError] Type)
>           | StmtType String (Either [TypeError] [Maybe StatementType])
>           | CatStmtType String [CatalogUpdate] (Either [TypeError] [Maybe StatementType])
>           | Ddl String [CatalogUpdate]
>
> typeCheckTests :: Test.Framework.Test
> typeCheckTests = itemToTft typeCheckTestData
>
> typeCheckTestData :: Item
> typeCheckTestData =
>   Group "astCheckTests" [
>
>    Group "basic literal types" [
>       e "1" $ Right typeInt
>      ,e "1.0" $ Right typeNumeric
>      ,e "'test'" $ Right UnknownType
>      ,e "true" $ Right typeBool
>      ,e "array[1,2,3]" $ Right (ArrayType typeInt)
>      ,e "array['a','b']" $ Right (ArrayType UnknownType)
>      ,e "array['a'::text,'b']" $ Right (ArrayType (ScalarType "text"))
>      ,e "array['a','b'::text]" $ Right (ArrayType (ScalarType "text"))
>      ,e "array[1,'b']" $ Right (ArrayType typeInt)
>      ,e "array[1,true]" $ Left [NoMatchingOperator "!arrayctor" [ScalarType "int4",ScalarType "bool"]]
>      ]

>   ,Group "some expressions" [
>       e "1=1" $ Right typeBool
>      ,e "1=true" $ Left [NoMatchingOperator "=" [typeInt,typeBool]]
>      ,e "substring('aqbc' from 2 for 2)" $ Right (ScalarType "text")
>
>      ,e "substring(3 from 2 for 2)" $ Left
>                                      [NoMatchingOperator "!substring"
>                                       [ScalarType "int4"
>                                       ,ScalarType "int4"
>                                       ,ScalarType "int4"]]
>      ,e "substring('aqbc' from 2 for true)" $ Left
>                     [NoMatchingOperator "!substring"
>                      [UnknownType
>                      ,ScalarType "int4"
>                      ,ScalarType "bool"]]
>
>      ,e "3 between 2 and 4" $ Right typeBool
>      ,e "3 between true and 4" $ Left
>                                [IncompatibleTypeSet [ScalarType "int4"
>                                                     ,ScalarType "bool"
>                                                     ,ScalarType "int4"]]
>
>      ,e "array[1,2,3][2]" $ Right typeInt
>      ,e "array['a','b'][1]" $ Right UnknownType
>      ,e "array['a'::text,'b'][1]" $ Right (ScalarType "text")
>      {-,p "array['a','b'][true]" (TypeError ("",0,0)
>                                   (WrongType
>                                    typeInt
>                                    UnknownType))-}
>      ,e "not true" $ Right typeBool
>      ,e "not 1" $ Left
>                  [NoMatchingOperator "!not" [typeInt]]
>
>      ,e "@ 3" $ Right typeInt
>      ,e "@ true" $ Left
>                  [NoMatchingOperator "@" [ScalarType "bool"]]
>
>      ,e "-3" $ Right typeInt
>      ,e "-'a'" $ Left
>                  [NoMatchingOperator "-" [UnknownType]]
>
>      ,e "4-3" $ Right typeInt
>
>      ,e "1 is null" $ Right typeBool
>      ,e "1 is not null" $ Right typeBool
>
>      ,e "1+1" $ Right typeInt
>      ,e "1+1" $ Right typeInt
>      ,e "31*511" $ Right typeInt
>      ,e "5/2" $ Right typeInt
>      ,e "2^10" $ Right typeFloat8
>      ,e "17%5" $ Right typeInt
>      ,e "3 and 4" $ Left
>                   [NoMatchingOperator "!and" [typeInt,typeInt]]
>      ,e "True and False" $ Right typeBool
>      ,e "false or true" $ Right typeBool
>      ,e "lower('TEST')" $ Right (ScalarType "text")
>      ,e "lower(1)" $ Left [NoMatchingOperator "lower" [typeInt]]
>      ]

>   ,Group "special functions" [
>       e "coalesce(null,1,2,null)" $ Right typeInt
>      ,e "coalesce('3',1,2,null)" $ Right typeInt
>      ,e "coalesce('3',1,true,null)"
>             $ Left [NoMatchingOperator "coalesce" [UnknownType
>                                                   ,ScalarType "int4"
>                                                   ,ScalarType "bool"
>                                                   ,UnknownType]]
>      ,e "nullif('hello','hello')" $ Right UnknownType
>      ,e "nullif('hello'::text,'hello')" $ Right (ScalarType "text")
>      ,e "nullif(3,4)" $ Right typeInt
>      ,e "nullif(true,3)"
>             $ Left [NoMatchingOperator "nullif" [ScalarType "bool"
>                                           ,ScalarType "int4"]]
>      ,e "greatest(3,5,6,4,3)" $ Right typeInt
>      ,e "least(3,5,6,4,3)" $ Right typeInt
>      ,e "least(5,true)"
>             $ Left [NoMatchingOperator "least" [ScalarType "int4",ScalarType "bool"]]
>      ]

~~~~
implicit casting and function/operator choice tests:
check when multiple implicit and one exact match on num args
check multiple matches with num args, only one with implicit conversions
check multiple implicit matches with one preferred
check multiple implicit matches with one preferred highest count
check casts from unknown string lits
~~~~

>   ,Group "some expressions" [
>       e "3 + '4'" $ Right typeInt
>      ,e "3.0 + '4'" $ Right typeNumeric
>      ,e "'3' + '4'" $ Left [NoMatchingOperator "+" [UnknownType
>                                               ,UnknownType]]
>      ]
>   ,Group "exists expressions" [
>       e "exists (select 1 from pg_type)" $ Right typeBool
>      ,e "exists (select testit from pg_type)"
>        $ Left [UnrecognisedIdentifier "testit"]
>      ]
>   ,Group "row comparison expressions" [

~~~~
rows different lengths
rows match types pairwise, same and different types
rows implicit cast from unknown
rows don't match types
~~~~

>       e "row(1)" $ Right (AnonymousRecordType [typeInt])
>      ,e "row(1,2)" $ Right (AnonymousRecordType [typeInt,typeInt])
>      ,e "row('t1','t2')" $ Right (AnonymousRecordType [UnknownType,UnknownType])
>      ,e "row(true,1,'t3')" $ Right (AnonymousRecordType [typeBool, typeInt,UnknownType])
>      ,e "(true,1,'t3',75.3)" $ Right (AnonymousRecordType [typeBool,typeInt
>                                       ,UnknownType,typeNumeric])
>      ,e "row(1) = row(2)" $ Right typeBool
>      ,e "row(1,2) = row(2,1)" $ Right typeBool
>      ,e "(1,2) = (2,1)" $ Right typeBool
>      ,e "(1,2,3) = (2,1)" $ Left [NoMatchingOperator "="
>                                   [AnonymousRecordType [ScalarType "int4"
>                                                        ,ScalarType "int4"
>                                                        ,ScalarType "int4"]
>                                    ,AnonymousRecordType [ScalarType "int4"
>                                                         ,ScalarType "int4"]]]
>      ,e "('1',2) = (2,'1')" $ Right typeBool
>      ,e "(1,true) = (2,1)" $ Left [NoMatchingOperator "="
>                                    [AnonymousRecordType [ScalarType "int4"
>                                                         ,ScalarType "bool"]
>                                    ,AnonymousRecordType [ScalarType "int4"
>                                                         ,ScalarType "int4"]]]
>      ,e "(1,2) <> (2,1)" $ Right typeBool
>      ,e "(1,2) >= (2,1)" $ Right typeBool
>      ,e "(1,2) <= (2,1)" $ Right typeBool
>      ,e "(1,2) > (2,1)" $ Right typeBool
>      ,e "(1,2) < (2,1)" $ Right typeBool
>      ]
>   ,Group "case expressions" [
>       e "case\n\
>         \ when true then 1\n\
>         \end" $ Right typeInt
>      ,e "case\n\
>         \ when 1=2 then 'stuff'\n\
>         \ when 2=3 then 'blah'\n\
>         \ else 'test'\n\
>         \end" $ Right UnknownType
>      ,e "case\n\
>         \ when 1=2 then 'stuff'\n\
>         \ when 2=3 then 'blah'\n\
>         \ else 'test'::text\n\
>         \end" $ Right $ ScalarType "text"
>      ,e "case\n\
>         \ when 1=2 then 'stuff'\n\
>         \ when true=3 then 'blah'\n\
>         \ else 'test'\n\
>         \end" $ Left [NoMatchingOperator "=" [typeBool,typeInt]]
>      ,e "case\n\
>         \ when 1=2 then true\n\
>         \ when 2=3 then false\n\
>         \ else 1\n\
>         \end" $ Left [IncompatibleTypeSet [typeBool
>                                         ,typeBool
>                                         ,typeInt]]
>      ,e "case\n\
>         \ when 1=2 then false\n\
>         \ when 2=3 then 1\n\
>         \ else true\n\
>         \end" $ Left [IncompatibleTypeSet [typeBool
>                                           ,typeInt
>                                           ,typeBool]]
>      ,e "case 1 when 2 then 3 else 4 end" $ Right typeInt
>      ,e "case 1 when true then 3 else 4 end"
>             $ Left [IncompatibleTypeSet [ScalarType "int4"
>                                          ,ScalarType "bool"]]
>      ,e "case 1 when 2 then true else false end" $ Right typeBool
>      ,e "case 1 when 2 then 3 else false end"
>             $ Left [IncompatibleTypeSet [ScalarType "int4"
>                                          ,ScalarType "bool"]]
>      ]

>   ,Group "polymorphic functions" [
>       e "array_append(ARRAY[1,2], 3)"
>         $ Right (ArrayType typeInt)
>      ,e "array_append(ARRAY['a','b'], 'c')"
>         $ Right (ArrayType UnknownType)
>      ,e "array_append(ARRAY['a','b'], 'c'::text)"
>         $ Right (ArrayType $ ScalarType "text")
>      ,e "array_append(ARRAY['a','b'::text], 'c')"
>         $ Right (ArrayType $ ScalarType "text")
>      ,e "array_append(ARRAY['a'::int,'b'], 'c')"
>         $ Right (ArrayType typeInt)
>      ]

>   ,Group "cast expressions" [
>       e "cast ('1' as integer)"
>         $ Right typeInt
>      ,e "cast ('1' as baz)"
>         $ Left [UnknownTypeName "baz"]
>      ,e "array[]" -- this isn't quite right but not sure how to do it atm
>                   -- no point fixing this case since need a load of other
>                   -- test cases where the behaviour is different
>         $ Right (Pseudo AnyArray) -- Left [TypelessEmptyArray]
>      ,e "array[] :: text[]"
>         $ Right (ArrayType (ScalarType "text"))
>      ]

>   ,Group "simple selects" [
>       s "select 1;" $ Right [Just $ ([], [("?column?", typeInt)])]
>      ,s "select 1 as a;" $
>         Right [Just $ ([], [("a", typeInt)])]
>      ,s "select 1,2;" $
>         Right [Just $ ([], [("?column?", typeInt)
>                                                 ,("?column?", typeInt)])]
>      ,s "select 1 as a, 2 as b;" $
>         Right [Just $ ([], [("a", typeInt)
>                                                 ,("b", typeInt)])]
>      ,s "select 1+2 as a, 'a' || 'b';" $
>         Right [Just $ ([], [("a", typeInt)
>                                        ,("?column?", ScalarType "text")])]
>      ,s "values (1,2);" $ Right [Just $ ([],
>                                           [("column1", typeInt)
>                                           ,("column2", typeInt)])]
>      ,s "values (1,2),('3', '4');" $ Right [Just $ ([],
>                                                      [("column1", typeInt)
>                                                      ,("column2", typeInt)])]
>      ,s "values (1,2),('a', true);" $ Left [IncompatibleTypeSet [typeInt
>                                                                 ,typeBool]]
>      ,s "values ('3', '4'),(1,2);" $ Right [Just $ ([],
>                                                      [("column1", typeInt)
>                                                      ,("column2", typeInt)])]
>      ,s "values ('a', true),(1,2);" $ Left [IncompatibleTypeSet [typeBool
>                                                                 ,typeInt]]
>      ,s "values ('a'::text, '2'::int2),('1','2');" $ Right [Just $ ([],
>                                      [("column1", ScalarType "text")
>                                      ,("column2", typeSmallInt)])]
>      ,s "values (1,2,3),(1,2);" $ Left [ValuesListsMustBeSameLength]
>      ]

>   ,Group "simple combine selects" [
>      s "select 1,2  union select '3', '4';" $ Right [Just $ ([],
>                                      [("?column?", typeInt)
>                                      ,("?column?", typeInt)])]
>      ,s "select 1,2 intersect select 'a', true;" $ Left [IncompatibleTypeSet [typeInt
>                                                         ,typeBool]]
>      ,s "select '3', '4' except select 1,2;" $ Right [Just $ ([],
>                                      [("?column?", typeInt)
>                                      ,("?column?", typeInt)])]
>      ,s "select 'a', true union select 1,2;"
>                                      $ Left [IncompatibleTypeSet [typeBool
>                                                         ,typeInt]]
>      ,s "select 'a'::text, '2'::int2 intersect select '1','2';" $ Right [Just $ ([],
>                                      [("text", ScalarType "text")
>                                      ,("int2", typeSmallInt)])]
>      ,s "select 1,2,3 except select 1,2;" $ Left [ValuesListsMustBeSameLength]
>      ,s "select '3' as a, '4' as b except select 1,2;" $ Right [Just $ ([],
>                                      [("a", typeInt)
>                                      ,("b", typeInt)])]
>      ,s [$here|
>          with a as (select 1 as a1),
>               b as (select * from a)
>               select * from a
>               union select * from b; |]
>          $ Right [Just $ ([],
>                            [("a1", typeInt)])]
>      ]

>   ,Group "simple selects from" [
>       s "select a from (select 1 as a, 2 as b) x;"
>         $ Right [Just $ ([], [("a", typeInt)])]
>      ,s "select b from (select 1 as a, 2 as b) x;"
>         $ Right [Just $ ([], [("b", typeInt)])]
>      ,s "select c from (select 1 as a, 2 as b) x;"
>         $ Left [UnrecognisedIdentifier "c"]
>      ,s "select typlen from pg_type;"
>         $ Right [Just $ ([], [("typlen", typeSmallInt)])]
>      ,s "select oid from pg_type;"
>         $ Right [Just $ ([], [("oid", ScalarType "oid")])]
>      ,s "select p.oid from pg_type p;"
>         $ Right [Just $ ([], [("oid", ScalarType "oid")])]
>      ,s "select typlen from nope;"
>         $ Left [UnrecognisedRelation "nope"]
>      ,s "select generate_series from generate_series(1,7);"
>         $ Right [Just $ ([], [("generate_series", typeInt)])]
>
>      -- check aliasing
>      ,s "select generate_series.generate_series from generate_series(1,7);"
>         $ Right [Just $ ([], [("generate_series", typeInt)])]
>      ,s "select g from generate_series(1,7) g;"
>         $ Right [Just $ ([], [("g", typeInt)])]
>      ,s "select g.g from generate_series(1,7) g;"
>         $ Right [Just $ ([], [("g", typeInt)])]
>      ,s "select generate_series.g from generate_series(1,7) g;"
>         $ Left [UnrecognisedCorrelationName "generate_series"]
>      ,s "select g.generate_series from generate_series(1,7) g;"
>         $ Left [UnrecognisedIdentifier "g.generate_series"]
>
>      ,s "select * from pg_attrdef;"
>         $ Right [Just $ ([],
>          [("adrelid",ScalarType "oid")
>          ,("adnum",ScalarType "int2")
>          ,("adbin",ScalarType "text")
>          ,("adsrc",ScalarType "text")])]
>      ,s "select abs from abs(3);"
>         $ Right [Just $ ([], [("abs", typeInt)])]
>         --todo: these are both valid,
>         --the second one means select 3+generate_series from generate_series(1,7)
>         --  select generate_series(1,7);
>         -- select 3 + generate_series(1,7);
>      ]

>   ,Group "simple selects from 2" [
>       c "select a,b from testfunc();"
>         [CatCreateComposite "testType" [("a", ScalarType "text")
>                                        ,("b", typeInt)
>                                        ,("c", typeInt)]
>         ,CatCreateFunction FunName "testfunc" []
>          (SetOfType $ NamedCompositeType "testType") False]
>         $ Right [Just $ ([],
>                  [("a",ScalarType "text"),("b",ScalarType "int4")])]
>
>      ,c "select testfunc();"
>         [CatCreateFunction FunName "testfunc" [] (Pseudo Void) False]
>         $ Right [Just $ ([], [])]
>      ]

>   {-currently broken, needs fixing
>     ,Group "simple join selects" [
>       s "select * from (select 1 as a, 2 as b) a\n\
>         \  cross join (select true as c, 4.5 as d) b;"
>         $ Right [Just $ ([], [("a", typeInt)
>                              ,("b", typeInt)
>                              ,("c", typeBool)
>                              ,("d", typeNumeric)])]
>      ,s "select * from (select 1 as a, 2 as b) a\n\
>         \  inner join (select true as c, 4.5 as d) b on true;"
>         $ Right [Just $ ([], [("a", typeInt)
>                              ,("b", typeInt)
>                              ,("c", typeBool)
>                              ,("d", typeNumeric)])]
>      ,s "select * from (select 1 as a, 2 as b) a\n\
>         \  inner join (select 1 as a, 4.5 as d) b using(a);"
>         $ Right [Just $ ([], [("a", typeInt)
>                              ,("b", typeInt)
>                              ,("d", typeNumeric)])]
>      ,s "select * from (select 1 as a, 2 as b) a\n\
>         \ natural inner join (select 1 as a, 4.5 as d) b;"
>         $ Right [Just $ ([], [("a", typeInt)
>                              ,("b", typeInt)
>                              ,("d", typeNumeric)])]
>         --check the attribute order
>      ,s "select * from (select 2 as b, 1 as a) a\n\
>         \ natural inner join (select 4.5 as d, 1 as a) b;"
>         $ Right [Just $ ([], [("a", typeInt)
>                              ,("b", typeInt)
>                              ,("d", typeNumeric)])]
>      ,s "select * from (select 1 as a1, 2 as b) a\n\
>         \ natural inner join (select true as a1, 4.5 as d) b;"
>         $ Left [UnrecognisedCorrelationName ""
>                ,IncompatibleTypeSet [ScalarType "int4"
>                                     ,ScalarType "bool"]]
>        ,s "select * from (select 1 as a1, 2 as b) a\n\
>         \ natural inner join (select true as a1, 4.5 as d) b;"
>         $ Left [UnrecognisedCorrelationName ""
>                ,IncompatibleTypeSet [ScalarType "int4"
>                                     ,ScalarType "bool"]]
>        ,s "select * from (select 1 as a1) a, (select 2 as a2) b;"
>         $ Right [Just $ ([], [("a1", typeInt)
>                                            ,("a2", typeInt)])]
>
>        ,s "select * from (select 1 as a1) a, (select 2 as a1) b;"
>         $ Right [Just $ ([], [("a1", typeInt)
>                                            ,("a1", typeInt)])]
>
>        ,s "select a1 from (select 1 as a1) a,  (select 2 as a1) b;"
>         $ Left [AmbiguousIdentifier "a1"]
>      ]-}

>   ,Group "simple scalar identifier qualification" [
>       s "select a.* from \n\
>         \(select 1 as a, 2 as b) a \n\
>         \cross join (select 3 as c, 4 as d) b;"
>         $ Right [Just $ ([], [("a", typeInt)
>                                           ,("b", typeInt)])]
>      ,s "select nothere.* from \n\
>         \(select 1 as a, 2 as b) a \n\
>         \cross join (select 3 as c, 4 as d) b;"
>         $ Left [UnrecognisedCorrelationName "nothere"]
>      {- currently broken, needs fixing
>        ,s "select a.b,b.c from \n\
>         \(select 1 as a, 2 as b) a \n\
>         \natural inner join (select 3 as a, 4 as c) b;"
>         $ Right [Just $ ([], [("b", typeInt)
>                                           ,("c", typeInt)])-}
>      {- currently broken, needs fixing
>        ,s "select a.a,b.a from \n\
>         \(select 1 as a, 2 as b) a \n\
>         \natural inner join (select 3 as a, 4 as c) b;"
>         $ Right [Just $ ([], [("a", typeInt)
>                                           ,("a", typeInt)])]-}
>
>      ,s "select pg_attrdef.adsrc from pg_attrdef;"
>         $ Right [Just $ ([], [("adsrc", ScalarType "text")])]
>
>      ,s "select a.adsrc from pg_attrdef a;"
>         $ Right [Just $ ([], [("adsrc", ScalarType "text")])]
>
>      ,s "select pg_attrdef.adsrc from pg_attrdef a;"
>         $ Left [UnrecognisedCorrelationName "pg_attrdef"]
>
>      ,s "select a from (select 2 as b, 1 as a) a\n\
>         \natural inner join (select 4.5 as d, 1 as a) b;"
>         $ Right [Just $ ([], [("a", typeInt)])]
>
> -- select g.fn from fn() g
>
>      ]

>   ,Group "aggregates" [
>        s "select max(prorettype::int) from pg_proc;"
>         $ Right [Just $ ([], [("max", typeInt)])]
>      ]

>   ,Group "simple wheres" [
>       s "select 1 from pg_type where true;"
>         $ Right [Just $ ([], [("?column?", typeInt)])]
>      ,s "select 1 from pg_type where 1;"
>         $ Left [ExpressionMustBeBool]
>      ,s "select typname from pg_type where typbyval;"
>         $ Right [Just $ ([], [("typname", ScalarType "name")])]
>      ,s "select typname from pg_type where typtype = 'b';"
>         $ Right [Just $ ([], [("typname", ScalarType "name")])]
>      ,s "select typname from pg_type where what = 'b';"
>         $ Left [UnrecognisedIdentifier "what"]
>      ]

>   ,Group "unnest" [
>       s "select conname,unnest(conkey) as attnum from pg_constraint;"
>         $ Right [Just (([], [("conname",ScalarType "name")
>                                          ,("attnum",typeSmallInt)]))]
>      ]

TODO: check identifier stacking working, then remove the pg_namespace
qualifier before oid and this should still work

>   ,Group "subqueries" [
>       s "select relname as relvar_name\n\
>         \    from pg_class\n\
>         \    where ((relnamespace =\n\
>         \           (select oid\n\
>         \              from pg_namespace\n\
>         \              where (nspname = 'public'))) and (relkind = 'r'));"
>         $ Right [Just $ ([], [("relvar_name",ScalarType "name")])]
>      ,s "select relname from pg_class where relkind in ('r', 'v');"
>         $ Right [Just $ ([], [("relname",ScalarType "name")])]
>      ,s "select * from generate_series(1,7) g\n\
>         \where g not in (select * from generate_series(3,5));"
>         $ Right [Just $ ([], [("g",typeInt)])]
>      ,s "select 3 = any(array[1,2,3]);"
>         $ Right [Just $ ([], [("?column?",typeBool)])]
>      ]
>
>  -- identifiers in select parts
>
> {-    ,testGroup "select part identifiers"
>     (mapStatementTypes [
>       p "select relname,attname from pg_class\n\
>         \inner join pg_attribute\n\
>         \on pg_attribute.attrelid = pg_class.oid;"
>         $ Right [Just $ ([], [("relvar_name",ScalarType "name")]]
>      ])-}

>   ,Group "insert" [
>       s "insert into nope (a,b) values (c,d);"
>         $ Left [UnrecognisedRelation "nope"
>                ,UnrecognisedIdentifier "c"
>                ,UnrecognisedIdentifier "d"]
>      ,s "insert into pg_attrdef (adrelid,adnum,adbin,adsrc)\n\
>         \values (1,2, 'a', 'b');"
>         $ Right [Just $ ([], [] {-InsertInfo "pg_attrdef"
>                           [("adrelid",ScalarType "oid")
>                           ,("adnum",ScalarType "int2")
>                           ,("adbin",ScalarType "text")
>                           ,("adsrc",ScalarType "text")]-})]
>      ,s "insert into pg_attrdef\n\
>         \values (1,2, 'a', 'c');"
>         $ Right [Just $ ([], [] {-InsertInfo "pg_attrdef"
>                           [("adrelid",ScalarType "oid")
>                           ,("adnum",ScalarType "int2")
>                           ,("adbin",ScalarType "text")
>                           ,("adsrc",ScalarType "text")]-})]
>      ,s "insert into pg_attrdef (hello,adnum,adbin,adsrc)\n\
>         \values (1,2, 'a', 'b');"
>         $ Left [UnrecognisedIdentifier "hello"]
>      ,s "insert into pg_attrdef (adrelid,adnum,adbin,adsrc)\n\
>         \values (1,true, 'a', 'b');"
>         $ Left [IncompatibleTypes (ScalarType "int2") (ScalarType "bool")]
>      ,s "insert into pg_attrdef (adrelid,adnum,adbin,adsrc)\n\
>         \values (1,true, 'a', 'b','c');"
>         $ Left [WrongNumberOfColumns]
>      ]

>   ,Group "update" [
>       s "update nope set a = 1;"
>         $ Left [UnrecognisedRelation "nope"
>                ,UnrecognisedIdentifier "a"]
>      ,s "update pg_attrdef set adsrc = '' where 1;"
>         $ Left [ExpressionMustBeBool]
>      ,s "update pg_attrdef set (adbin,adsrc) = ('a','b','c');"
>         $ Left [NoMatchingOperator "="
>                 [AnonymousRecordType [ScalarType "text"
>                                      ,ScalarType "text"]
>                 ,AnonymousRecordType [UnknownType
>                                      ,UnknownType
>                                      ,UnknownType]]]
>      ,s "update pg_attrdef set (adrelid,adsrc) = (true,'b');"
>         $ Left [NoMatchingOperator "="
>                 [AnonymousRecordType [ScalarType "oid"
>                                      ,ScalarType "text"]
>                 ,AnonymousRecordType [ScalarType "bool"
>                                      ,UnknownType]]]
>      ,s "update pg_attrdef set (shmadrelid,adsrc) = ('a','b');"
>         $ Left [UnrecognisedIdentifier "shmadrelid"]
>      ,s "update pg_attrdef set adsrc='';"
>         $ Right [Just $ ([], [] {-UpdateInfo "pg_attrdef" [("adsrc",ScalarType "text")]-})]
>      ,s "update pg_attrdef set adsrc='' where 1=2;"
>         $ Right [Just $ ([], [] {-UpdateInfo "pg_attrdef" [("adsrc",ScalarType "text")]-})]
>       -- TODO: actually, pg doesn't support this so need to generate error instead
>      {-,s "update pg_attrdef set (adbin,adsrc) = ((select 'a','b'));"
>         $ Right [Just $ ([], [] {-UpdateInfo "pg_attrdef" [("adbin",ScalarType "text"),("adsrc",ScalarType "text")]-})]-}
>      --check where ids
>      ,s "update pg_attrdef set adsrc='' where adsrc='';"
>         $ Right [Just $ ([], [] {-UpdateInfo "pg_attrdef" [("adsrc",ScalarType "text")]-})]
>      ,s "update pg_attrdef set adnum = adnum + 1;"
>         $ Right [Just $ ([], [] {-UpdateInfo "pg_attrdef" [("adnum",ScalarType "int2")]-})]
>      ]

>   ,Group "delete" [
>       s "delete from nope;"
>         $ Left [UnrecognisedRelation "nope"]
>      ,s "delete from pg_attrdef where 1=2;"
>         $ Right [Just $ ([], [])]
>      ,s "delete from pg_attrdef where 1;"
>         $ Left [ExpressionMustBeBool]
>      ,s "delete from pg_attrdef where adsrc='';"
>         $ Right [Just $ ([], [])]
>      ]

--------------------------------------------------------------------------------

test the catalog updates from creates, etc.

>   ,Group "creates" [
>       d "create table t1 (\n\
>         \   a int,\n\
>         \   b text\n\
>         \);"
>         [CatCreateTable "t1" [("a",ScalarType "int4")
>                               ,("b",ScalarType "text")]
>                               [("tableoid", ScalarType "oid")
>                               ,("cmax", ScalarType "cid")
>                               ,("xmax", ScalarType "xid")
>                               ,("cmin", ScalarType "cid")
>                               ,("xmin", ScalarType "xid")
>                               ,("ctid", ScalarType "tid")]]
>      ,d "create type t1 as (\n\
>         \   a int,\n\
>         \   b text\n\
>         \);"
>         [CatCreateComposite "t1" [("a",ScalarType "int4")
>                                   ,("b",ScalarType "text")]]
>
>      ,d "create domain t1 as text;"
>         [CatCreateDomain (DomainType "t1") (ScalarType "text")]
>
>      ,d "create domain t1 as text check (value in ('a', 'b'));\n\
>         \select 'text'::t1;"
>         [CatCreateDomain (DomainType "t1") (ScalarType "text")]
>
>
>      ,d "create view v1 as select * from pg_attrdef;"
>         [CatCreateView "v1" [("adrelid",ScalarType "oid")
>                              ,("adnum",ScalarType "int2")
>                              ,("adbin",ScalarType "text")
>                              ,("adsrc",ScalarType "text")]]
>
>      ,d "create function t1(text) returns text as $$\n\
>         \null;\n\
>         \$$ language sql stable;"
>         [CatCreateFunction FunName "t1" [ScalarType "text"]
>                             (ScalarType "text") False]
>      ,d "create language plpgsql;"
>         [CatCreateFunction FunName "plpgsql_call_handler" [] (Pseudo LanguageHandler) False
>         ,CatCreateFunction FunName "plpgsql_validator" [ScalarType "oid"] (Pseudo Void) False]
>      ]
>

--------------------------------------------------------------------------------

~~~~
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
~~~~

>   ,Group "create function identifier resolution" [
>       s "create function t1(stuff text) returns text as $$\n\
>         \begin\n\
>         \  return stuff || ' and stuff';\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         (Right [Nothing])
>      ,s "create function t1(stuff text) returns text as $$\n\
>         \begin\n\
>         \  return badstuff || ' and stuff';\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         (Left [UnrecognisedIdentifier "badstuff"])
>      ,s "create function t1() returns text as $$\n\
>         \declare\n\
>         \  stuff text;\n\
>         \begin\n\
>         \  return stuff || ' and stuff';\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         (Right [Nothing])
>      ]

--------------------------------------------------------------------------------

>   ,Group "plpgsqlbits" [
>       s "create function t1(stuff text) returns text as $$\n\
>         \begin\n\
>         \  return stuff || ' and stuff';\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         (Right [Nothing])
>      ,s "create function t1(stuff text) returns text as $$\n\
>         \begin\n\
>         \  return badstuff || ' and stuff';\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         (Left [UnrecognisedIdentifier "badstuff"])
>      ,s "create function t1() returns text as $$\n\
>         \declare\n\
>         \  stuff text;\n\
>         \begin\n\
>         \  return stuff || ' and stuff';\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         (Right [Nothing])
>      ]

--------------------------------------------------------------------------------

>   ,Group "plpgsqlbits" [
>       s "create function t1() returns void as $$\n\
>         \declare\n\
>         \  a bool;\n\
>         \begin\n\
>         \  a := 3;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         (Left [IncompatibleTypes (ScalarType "bool") (ScalarType "int4")])
>      ,s "create function t1() returns void as $$\n\
>         \declare\n\
>         \  a boolean;\n\
>         \begin\n\
>         \  a := true;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         (Right [Nothing])
>      ]

--------------------------------------------------------------------------------

>   ,Group "for loops" [
>       s "create function t1() returns void as $$\n\
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
>
>
>      {-,s "create function t1() returns void as $$\n\
>         \declare\n\
>         \  r record;\n\
>         \  t int;\n\
>         \begin\n\
>         \  for r in select adnum from pg_attrdef loop\n\
>         \    t := r;\n\
>         \  end loop;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         (Right [Nothing])-}
>
>       -- loop var already declared
>
>      ,s "create function test() returns void as $$\n\
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
>
>        --implicitly created loop var
>
>      ,s "create function test() returns void as $$\n\
>         \declare\n\
>         \  i1 int;\n\
>         \begin\n\
>         \  for i in 0 .. 10 loop\n\
>         \    i1 := i;\n\
>         \  end loop;\n\
>         \end;\n\
>         \$$ language plpgsql volatile;"
>        (Right [Nothing])
>
>        --loop var already declared, wrong type
>
>      ,s "create function test() returns void as $$\n\
>         \declare\n\
>         \  i bool;\n\
>         \begin\n\
>         \  for i in 0 .. 10 loop\n\
>         \    null;\n\
>         \  end loop;\n\
>         \end;\n\
>         \$$ language plpgsql volatile;"
>        (Left [IncompatibleTypes (ScalarType "bool") (ScalarType "int4")])
>
>       --loop var implicit check it's type
>
>      ,s "create function test() returns void as $$\n\
>         \declare\n\
>         \  t bool;\n\
>         \begin\n\
>         \  for i in 0 .. 10 loop\n\
>         \    t := i;\n\
>         \  end loop;\n\
>         \end;\n\
>         \       $$ language plpgsql volatile;"
>         (Left [IncompatibleTypes (ScalarType "bool") (ScalarType "int4")])
>      ]

--------------------------------------------------------------------------------

>   ,Group "check catalog chaining" [
>
>     -- create function then select
>     -- select then create function
>     -- then in two separate chained asts
>
>       s "create function t1() returns void as $$\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \$$ language plpgsql stable;\n\
>         \select t1();"
>         (Right [Nothing,Just (([], []))])
>      ,s "select t1();\n\
>         \create function t1() returns void as $$\n\
>         \begin\n\
>         \  null;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         (Left [NoMatchingOperator "t1" []])
>      ]
>
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

--------------------------------------------------------------------------------

~~~~
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
~~~~

--------------------------------------------------------------------------------

~~~~
check insert returning, update returning, delete returning, one check each
check select into: multiple vars, record (then access fields to check),
  composite var
check errors: select into wrong number of vars, wrong types, and into
  composite wrong number and wrong type
~~~~

>   ,Group "select into" [
>       s "insert into pg_attrdef (adrelid,adnum,adbin,adsrc)\n\
>         \values (1,2, 'a', 'b') returning adnum,adbin;"
>         $ Right [Just $ ([], [("adnum", ScalarType "int2")
>                                          ,("adbin", ScalarType "text")])]
>      ,s "update pg_attrdef set adnum = adnum + 1 returning adnum;"
>         $ Right [Just $ ([], [("adnum", ScalarType "int2")])]
>      ,s "delete from pg_attrdef returning adnum,adbin;"
>         $ Right [Just $ ([], [("adnum", ScalarType "int2")
>                                          ,("adbin", ScalarType "text")])]
>      ,s "create function t1() returns void as $$\n\
>         \declare\n\
>         \  a int;\n\
>         \  b text;\n\
>         \begin\n\
>         \  select adnum,adbin into a,b from pg_attrdef;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         $ Right [Nothing]
>      ,s "create function t1() returns void as $$\n\
>         \declare\n\
>         \  a int;\n\
>         \  b text;\n\
>         \begin\n\
>         \  select adnum,adbin into b,a from pg_attrdef;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         $ Left [IncompatibleTypes (ScalarType "text") (ScalarType "int2")
>                ,IncompatibleTypes (ScalarType "int4") (ScalarType "text")]
>      ,s "create function t1() returns void as $$\n\
>         \declare\n\
>         \  a int;\n\
>         \  b text;\n\
>         \begin\n\
>         \  select adnum,adbin into a,c from pg_attrdef;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         $ Left [UnrecognisedIdentifier "c"]
>      ,s "create function t1() returns void as $$\n\
>         \declare\n\
>         \  a int;\n\
>         \  b text;\n\
>         \begin\n\
>         \  select adnum,adbin into a from pg_attrdef;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         $ Left [WrongNumberOfColumns]
>      ,s "create function t1() returns void as $$\n\
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
>      ,s "create function t1() returns void as $$\n\
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
>         $ Left [UnrecognisedIdentifier "adsrc"]
>      ,s "create function t1() returns void as $$\n\
>         \declare\n\
>         \  r pg_attrdef;\n\
>         \begin\n\
>         \  select * into r from pg_attrdef;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         $ Right [Nothing]
>      ,s "create function t1() returns void as $$\n\
>         \declare\n\
>         \  r pg_class;\n\
>         \begin\n\
>         \  select adnum,adbin into r from pg_attrdef;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         $ Left [IncompatibleTypes (NamedCompositeType "pg_class") (AnonymousRecordType [ScalarType "int2",ScalarType "text"])]
>      ,s "create function t1() returns void as $$\n\
>         \declare\n\
>         \  r record;\n\
>         \begin\n\
>         \  select adnum,adbin into r from pg_attrdef;\n\
>         \  select relname into r from pg_class;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         $ Right [Nothing]
>      ]

>   ,Group "composite elements" [
>       s "create function t1() returns void as $$\n\
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
>      ]

>   ,Group "positional args" [
>       s "create function distance(int, int, int, int) returns float(24) as $$\n\
>         \  select (point($1, $2) <-> point($3, $4))::float(24) as result;\n\
>         \$$ language sql immutable;"
>         $ Right [Nothing]
>      ,s "create function distance(int, int, int, int) returns float(24) as $$\n\
>         \  select (point($1, $2) <-> point($3, $5))::float(24) as result;\n\
>         \$$ language sql immutable;"
>         $ Left [UnrecognisedIdentifier "$5"]
>      ]

>   ,Group "window fns" [
>       s "select *, row_number() over () from pg_attrdef;"
>         $ Right [Just $ ([],
>                   [("adrelid",ScalarType "oid")
>                   ,("adnum",ScalarType "int2")
>                   ,("adbin",ScalarType "text")
>                   ,("adsrc",ScalarType "text")
>                   ,("row_number",ScalarType "int8")])]
>      ]

>   ,Group "drop stuff" [
>       d "create function test(a int) returns void as $$\n\
>         \begin\n\
>         \  null;\n\
>         \end\n\
>         \$$ language plpgsql;"
>         [CatCreateFunction FunName "test" [typeInt] (Pseudo Void) False]
>      ,d "create function test(a int) returns void as $$\n\
>         \begin\n\
>         \  null;\n\
>         \end\n\
>         \$$ language plpgsql;\n\
>         \drop function test(int);"
>         []
>      ,d "drop function test(int);" -- this should fail but doesn't
>         []
>      ,d "drop function if exists test(int);"
>         []
>      ]

>   ,Group "triggers" [
>       {-d [$here|
>          create table t1 (
>            a int,
>            b int,
>            c text
>          );
>          create table t2 (
>            a int,
>            b int,
>            c int
>          );
>          create function trig_fn() returns trigger as $a$
>          begin
>            if (NEW.a > 100) or (OLD.b < 5) then
>                raise exception 'no good';
>            end if;
>            return NEW;
>          end;
>          $a$ language plpgsql volatile;
>          create trigger trig
>            after update on t1
>            for each row
>            execute procedure check_relvar_update();
>          create trigger trig
>            after update on t2
>            for each row
>            execute procedure check_relvar_update();
>          |]
>       [CatCreateTable "t1" [("a",ScalarType "int4")
>                            ,("b",ScalarType "int4")
>                            ,("c",ScalarType "text")]
>                            [("tableoid",ScalarType "oid")
>                            ,("cmax",ScalarType "cid")
>                            ,("xmax",ScalarType "xid")
>                            ,("cmin",ScalarType "cid")
>                            ,("xmin",ScalarType "xid")
>                            ,("ctid",ScalarType "tid")]
>       ,CatCreateTable "t2" [("a",ScalarType "int4")
>                            ,("b",ScalarType "int4")
>                            ,("c",ScalarType "int4")]
>                            [("tableoid",ScalarType "oid")
>                            ,("cmax",ScalarType "cid")
>                            ,("xmax",ScalarType "xid")
>                            ,("cmin",ScalarType "cid")
>                            ,("xmin",ScalarType "xid")
>                            ,("ctid",ScalarType "tid")]
>       ,CatCreateFunction FunName "trig_fn" [] (Pseudo Trigger) False]-}
>      {-,s [$here|
>          create table t1 (
>            a int,
>            b int,
>            c text
>          );
>          create function trig_fn() returns trigger as $a$
>          begin
>            if NEW.d = 'bad value' then
>              return NEW;
>                raise exception 'no good';
>            end if;
>          end;
>          $a$ language plpgsql volatile;
>          create trigger trig
>            after update on t1
>            for each row
>            execute procedure check_relvar_update();
>          |]
>         $ Left []-}
>      ]
>   ]
>  where
>    e = Expr
>    s = StmtType
>    c = CatStmtType
>    d = Ddl

--------------------------------------------------------------------------------

> testExpressionType :: String -> Either [TypeError] Type -> Test.Framework.Test
> testExpressionType src et = testCase ("typecheck " ++ src) $
>   let ast = case parseScalarExpr "" src of
>                                      Left e -> error $ show e
>                                      Right l -> l
>       aast = typeCheckScalarExpr defaultTemplate1Catalog ast
>       ty = atype $ getAnnotation aast
>       er :: [TypeError]
>       er = [x | x <- universeBi aast]
>   in if null er
>      then assertEqual ("typecheck " ++ src) (Just et) $ fmap Right ty
>      else assertEqual ("typecheck " ++ src) et $ Left er
>
> testStatementType :: String -> Either [TypeError] [Maybe StatementType] -> Test.Framework.Test
> testStatementType src sis = testCase ("typecheck " ++ src) $
>   let ast = case parseStatements "" src of
>                               Left e -> error $ show e
>                               Right l -> l
>       aast = snd $ typeCheckStatements defaultTemplate1Catalog ast
>       is = map (stType . getAnnotation) aast
>       er :: [TypeError]
>       er = [x | x <- universeBi aast]
>   in case (length er, length is) of
>        (0,0) -> assertFailure "didn't get any infos?"
>        (0,_) -> assertTrace (groom aast) ("typecheck " ++ src) sis $ Right is
>        _ -> assertTrace (groom aast) ("typecheck " ++ src) sis $ Left er

> testCatUpStatementType :: String
>                        -> [CatalogUpdate]
>                        -> Either [TypeError] [Maybe StatementType]
>                        -> Test.Framework.Test
> testCatUpStatementType src eu sis = testCase ("typecheck " ++ src) $
>   let ast = case parseStatements "" src of
>                               Left e -> error $ show e
>                               Right l -> l
>       aast = snd $ typeCheckStatements makeCat ast
>       is = map (stType . getAnnotation) aast
>       er :: [TypeError]
>       er = [x | x <- universeBi aast]
>   in {-trace (show aast) $-} case (length er, length is) of
>        (0,0) -> assertFailure "didn't get any infos?"
>        (0,_) -> assertEqual ("typecheck " ++ src) sis $ Right is
>        _ -> assertEqual ("typecheck " ++ src) sis $ Left er
>   where
>     makeCat = case updateCatalog defaultTemplate1Catalog eu of
>                         Left x -> error $ show x
>                         Right e -> e
>
> testCat :: String -> [CatalogUpdate] -> Test.Framework.Test
> testCat src eu = testCase ("check catalog: " ++ src) $
>   let ast = case parseStatements "" src of
>                               Left e -> error $ show e
>                               Right l -> l
>       (ncat,aast) = typeCheckStatements defaultTemplate1Catalog ast
>       er :: [TypeError]
>       er = [x | x <- universeBi aast]
>       neu = deconstructCatalog ncat \\ deconstructCatalog defaultTemplate1Catalog
>   in if not (null er)
>        then assertFailure $ show er
>        else assertEqual "check eus" eu neu
>
> itemToTft :: Item -> Test.Framework.Test
> itemToTft (Expr s r) = testExpressionType s r
> itemToTft (StmtType s r) = testStatementType s r
> itemToTft (CatStmtType s c r) = testCatUpStatementType s c r
> itemToTft (Ddl s c) = testCat s c
> itemToTft (Group s is) = testGroup s $ map itemToTft is
