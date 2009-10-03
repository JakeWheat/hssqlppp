sCopyright 2009 Jake Wheat

Set of tests to check the type checking code

> module Database.HsSqlPpp.Tests.AstCheckTests (astCheckTests) where

> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> import Data.Char
> --import Debug.Trace

> import Database.HsSqlPpp.Parsing.Parser
> import Database.HsSqlPpp.Ast.Annotator
> import Database.HsSqlPpp.Ast.Annotation
> import Database.HsSqlPpp.Ast.Environment
> import Database.HsSqlPpp.Ast.SqlTypes

> astCheckTests :: Test.Framework.Test
> astCheckTests = testGroup "astCheckTests" [
>     --testGroup "test test"
>     --(mapAttr [
>     --  p "select 1;" []
>     -- ])

this is disabled because the messages are getting a rethink, the code
that supports this test passing is commented out also

> {-
>    ,testGroup "loop tests"
>     (mapAttr [] {-
>       p "create function cont_test1() returns void as $$\n\
>         \declare\n\
>         \  r record;\n\
>         \begin\n\
>         \  for r in select a from b loop\n\
>         \    null;\n\
>         \    continue;\n\
>         \  end loop;\n\
>         \end;\n\
>         \$$ language plpgsql volatile;\n" []
>      ,p "create function cont_test2() returns void as $$\n\
>         \begin\n\
>         \    continue;\n\
>         \end;\n\
>         \$$ language plpgsql volatile;\n" [Error ("",3,5) ContinueNotInLoop]
>      ]-}
>      )-}

>    testGroup "basic literal types"
>     (mapExprType [
>       p "1" $ Right typeInt
>      ,p "1.0" $ Right typeNumeric
>      ,p "'test'" $ Right UnknownStringLit
>      ,p "true" $ Right typeBool
>      ,p "array[1,2,3]" $ Right (ArrayType typeInt)
>      ,p "array['a','b']" $ Right (ArrayType (ScalarType "text"))
>      ,p "array[1,'b']" $ Right (ArrayType typeInt)
>      ,p "array[1,true]" $ Left [IncompatibleTypeSet [typeInt,typeBool]]
>      ])
>
>    ,testGroup "some expressions"
>     (mapExprType [
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
>                      [UnknownStringLit
>                      ,ScalarType "int4"
>                      ,ScalarType "bool"]]

>      ,p "3 between 2 and 4" $ Right typeBool
>      ,p "3 between true and 4" $ Left
>                                [NoMatchingOperator ">="
>                                 [typeInt
>                                 ,typeBool]]

>      ,p "array[1,2,3][2]" $ Right typeInt
>      ,p "array['a','b'][1]" $ Right (ScalarType "text")

 >      ,p "array['a','b'][true]" (TypeError ("",0,0)
 >                                   (WrongType
 >                                    typeInt
 >                                    UnknownStringLit))

>      ,p "not true" $ Right typeBool
>      ,p "not 1" $ Left
>                  [NoMatchingOperator "!not" [typeInt]]

>      ,p "@ 3" $ Right typeInt
>      ,p "@ true" $ Left
>                  [NoMatchingOperator "@" [ScalarType "bool"]]

>      ,p "-3" $ Right typeInt
>      ,p "-'a'" $ Left
>                  [NoMatchingOperator "-" [UnknownStringLit]]

>      ,p "4-3" $ Right typeInt

>      --,p "1 is null" typeBool
>      --,p "1 is not null" typeBool

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
>      ])

>    ,testGroup "special functions"
>     (mapExprType [
>       p "coalesce(null,1,2,null)" $ Right typeInt
>      ,p "coalesce('3',1,2,null)" $ Right typeInt
>      ,p "coalesce('3',1,true,null)"
>             $ Left [IncompatibleTypeSet [UnknownStringLit
>                                 ,ScalarType "int4"
>                                 ,ScalarType "bool"
>                                 ,UnknownStringLit]]
>      ,p "nullif('hello','hello')" $ Right (ScalarType "text")
>      ,p "nullif(3,4)" $ Right typeInt
>      ,p "nullif(true,3)"
>             $ Left [NoMatchingOperator "nullif" [ScalarType "bool"
>                                           ,ScalarType "int4"]]
>      ,p "greatest(3,5,6,4,3)" $ Right typeInt
>      ,p "least(3,5,6,4,3)" $ Right typeInt
>      ,p "least(5,true)"
>             $ Left [IncompatibleTypeSet [ScalarType "int4"
>                                               ,ScalarType "bool"]]
>      ])

implicit casting and function/operator choice tests:
check when multiple implicit and one exact match on num args
check multiple matches with num args, only one with implicit conversions
check multiple implicit matches with one preferred
check multiple implicit matches with one preferred highest count
check casts from unknown string lits

>    ,testGroup "some expressions"
>     (mapExprType [
>       p "3 + '4'" $ Right typeInt
>      ,p "3.0 + '4'" $ Right typeNumeric
>      ,p "'3' + '4'" $ Left [NoMatchingOperator "+" [UnknownStringLit
>                                               ,UnknownStringLit]]
>      ])
>

>    ,testGroup "random expressions"
>     (mapExprType [
>       p "exists (select 1 from pg_type)" $ Right typeBool
>      ,p "exists (select testit from pg_type)"
>        $ Left [UnrecognisedIdentifier "testit"]
>     ])

rows different lengths
rows match types pairwise, same and different types
rows implicit cast from unknown
rows don't match types


>    ,testGroup "row comparison expressions"
>     (mapExprType [
>       p "row(1)" $ Right (RowCtor [typeInt])
>      ,p "row(1,2)" $ Right (RowCtor [typeInt,typeInt])
>      ,p "row('t1','t2')" $ Right (RowCtor [UnknownStringLit,UnknownStringLit])
>      ,p "row(true,1,'t3')" $ Right (RowCtor [typeBool, typeInt,UnknownStringLit])
>      ,p "(true,1,'t3',75.3)" $ Right (RowCtor [typeBool,typeInt
>                                       ,UnknownStringLit,typeNumeric])
>      ,p "row(1) = row(2)" $ Right typeBool
>      ,p "row(1,2) = row(2,1)" $ Right typeBool
>      ,p "(1,2) = (2,1)" $ Right typeBool
>      ,p "(1,2,3) = (2,1)" $ Left [ValuesListsMustBeSameLength]
>      ,p "('1',2) = (2,'1')" $ Right typeBool
>      ,p "(1,true) = (2,1)" $ Left [IncompatibleTypeSet [ScalarType "bool",ScalarType "int4"]]
>      ,p "(1,2) <> (2,1)" $ Right typeBool
>      ,p "(1,2) >= (2,1)" $ Right typeBool
>      ,p "(1,2) <= (2,1)" $ Right typeBool
>      ,p "(1,2) > (2,1)" $ Right typeBool
>      ,p "(1,2) < (2,1)" $ Right typeBool
>     ])



>    ,testGroup "case expressions"
>     (mapExprType [
>       p "case\n\
>         \ when true then 1\n\
>         \end" $ Right typeInt
>      ,p "case\n\
>         \ when 1=2 then 'stuff'\n\
>         \ when 2=3 then 'blah'\n\
>         \ else 'test'\n\
>         \end" $ Right (ScalarType "text")
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

>      ])

>    ,testGroup "polymorphic functions"
>     (mapExprType [
>       p "array_append(ARRAY[1,2], 3)"
>         $ Right (ArrayType typeInt)
>      ,p "array_append(ARRAY['a','b'], 'c')"
>         $ Right (ArrayType $ ScalarType "text")
>      ,p "array_append(ARRAY['a'::int,'b'], 'c')"
>         $ Right (ArrayType typeInt)
>      ])

todo:



>    ,testGroup "cast expressions"
>     (mapExprType [
>       p "cast ('1' as integer)"
>         $ Right typeInt
>      ,p "cast ('1' as baz)"
>         $ Left [UnknownTypeName "baz"]
>      ,p "array[]"
>         $ Left [TypelessEmptyArray]
>      --todo: figure out how to do this
>      --,p "array[] :: text[]"
>      --   (ArrayType (ScalarType "text"))

>      ])

>    ,testGroup "simple selects"
>     (mapStatementInfo [
>       p "select 1;" $ Right [Just $ SelectInfo $ SetOfType $
>                              UnnamedCompositeType [("?column?", typeInt)]]
>      ,p "select 1 as a;" $
>         Right [Just $ SelectInfo $ SetOfType $ UnnamedCompositeType [("a", typeInt)]]
>      ,p "select 1,2;" $
>         Right [Just $ SelectInfo $ SetOfType $ UnnamedCompositeType [("?column?", typeInt)
>                                                 ,("?column?", typeInt)]]
>      ,p "select 1 as a, 2 as b;" $
>         Right [Just $ SelectInfo $ SetOfType $ UnnamedCompositeType [("a", typeInt)
>                                                 ,("b", typeInt)]]
>      ,p "select 1+2 as a, 'a' || 'b';" $
>         Right [Just $ SelectInfo $ SetOfType $
>                UnnamedCompositeType [("a", typeInt)
>                                     ,("?column?", ScalarType "text")]]
>      ,p "values (1,2);" $ Right [Just $ SelectInfo $ SetOfType $
>                          UnnamedCompositeType
>                          [("column1", typeInt)
>                          ,("column2", typeInt)]]
>      ,p "values (1,2),('3', '4');" $ Right [Just $ SelectInfo $ SetOfType $
>                                      UnnamedCompositeType
>                                      [("column1", typeInt)
>                                      ,("column2", typeInt)]]
>      ,p "values (1,2),('a', true);" $ Left [IncompatibleTypeSet [typeInt
>                                                                 ,typeBool]]
>      ,p "values ('3', '4'),(1,2);" $ Right [Just $ SelectInfo $ SetOfType $
>                                      UnnamedCompositeType
>                                      [("column1", typeInt)
>                                      ,("column2", typeInt)]]
>      ,p "values ('a', true),(1,2);" $ Left [IncompatibleTypeSet [typeBool
>                                                                 ,typeInt]]
>      ,p "values ('a'::text, '2'::int2),('1','2');" $ Right [Just $ SelectInfo $ SetOfType $
>                                      UnnamedCompositeType
>                                      [("column1", ScalarType "text")
>                                      ,("column2", typeSmallInt)]]
>      ,p "values (1,2,3),(1,2);" $ Left [ValuesListsMustBeSameLength]
>      ])

>    ,testGroup "simple combine selects"
>     (mapStatementInfo [
>      p "select 1,2  union select '3', '4';" $ Right [Just $ SelectInfo $ SetOfType $
>                                      UnnamedCompositeType
>                                      [("?column?", typeInt)
>                                      ,("?column?", typeInt)]]
>      ,p "select 1,2 intersect select 'a', true;" $ Left [IncompatibleTypeSet [typeInt
>                                                         ,typeBool]]
>      ,p "select '3', '4' except select 1,2;" $ Right [Just $ SelectInfo $ SetOfType $
>                                      UnnamedCompositeType
>                                      [("?column?", typeInt)
>                                      ,("?column?", typeInt)]]
>      ,p "select 'a', true union select 1,2;"
>                                      $ Left [IncompatibleTypeSet [typeBool
>                                                         ,typeInt]]
>      ,p "select 'a'::text, '2'::int2 intersect select '1','2';" $ Right [Just $ SelectInfo $ SetOfType $
>                                      UnnamedCompositeType
>                                      [("text", ScalarType "text")
>                                      ,("int2", typeSmallInt)]]
>      ,p "select 1,2,3 except select 1,2;" $ Left [ValuesListsMustBeSameLength]
>      ,p "select '3' as a, '4' as b except select 1,2;" $ Right [Just $ SelectInfo $ SetOfType $
>                                      UnnamedCompositeType
>                                      [("a", typeInt)
>                                      ,("b", typeInt)]]
>      ])


>    ,testGroup "simple selects from"
>     (mapStatementInfo [
>       p "select a from (select 1 as a, 2 as b) x;"
>         $ Right [Just $ SelectInfo $ SetOfType $ UnnamedCompositeType [("a", typeInt)]]
>      ,p "select b from (select 1 as a, 2 as b) x;"
>         $ Right [Just $ SelectInfo $ SetOfType $ UnnamedCompositeType [("b", typeInt)]]
>      ,p "select c from (select 1 as a, 2 as b) x;"
>         $ Left [UnrecognisedIdentifier "c"]
>      ,p "select typlen from pg_type;"
>         $ Right [Just $ SelectInfo $ SetOfType $ UnnamedCompositeType [("typlen", typeSmallInt)]]
>      ,p "select oid from pg_type;"
>         $ Right [Just $ SelectInfo $ SetOfType $ UnnamedCompositeType [("oid", ScalarType "oid")]]
>      ,p "select p.oid from pg_type p;"
>         $ Right [Just $ SelectInfo $ SetOfType $ UnnamedCompositeType [("oid", ScalarType "oid")]]
>      ,p "select typlen from nope;"
>         $ Left [UnrecognisedIdentifier "typlen",UnrecognisedRelation "nope"]
>      ,p "select generate_series from generate_series(1,7);"
>         $ Right [Just $ SelectInfo $ SetOfType $ UnnamedCompositeType [("generate_series", typeInt)]]

check aliasing

>      ,p "select generate_series.generate_series from generate_series(1,7);"
>         $ Right [Just $ SelectInfo $ SetOfType $ UnnamedCompositeType [("generate_series", typeInt)]]
>      ,p "select g from generate_series(1,7) g;"
>         $ Right [Just $ SelectInfo $ SetOfType $ UnnamedCompositeType [("g", typeInt)]]
>      ,p "select g.g from generate_series(1,7) g;"
>         $ Right [Just $ SelectInfo $ SetOfType $ UnnamedCompositeType [("g", typeInt)]]
>      ,p "select generate_series.g from generate_series(1,7) g;"
>         $ Left [UnrecognisedCorrelationName "generate_series"]
>      ,p "select g.generate_series from generate_series(1,7) g;"
>         $ Left [UnrecognisedIdentifier "g.generate_series"]


>      ,p "select * from pg_attrdef;"
>         $ Right [Just $ SelectInfo $ SetOfType $ UnnamedCompositeType
>          [("adrelid",ScalarType "oid")
>          ,("adnum",ScalarType "int2")
>          ,("adbin",ScalarType "text")
>          ,("adsrc",ScalarType "text")]]
>      ,p "select abs from abs(3);"
>         $ Right [Just $ SelectInfo $ SetOfType $ UnnamedCompositeType [("abs", typeInt)]]
>         --todo: these are both valid,
>         --the second one means select 3+generate_series from generate_series(1,7)
>         --  select generate_series(1,7);
>         -- select 3 + generate_series(1,7);
>      ])


>    ,testGroup "simple selects from 2"
>     (mapStatementInfoEnv [
>       t "select a,b from testfunc();"
>         (makeEnv
>              [EnvCreateComposite "testType" [("a", ScalarType "text")
>                                             ,("b", typeInt)
>                                             ,("c", typeInt)]
>              ,EnvCreateFunction FunName "testfunc"  [] (SetOfType $ CompositeType "testType")])
>         $ Right [Just $ SelectInfo $ SetOfType $ UnnamedCompositeType
>                  [("a",ScalarType "text"),("b",ScalarType "int4")]]

>      ,t "select testfunc();"
>         (makeEnv
>              [EnvCreateFunction FunName "testfunc"  [] $ Pseudo Void])
>         $ Right [Just $ SelectInfo $ Pseudo Void]

>      ])

>    ,testGroup "simple join selects"
>     (mapStatementInfo [
>       p "select * from (select 1 as a, 2 as b) a\n\
>         \  cross join (select true as c, 4.5 as d) b;"
>         $ Right [Just $ SelectInfo $ SetOfType $ UnnamedCompositeType [("a", typeInt)
>                                           ,("b", typeInt)
>                                           ,("c", typeBool)
>                                           ,("d", typeNumeric)]]
>      ,p "select * from (select 1 as a, 2 as b) a\n\
>         \  inner join (select true as c, 4.5 as d) b on true;"
>         $ Right [Just $ SelectInfo $ SetOfType $ UnnamedCompositeType [("a", typeInt)
>                                           ,("b", typeInt)
>                                           ,("c", typeBool)
>                                           ,("d", typeNumeric)]]
>      ,p "select * from (select 1 as a, 2 as b) a\n\
>         \  inner join (select 1 as a, 4.5 as d) b using(a);"
>         $ Right [Just $ SelectInfo $ SetOfType $ UnnamedCompositeType [("a", typeInt)
>                                           ,("b", typeInt)
>                                           ,("d", typeNumeric)]]
>      ,p "select * from (select 1 as a, 2 as b) a\n\
>         \ natural inner join (select 1 as a, 4.5 as d) b;"
>         $ Right [Just $ SelectInfo $ SetOfType $ UnnamedCompositeType [("a", typeInt)
>                                           ,("b", typeInt)
>                                           ,("d", typeNumeric)]]
>         --check the attribute order
>      ,p "select * from (select 2 as b, 1 as a) a\n\
>         \ natural inner join (select 4.5 as d, 1 as a) b;"
>         $ Right [Just $ SelectInfo $ SetOfType $ UnnamedCompositeType [("a", typeInt)
>                                           ,("b", typeInt)
>                                           ,("d", typeNumeric)]]
>      ,p "select * from (select 1 as a, 2 as b) a\n\
>         \ natural inner join (select true as a, 4.5 as d) b;"
>         $ Left [IncompatibleTypeSet [ScalarType "int4"
>                                      ,ScalarType "bool"]]
>      ])

>    ,testGroup "simple scalar identifier qualification"
>     (mapStatementInfo [
>       p "select a.* from \n\
>         \(select 1 as a, 2 as b) a \n\
>         \cross join (select 3 as c, 4 as d) b;"
>         $ Right [Just $ SelectInfo $ SetOfType $ UnnamedCompositeType [("a", typeInt)
>                                           ,("b", typeInt)]]
>      ,p "select nothere.* from \n\
>         \(select 1 as a, 2 as b) a \n\
>         \cross join (select 3 as c, 4 as d) b;"
>         $ Left [UnrecognisedCorrelationName "nothere"]
>      ,p "select a.b,b.c from \n\
>         \(select 1 as a, 2 as b) a \n\
>         \natural inner join (select 3 as a, 4 as c) b;"
>         $ Right [Just $ SelectInfo $ SetOfType $ UnnamedCompositeType [("b", typeInt)
>                                           ,("c", typeInt)]]
>      ,p "select a.a,b.a from \n\
>         \(select 1 as a, 2 as b) a \n\
>         \natural inner join (select 3 as a, 4 as c) b;"
>         $ Right [Just $ SelectInfo $ SetOfType $ UnnamedCompositeType [("a", typeInt)
>                                           ,("a", typeInt)]]

>      ,p "select pg_attrdef.adsrc from pg_attrdef;"
>         $ Right [Just $ SelectInfo $ SetOfType $ UnnamedCompositeType [("adsrc", ScalarType "text")]]

>      ,p "select a.adsrc from pg_attrdef a;"
>         $ Right [Just $ SelectInfo $ SetOfType $ UnnamedCompositeType [("adsrc", ScalarType "text")]]

>      ,p "select pg_attrdef.adsrc from pg_attrdef a;"
>         $ Left [UnrecognisedCorrelationName "pg_attrdef"]

>      ,p "select a from (select 2 as b, 1 as a) a\n\
>         \natural inner join (select 4.5 as d, 1 as a) b;"
>         $ Right [Just $ SelectInfo $ SetOfType $ UnnamedCompositeType [("a", typeInt)]]

select g.fn from fn() g


>      ])

>    ,testGroup "aggregates"
>     (mapStatementInfo [
>        p "select max(prorettype::int) from pg_proc;"
>         $ Right [Just $ SelectInfo $ SetOfType $ UnnamedCompositeType [("max", typeInt)]]
>      ])

>    ,testGroup "simple wheres"
>     (mapStatementInfo [
>       p "select 1 from pg_type where true;"
>         $ Right [Just $ SelectInfo $ SetOfType $ UnnamedCompositeType [("?column?", typeInt)]]
>      ,p "select 1 from pg_type where 1;"
>         $ Left [ExpressionMustBeBool]
>      ,p "select typname from pg_type where typbyval;"
>         $ Right [Just $ SelectInfo $ SetOfType $ UnnamedCompositeType [("typname", ScalarType "name")]]
>      ,p "select typname from pg_type where typtype = 'b';"
>         $ Right [Just $ SelectInfo $ SetOfType $ UnnamedCompositeType [("typname", ScalarType "name")]]
>      ,p "select typname from pg_type where what = 'b';"
>         $ Left [UnrecognisedIdentifier "what"]
>      ])

TODO: check identifier stacking working, then remove the pg_namespace
qualifier before oid and this should still work

>    ,testGroup "subqueries"
>     (mapStatementInfo [
>       p "select relname as relvar_name\n\
>         \    from pg_class\n\
>         \    where ((relnamespace =\n\
>         \           (select oid\n\
>         \              from pg_namespace\n\
>         \              where (nspname = 'public'))) and (relkind = 'r'));"
>         $ Right [Just $ SelectInfo $ SetOfType $ UnnamedCompositeType [("relvar_name",ScalarType "name")]]
>      ,p "select relname from pg_class where relkind in ('r', 'v');"
>         $ Right [Just $ SelectInfo $ SetOfType $ UnnamedCompositeType [("relname",ScalarType "name")]]
>      ,p "select * from generate_series(1,7) g\n\
>         \where g not in (select * from generate_series(3,5));"
>         $ Right [Just $ SelectInfo $ SetOfType (UnnamedCompositeType [("g",typeInt)])]
>      ,p "select 3 = any(array[1,2,3]);"
>         $ Right [Just $ SelectInfo $ SetOfType (UnnamedCompositeType [("?column?",typeBool)])]
>      ])

identifiers in select parts

>{-    ,testGroup "select part identifiers"
>     (mapStatementInfo [
>       p "select relname,attname from pg_class\n\
>         \inner join pg_attribute\n\
>         \on pg_attribute.attrelid = pg_class.oid;"
>         $ Right [Just $ SelectInfo $ SetOfType $ UnnamedCompositeType [("relvar_name",ScalarType "name")]]
>      ])-}



insert

>    ,testGroup "insert"
>     (mapStatementInfo [
>       p "insert into nope (a,b) values (c,d);"
>         $ Left [UnrecognisedRelation "nope",UnrecognisedIdentifier "c",UnrecognisedIdentifier "d"]
>      ,p "insert into pg_attrdef (adrelid,adnum,adbin,adsrc)\n\
>         \values (1,2, 'a', 'b');"
>         $ Right [Just $ InsertInfo "pg_attrdef"
>                           [("adrelid",ScalarType "oid")
>                           ,("adnum",ScalarType "int2")
>                           ,("adbin",ScalarType "text")
>                           ,("adsrc",ScalarType "text")]]
>      ,p "insert into pg_attrdef\n\
>         \values (1,2, 'a', 'b');"
>         $ Right [Just $ InsertInfo "pg_attrdef"
>                           [("adrelid",ScalarType "oid")
>                           ,("adnum",ScalarType "int2")
>                           ,("adbin",ScalarType "text")
>                           ,("adsrc",ScalarType "text")]]
>      ,p "insert into pg_attrdef (hello,adnum,adbin,adsrc)\n\
>         \values (1,2, 'a', 'b');"
>         $ Left [UnrecognisedIdentifier "hello"]
>      ,p "insert into pg_attrdef (adrelid,adnum,adbin,adsrc)\n\
>         \values (1,true, 'a', 'b');"
>         $ Left [IncompatibleTypes (ScalarType "int2") (ScalarType "bool")]
>      ,p "insert into pg_attrdef (adrelid,adnum,adbin,adsrc)\n\
>         \values (1,true, 'a', 'b','c');"
>         $ Left [WrongNumberOfColumns]

>      ])

>    ,testGroup "update"
>     (mapStatementInfo [
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
>         $ Right [Just $ UpdateInfo "pg_attrdef" [("adsrc",ScalarType "text")]]
>      ,p "update pg_attrdef set adsrc='' where 1=2;"
>         $ Right [Just $ UpdateInfo "pg_attrdef" [("adsrc",ScalarType "text")]]
>       -- TODO: actually, pg doesn't support this so need to generate error instead
>      ,p "update pg_attrdef set (adbin,adsrc) = ((select 'a','b'));"
>         $ Right [Just $ UpdateInfo "pg_attrdef" [("adbin",ScalarType "text"),("adsrc",ScalarType "text")]]
>      --check where ids
>      ,p "update pg_attrdef set adsrc='' where adsrc='';"
>         $ Right [Just $ UpdateInfo "pg_attrdef" [("adsrc",ScalarType "text")]]
>      ,p "update pg_attrdef set adnum = adnum + 1;"
>         $ Right [Just (UpdateInfo "pg_attrdef" [("adnum",ScalarType "int2")])]


>      ])

>    ,testGroup "delete"
>     (mapStatementInfo [
>       p "delete from nope;"
>         $ Left [UnrecognisedRelation "nope"]
>      ,p "delete from pg_attrdef where 1=2;"
>         $ Right [Just $ DeleteInfo "pg_attrdef"]
>      ,p "delete from pg_attrdef where 1;"
>         $ Left [ExpressionMustBeBool]
>      ,p "delete from pg_attrdef where adsrc='';"
>         $ Right [Just $ DeleteInfo "pg_attrdef"]
>      ])


================================================================================

>    ,testGroup "creates"
>     (mapStatementInfoEu [
>       t "create table t1 (\n\
>         \   a int,\n\
>         \   b text\n\
>         \);"
>         (Right [Nothing])
>         [[EnvCreateTable "t1" [("a",ScalarType "int4")
>                               ,("b",ScalarType "text")]
>           []]]
>      ,t "create type t1 as (\n\
>         \   a int,\n\
>         \   b text\n\
>         \);"
>         (Right [Nothing])
>         [[EnvCreateComposite "t1" [("a",ScalarType "int4")
>                                   ,("b",ScalarType "text")]]]

>      ,t "create domain t1 as text;"
>         (Right [Nothing])
>         [[EnvCreateDomain (DomainType "t1") (ScalarType "text")]]

>      ,t "create domain t1 as text check (value in ('a', 'b'));\n\
>         \select 'text'::t1;"
>         (Right [Nothing])
>         [[EnvCreateDomain (DomainType "t1") (ScalarType "text")]]


>      ,t "create view v1 as select * from pg_attrdef;"
>         (Right [Nothing])
>         [[EnvCreateView "v1" [("adrelid",ScalarType "oid")
>                              ,("adnum",ScalarType "int2")
>                              ,("adbin",ScalarType "text")
>                              ,("adsrc",ScalarType "text")]]]

>      ,t "create function t1(text) returns text as $$\n\
>         \null;\n\
>         \$$ language sql stable;"
>         (Right [Nothing])
>         [[EnvCreateFunction FunName "t1" [ScalarType "text"]
>                             (ScalarType "text")]]

>      ])

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

>    ,testGroup "create function identifier resolution"
>     (mapStatementInfo [
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
>      ])

================================================================================

>    ,testGroup "plpgsqlbits"
>     (mapStatementInfo [
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
>      ])

================================================================================

>    ,testGroup "plpgsqlbits"
>     (mapStatementInfo [
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
>      ])

================================================================================

>    ,testGroup "for loops"
>     (mapStatementInfo [
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
>         \  r int;\n\
>         \  t int;\n\
>         \begin\n\
>         \  for r in select adnum from pg_attrdef loop\n\
>         \    t := r;\n\
>         \  end loop;\n\
>         \end;\n\
>         \$$ language plpgsql stable;"
>         (Right [Nothing])
>      ])

================================================================================

create function then select
select then create function
then in two separate chained asts

>    ,testGroup "check catalog chaining"
>     (mapStatementInfo [
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
>      ])

>    ,testGroup "check catalog chaining2"
>     (mapStatementInfos [
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
>      ])

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

>    ,testGroup "check catalog chaining2"
>     (mapStatementInfos [
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
>      ])


todo for chaos sql
for loop var = scalar, select = setof composite with one scalar

select into
composite assignment and equality
autocast from rowctor to composite when calling function
assignment to composite fields
read fields of composite

array_contains -> match anyelement
createtable as env update
window functions
assign domain <-> base
sql function not working

>
>    ]
>         where
>           --mapAttr = map $ uncurry checkAttrs
>           p a b = (a,b)
>           t a b c = (a,b,c)
>           mapExprType = map (uncurry $ checkExpressionType defaultTemplate1Environment)
>           --mapStatementType = map $ uncurry checkStatementType
>           mapStatementInfo = map $ uncurry checkStatementInfo
>           mapStatementInfos = map $ uncurry checkStatementInfos
>           mapStatementInfoEu = map (\(a,b,c) ->  checkStatementInfoEu a b c)
>           {-mapExprEnvType = map (\(a,b,c) -> checkExpressionType b a c)-}
>           makeEnv eu = case updateEnvironment defaultTemplate1Environment eu of
>                         Left x -> error $ show x
>                         Right e -> e
>           mapStatementInfoEnv = map (\(a,b,c) -> checkStatementInfoEnv b a c)

> checkExpressionType :: Environment -> String -> Either [TypeError] Type -> Test.Framework.Test
> checkExpressionType env src typ = testCase ("typecheck " ++ src) $
>   let ast = case parseExpression src of
>                                      Left e -> error $ show e
>                                      Right l -> l
>       aast = annotateExpression env ast
>       ty = getTopLevelTypes [aast]
>       er = concatMap snd $ getTypeErrors aast
>   in case (length er, length ty) of
>        (0,0) -> assertFailure "didn't get any types?"
>        (0,1) -> assertEqual ("typecheck " ++ src) typ $ Right $ head ty
>        (0,_) -> assertFailure "got too many types"
>        _ -> assertEqual ("typecheck " ++ src) typ $ Left er

> checkStatementInfo :: String -> Either [TypeError] [Maybe StatementInfo] -> Test.Framework.Test
> checkStatementInfo src sis = testCase ("typecheck " ++ src) $
>   let ast = case parseSql src of
>                               Left e -> error $ show e
>                               Right l -> l
>       aast = annotateAst ast
>       is = getTopLevelInfos aast
>       er = concatMap snd $ getTypeErrors aast
>   in {-trace (show $ map getAnnotation aast) $-} case (length er, length is) of
>        (0,0) -> assertFailure "didn't get any infos?"
>        (0,_) -> assertEqual ("typecheck " ++ src) sis $ Right is
>        _ -> assertEqual ("typecheck " ++ src) sis $ Left er


> checkStatementInfos :: [String] -> Either [TypeError] [Maybe StatementInfo] -> Test.Framework.Test
> checkStatementInfos srcs sis = testCase ("typecheck " ++ show srcs) $
>   let asts = map (\src -> case parseSql src of
>                                             Left e -> error $ show e
>                                             Right l -> l) srcs
>       aasts = annotateAstsEnv defaultTemplate1Environment asts
>       is = getTopLevelInfos $ last aasts
>       er = concatMap snd $ concatMap getTypeErrors aasts
>   in {-trace (show $ map getAnnotation aast) $-} case (length er, length is) of
>        (0,0) -> assertFailure "didn't get any infos?"
>        (0,_) -> assertEqual ("typecheck " ++ show srcs) sis $ Right is
>        _ -> assertEqual ("typecheck " ++ show srcs) sis $ Left er


> checkStatementInfoEnv :: Environment -> String -> Either [TypeError] [Maybe StatementInfo] -> Test.Framework.Test
> checkStatementInfoEnv env src sis = testCase ("typecheck " ++ src) $
>   let ast = case parseSql src of
>                               Left e -> error $ show e
>                               Right l -> l
>       aast = annotateAstEnv env ast
>       is = getTopLevelInfos aast
>       er = concatMap snd $ getTypeErrors aast
>   in {-trace (show aast) $-} case (length er, length is) of
>        (0,0) -> assertFailure "didn't get any infos?"
>        (0,_) -> assertEqual ("typecheck " ++ src) sis $ Right is
>        _ -> assertEqual ("typecheck " ++ src) sis $ Left er

> checkStatementInfoEu :: String -> Either [TypeError] [Maybe StatementInfo] -> [[EnvironmentUpdate]] -> Test.Framework.Test
> checkStatementInfoEu src sis eu = testCase ("typecheck " ++ src) $
>   let ast = case parseSql src of
>                               Left e -> error $ show e
>                               Right l -> l
>       aast = annotateAst ast
>       is = getTopLevelInfos aast
>       er = concatMap snd $ getTypeErrors aast
>       eu' = getTopLevelEnvUpdates aast
>   in {-trace (show aast) $-} case (length er, length is, length eu') of
>        (0,0,0) -> assertFailure "didn't get any infos or envupdates?"
>        (0,_,_) -> do
>          assertEqual ("typecheck " ++ src) sis $ Right is
>          assertEqual ("eu " ++ src) eu eu'
>        _ -> assertEqual ("typecheck " ++ src) sis $ Left er
