Copyright 2009 Jake Wheat

Set of tests to check the type checking code

> module Database.HsSqlPpp.AstCheckTests (astCheckTests) where

> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> import Data.Char
> import Control.Arrow
> import Debug.Trace

> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Parser
> import Database.HsSqlPpp.Scope
> import Database.HsSqlPpp.ScopeData
> import Database.HsSqlPpp.TypeType

> astCheckTests :: Test.Framework.Test
> astCheckTests = testGroup "astCheckTests" [
>     --testGroup "test test"
>     --(mapAttr [
>     --  p "select 1;" []
>     -- ])
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

>    ,testGroup "expressions and scope"
>     (mapExprScopeType [
>      t "a" (makeScope [("test", [("a", typeInt)])]) $ Right typeInt
>     ,t "b" (makeScope [("test", [("a", typeInt)])])
>        $ Left [UnrecognisedIdentifier "b"]
>     ])

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
>       p "select 1;" $ Right [SelectInfo $ SetOfType $
>                              UnnamedCompositeType [("?column?", typeInt)]]
>      ,p "select 1 as a;" $
>         Right [SelectInfo $ SetOfType $ UnnamedCompositeType [("a", typeInt)]]
>      ,p "select 1,2;" $
>         Right [SelectInfo $ SetOfType $ UnnamedCompositeType [("?column?", typeInt)
>                                                 ,("?column?", typeInt)]]
>      ,p "select 1 as a, 2 as b;" $
>         Right [SelectInfo $ SetOfType $ UnnamedCompositeType [("a", typeInt)
>                                                 ,("b", typeInt)]]
>      ,p "select 1+2 as a, 'a' || 'b';" $
>         Right [SelectInfo $ SetOfType $
>                UnnamedCompositeType [("a", typeInt)
>                                     ,("?column?", ScalarType "text")]]
>      ,p "values (1,2);" $ Right [SelectInfo $ SetOfType $
>                          UnnamedCompositeType
>                          [("column1", typeInt)
>                          ,("column2", typeInt)]]
>      ,p "values (1,2),('3', '4');" $ Right [SelectInfo $ SetOfType $
>                                      UnnamedCompositeType
>                                      [("column1", typeInt)
>                                      ,("column2", typeInt)]]
>      ,p "values (1,2),('a', true);" $ Left [IncompatibleTypeSet [typeInt
>                                                                 ,typeBool]]
>      ,p "values ('3', '4'),(1,2);" $ Right [SelectInfo $ SetOfType $
>                                      UnnamedCompositeType
>                                      [("column1", typeInt)
>                                      ,("column2", typeInt)]]
>      ,p "values ('a', true),(1,2);" $ Left [IncompatibleTypeSet [typeBool
>                                                                 ,typeInt]]
>      ,p "values ('a'::text, '2'::int2),('1','2');" $ Right [SelectInfo $ SetOfType $
>                                      UnnamedCompositeType
>                                      [("column1", ScalarType "text")
>                                      ,("column2", typeSmallInt)]]
>      ,p "values (1,2,3),(1,2);" $ Left [ValuesListsMustBeSameLength]
>      ])

>    ,testGroup "simple combine selects"
>     (mapStatementInfo [
>      p "select 1,2  union select '3', '4';" $ Right [SelectInfo $ SetOfType $
>                                      UnnamedCompositeType
>                                      [("?column?", typeInt)
>                                      ,("?column?", typeInt)]]
>      ,p "select 1,2 intersect select 'a', true;" $ Left [IncompatibleTypeSet [typeInt
>                                                         ,typeBool]]
>      ,p "select '3', '4' except select 1,2;" $ Right [SelectInfo $ SetOfType $
>                                      UnnamedCompositeType
>                                      [("?column?", typeInt)
>                                      ,("?column?", typeInt)]]
>      ,p "select 'a', true union select 1,2;"
>                                      $ Left [IncompatibleTypeSet [typeBool
>                                                         ,typeInt]]
>      ,p "select 'a'::text, '2'::int2 intersect select '1','2';" $ Right [SelectInfo $ SetOfType $
>                                      UnnamedCompositeType
>                                      [("text", ScalarType "text")
>                                      ,("int2", typeSmallInt)]]
>      ,p "select 1,2,3 except select 1,2;" $ Left [ValuesListsMustBeSameLength]
>      ,p "select '3' as a, '4' as b except select 1,2;" $ Right [SelectInfo $ SetOfType $
>                                      UnnamedCompositeType
>                                      [("a", typeInt)
>                                      ,("b", typeInt)]]
>      ])


>    ,testGroup "simple selects from"
>     (mapStatementInfo [
>       p "select a from (select 1 as a, 2 as b) x;"
>         $ Right [SelectInfo $ SetOfType $ UnnamedCompositeType [("a", typeInt)]]
>      ,p "select b from (select 1 as a, 2 as b) x;"
>         $ Right [SelectInfo $ SetOfType $ UnnamedCompositeType [("b", typeInt)]]
>      ,p "select c from (select 1 as a, 2 as b) x;"
>         $ Left [UnrecognisedIdentifier "c"]
>      ,p "select typlen from pg_type;"
>         $ Right [SelectInfo $ SetOfType $ UnnamedCompositeType [("typlen", typeSmallInt)]]
>      ,p "select oid from pg_type;"
>         $ Right [SelectInfo $ SetOfType $ UnnamedCompositeType [("oid", ScalarType "oid")]]
>      ,p "select p.oid from pg_type p;"
>         $ Right [SelectInfo $ SetOfType $ UnnamedCompositeType [("oid", ScalarType "oid")]]
>      ,p "select typlen from nope;"
>         $ Left [UnrecognisedRelation "nope"]
>      ,p "select generate_series from generate_series(1,7);"
>         $ Right [SelectInfo $ SetOfType $ UnnamedCompositeType [("generate_series", typeInt)]]

check aliasing

>      ,p "select generate_series.generate_series from generate_series(1,7);"
>         $ Right [SelectInfo $ SetOfType $ UnnamedCompositeType [("generate_series", typeInt)]]
>      ,p "select g from generate_series(1,7) g;"
>         $ Right [SelectInfo $ SetOfType $ UnnamedCompositeType [("g", typeInt)]]
>      ,p "select g.g from generate_series(1,7) g;"
>         $ Right [SelectInfo $ SetOfType $ UnnamedCompositeType [("g", typeInt)]]
>      ,p "select generate_series.g from generate_series(1,7) g;"
>         $ Left [UnrecognisedCorrelationName "generate_series"]
>      ,p "select g.generate_series from generate_series(1,7) g;"
>         $ Left [UnrecognisedIdentifier "g.generate_series"]


>      ,p "select * from pg_attrdef;"
>         $ Right [SelectInfo $ SetOfType $ UnnamedCompositeType
>          [("adrelid",ScalarType "oid")
>          ,("adnum",ScalarType "int2")
>          ,("adbin",ScalarType "text")
>          ,("adsrc",ScalarType "text")]]
>      ,p "select abs from abs(3);"
>         $ Right [SelectInfo $ SetOfType $ UnnamedCompositeType [("abs", typeInt)]]
>         --todo: these are both valid,
>         --the second one means select 3+generate_series from generate_series(1,7)
>         --  select generate_series(1,7);
>         -- select 3 + generate_series(1,7);
>      ])


>  {-  ,testGroup "simple selects from 2"
>     (mapStatementTypeScope [
>
>       t "select a,b from testfunc();"
>         (let fn = ("testfunc", [], SetOfType $ CompositeType "testType")
>          in emptyScope {scopeFunctions = [fn]
>                        ,scopeAllFns = [fn]
>                        ,scopeAttrDefs =
>                         [("testType"
>                          ,Composite
>                          ,UnnamedCompositeType
>                             [("a", ScalarType "text")
>                             ,("b", typeInt)
>                             ,("c", typeInt)])]})
>         [SetOfType $ UnnamedCompositeType
>          [("a",ScalarType "text"),("b",ScalarType "int4")]]

>      ,t "select testfunc();"
>         (let fn = ("testfunc", [], Pseudo Void)
>          in emptyScope {scopeFunctions = [fn]
>                        ,scopeAllFns = [fn]
>                        ,scopeAttrDefs =
>                         []})
>         [Pseudo Void]

>      ])

>    ,testGroup "simple join selects"
>     (mapStatementType [
>       p "select * from (select 1 as a, 2 as b) a\n\
>         \  cross join (select true as c, 4.5 as d) b;"
>         [SetOfType $ UnnamedCompositeType [("a", typeInt)
>                                           ,("b", typeInt)
>                                           ,("c", typeBool)
>                                           ,("d", typeNumeric)]]
>      ,p "select * from (select 1 as a, 2 as b) a\n\
>         \  inner join (select true as c, 4.5 as d) b on true;"
>         [SetOfType $ UnnamedCompositeType [("a", typeInt)
>                                           ,("b", typeInt)
>                                           ,("c", typeBool)
>                                           ,("d", typeNumeric)]]
>      ,p "select * from (select 1 as a, 2 as b) a\n\
>         \  inner join (select 1 as a, 4.5 as d) b using(a);"
>         [SetOfType $ UnnamedCompositeType [("a", typeInt)
>                                           ,("b", typeInt)
>                                           ,("d", typeNumeric)]]
>      ,p "select * from (select 1 as a, 2 as b) a\n\
>         \ natural inner join (select 1 as a, 4.5 as d) b;"
>         [SetOfType $ UnnamedCompositeType [("a", typeInt)
>                                           ,("b", typeInt)
>                                           ,("d", typeNumeric)]]
>         --check the attribute order
>      ,p "select * from (select 2 as b, 1 as a) a\n\
>         \ natural inner join (select 4.5 as d, 1 as a) b;"
>         [SetOfType $ UnnamedCompositeType [("a", typeInt)
>                                           ,("b", typeInt)
>                                           ,("d", typeNumeric)]]
>      ,p "select * from (select 1 as a, 2 as b) a\n\
>         \ natural inner join (select true as a, 4.5 as d) b;"
>         [TypeError (IncompatibleTypeSet [ScalarType "int4"
>                                                ,ScalarType "bool"])]
>      ])

>    ,testGroup "simple scalar identifier qualification"
>     (mapStatementType [
>       p "select a.* from \n\
>         \(select 1 as a, 2 as b) a \n\
>         \cross join (select 3 as c, 4 as d) b;"
>         [SetOfType $ UnnamedCompositeType [("a", typeInt)
>                                           ,("b", typeInt)]]
>      ,p "select nothere.* from \n\
>         \(select 1 as a, 2 as b) a \n\
>         \cross join (select 3 as c, 4 as d) b;"
>         [SetOfType $ UnnamedCompositeType [("a", typeInt)
>                                           ,("b", typeInt)]]
>      ,p "select a.b,b.c from \n\
>         \(select 1 as a, 2 as b) a \n\
>         \natural inner join (select 3 as a, 4 as c) b;"
>         [SetOfType $ UnnamedCompositeType [("b", typeInt)
>                                           ,("c", typeInt)]]
>      ,p "select a.a,b.a from \n\
>         \(select 1 as a, 2 as b) a \n\
>         \natural inner join (select 3 as a, 4 as c) b;"
>         [SetOfType $ UnnamedCompositeType [("a", typeInt)
>                                           ,("a", typeInt)]]

>      ,p "select pg_attrdef.adsrc from pg_attrdef;"
>         [SetOfType $ UnnamedCompositeType [("adsrc", ScalarType "text")]]

>      ,p "select a.adsrc from pg_attrdef a;"
>         [SetOfType $ UnnamedCompositeType [("adsrc", ScalarType "text")]]

>      ,p "select pg_attrdef.adsrc from pg_attrdef a;"
>         [TypeError (UnrecognisedCorrelationName "pg_attrdef")]

>      ,p "select a from (select 2 as b, 1 as a) a\n\
>         \natural inner join (select 4.5 as d, 1 as a) b;"
>         [SetOfType $ UnnamedCompositeType [("a", typeInt)]]

select g.fn from fn() g


>      ])

>    ,testGroup "aggregates"
>     (mapStatementType [
>        p "select max(prorettype::int) from pg_proc;"
>         [SetOfType $ UnnamedCompositeType [("max", typeInt)]]
>      ])

>    ,testGroup "simple wheres"
>     (mapStatementType [
>       p "select 1 from pg_type where true;"
>         [SetOfType $ UnnamedCompositeType [("?column?", typeInt)]]
>      ,p "select 1 from pg_type where 1;"
>         [TypeError ExpressionMustBeBool]
>      ,p "select typname from pg_type where typbyval;"
>         [SetOfType $ UnnamedCompositeType [("typname", ScalarType "name")]]
>      ,p "select typname from pg_type where typtype = 'b';"
>         [SetOfType $ UnnamedCompositeType [("typname", ScalarType "name")]]
>      ,p "select typname from pg_type where what = 'b';"
>         [TypeError (UnrecognisedIdentifier "what")]
>      ])

>    ,testGroup "subqueries"
>     (mapStatementType [
>       p "select relname as relvar_name\n\
>         \    from pg_class\n\
>         \    where ((relnamespace =\n\
>         \           (select oid\n\
>         \              from pg_namespace\n\
>         \              where (nspname = 'public'))) and (relkind = 'r'));"
>         [SetOfType $ UnnamedCompositeType [("relvar_name",ScalarType "name")]]
>      ,p "select relname from pg_class where relkind in ('r', 'v');"
>         [SetOfType $ UnnamedCompositeType [("relname",ScalarType "name")]]
>      ,p "select * from generate_series(1,7) g\n\
>         \where g not in (select * from generate_series(3,5));"
>         [SetOfType (UnnamedCompositeType [("g",ScalarType "int4")])]
>      ])

insert

>    ,testGroup "insert"
>     (mapStatementInfo [
>       p "insert into nope (a,b) values (c,d);"
>         [DefaultStatementInfo (TypeError (UnrecognisedRelation "nope"))]
>      ,p "insert into pg_attrdef (adrelid,adnum,adbin,adsrc)\n\
>         \values (1,2, 'a', 'b');"
>         [InsertInfo "pg_attrdef"
>          (UnnamedCompositeType [("adrelid",ScalarType "oid")
>                                 ,("adnum",ScalarType "int2")
>                                 ,("adbin",ScalarType "text")
>                                 ,("adsrc",ScalarType "text")])]
>      ,p "insert into pg_attrdef\n\
>         \values (1,2, 'a', 'b');"
>         [InsertInfo "pg_attrdef"
>          (UnnamedCompositeType [("adrelid",ScalarType "oid")
>                                 ,("adnum",ScalarType "int2")
>                                 ,("adbin",ScalarType "text")
>                                 ,("adsrc",ScalarType "text")])]
>      ,p "insert into pg_attrdef (hello,adnum,adbin,adsrc)\n\
>         \values (1,2, 'a', 'b');"
>         [DefaultStatementInfo (TypeError (UnrecognisedIdentifier "hello"))]
>      ,p "insert into pg_attrdef (adrelid,adnum,adbin,adsrc)\n\
>         \values (1,true, 'a', 'b');"
>         [DefaultStatementInfo (TypeError (IncompatibleTypes (ScalarType "int2") (ScalarType "bool")))]
>      ,p "insert into pg_attrdef (adrelid,adnum,adbin,adsrc)\n\
>         \values (1,true, 'a', 'b','c');"
>         [DefaultStatementInfo (TypeError WrongNumberOfColumns)]

>      ])

>    ,testGroup "update"
>     (mapStatementInfo [
>       p "update nope set a = 1;"
>         [DefaultStatementInfo (TypeError (UnrecognisedRelation "nope"))]
>      ,p "update pg_attrdef set adsrc = '' where 1;"
>         [DefaultStatementInfo (TypeError ExpressionMustBeBool)]
>      ,p "update pg_attrdef set (adbin,adsrc) = ('a','b','c');"
>         [DefaultStatementInfo (TypeError WrongNumberOfColumns)]
>      ,p "update pg_attrdef set (adrelid,adsrc) = (true,'b');"
>         [DefaultStatementInfo (TypeError (IncompatibleTypes (ScalarType "oid") typeBool))]
>      ,p "update pg_attrdef set (shmadrelid,adsrc) = ('a','b');"
>         [DefaultStatementInfo (TypeError (UnrecognisedIdentifier "shmadrelid"))]
>      ,p "update pg_attrdef set adsrc='';"
>         [UpdateInfo "pg_attrdef" (UnnamedCompositeType [("adsrc",ScalarType "text")])]
>      ,p "update pg_attrdef set adsrc='' where 1=2;"
>         [UpdateInfo "pg_attrdef" (UnnamedCompositeType [("adsrc",ScalarType "text")])]
>       -- TODO: actually, pg doesn't support this so need to generate error instead
>      ,p "update pg_attrdef set (adbin,adsrc) = ((select 'a','b'));"
>         [UpdateInfo "pg_attrdef" (UnnamedCompositeType [("adbin",ScalarType "text"),("adsrc",ScalarType "text")])]
>      ])

>    ,testGroup "delete"
>     (mapStatementInfo [
>       p "delete from nope;"
>         [DefaultStatementInfo (TypeError (UnrecognisedRelation "nope"))]
>      ,p "delete from pg_attrdef where 1=2;"
>         [DeleteInfo "pg_attrdef"]
>      ,p "delete from pg_attrdef where 1;"
>         [DefaultStatementInfo (TypeError ExpressionMustBeBool)]
>      ])
> -}
>    ]
>         where
>           --mapAttr = map $ uncurry checkAttrs
>           p a b = (a,b)
>           t a b c = (a,b,c)
>           mapExprType = map (uncurry $ checkExpressionType emptyScope)
>           --mapStatementType = map $ uncurry checkStatementType
>           mapStatementInfo = map $ uncurry checkStatementInfo
>           mapExprScopeType = map (\(a,b,c) -> checkExpressionType b a c)
>           makeScope l = scopeReplaceIds defaultScope (map (second (\a->(a,[]))) l) []
>           --mapStatementTypeScope = map (\(a,b,c) -> checkStatementTypeScope b a c)

> {-
> checkAttrs :: String -> [Message] -> Test.Framework.Test
> checkAttrs src msgs = testCase ("check " ++ src) $ do
>   let ast = case parseSql src of
>                Left er -> assertFailure $ show er
>                Right l -> l
>       msgs1 = checkAst ast
>   assertEqual ("check " ++ src) msgs msgs1
> -}

> checkExpressionType :: Scope -> String -> Either [TypeError] Type -> Test.Framework.Test
> checkExpressionType scope src typ = testCase ("typecheck " ++ src) $
>   let ast = case parseExpression src of
>                                      Left e -> error $ show e
>                                      Right l -> l
>       aast = annotateExpression scope ast
>       ty = getTopLevelTypes [aast]
>       er = getTypeErrors [aast]
>   in case (length er, length ty) of
>        (0,0) -> assertFailure "didn't get any types?"
>        (0,1) -> assertEqual ("typecheck " ++ src) typ $ Right $ head ty
>        (0,_) -> assertFailure "got too many types"
>        _ -> assertEqual ("typecheck " ++ src) typ $ Left er

> checkStatementInfo :: String -> Either [TypeError] [StatementInfo] -> Test.Framework.Test
> checkStatementInfo src sis = testCase ("typecheck " ++ src) $
>   let ast = case parseSql src of
>                               Left e -> error $ show e
>                               Right l -> l
>       aast = annotateAst ast
>       is = getTopLevelInfos aast
>       er = getTypeErrors aast
>   in {-trace (show aast) $-} case (length er, length is) of
>        (0,0) -> assertFailure "didn't get any infos?"
>        (0,_) -> assertEqual ("typecheck " ++ src) sis $ Right is
>        _ -> assertEqual ("typecheck " ++ src) sis $ Left er


 >   in {-trace (show aast) $-} assertEqual ("typecheck " ++ src) sis combo


> {-
> checkStatementType :: String -> [Type] -> Test.Framework.Test
> checkStatementType src typ = testCase ("typecheck " ++ src) $
>   assertEqual ("typecheck " ++ src) typ (parseAndGetType src)

> checkStatementTypeScope ::  Scope -> String -> [Type] -> Test.Framework.Test
> checkStatementTypeScope scope src typ = testCase ("typecheck " ++ src) $
>   assertEqual ("typecheck " ++ src) typ (parseAndGetTypeScope scope src)

> checkStatementInfo :: String -> [StatementInfo] -> Test.Framework.Test
> checkStatementInfo src typ = testCase ("typecheck " ++ src) $
>   assertEqual ("typecheck " ++ src) typ (parseAndGetInfo src)

> parseAndGetInfo :: String -> [StatementInfo]
> parseAndGetInfo src =
>   let ast = case parseSql src of
>                               Left er -> error $ show er
>                               Right l -> l
>   in getStatementsInfo ast


> parseAndGetType :: String -> [Type]
> parseAndGetType src =
>   let ast = case parseSql src of
>                               Left er -> error $ show er
>                               Right l -> l
>   in getStatementsType ast

> parseAndGetTypeScope :: Scope -> String -> [Type]
> parseAndGetTypeScope scope src =
>   let ast = case parseSql src of
>                               Left er -> error $ show er
>                               Right l -> l
>   in getStatementsTypeScope scope ast
> -}

 > parseAndGetExpressionType :: Scope -> String -> Type
 > parseAndGetExpressionType scope src = undefined

 >   let ast = case parseExpression src of
 >                                      Left er -> error $ show er
 >                                      Right l -> l
 >   in getExpressionType scope ast
