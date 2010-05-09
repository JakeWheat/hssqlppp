
Local bindings tests: test updating the local bindings

This code contains most of the join lookup code.

New plan:

Write tests against the local bindings module for the full variety of
behaviour

local bindings are updated by
* tref (or table reference in an update/insert/delete)
* parameter in function
* declaration in function block
* implicit integer loop var in for loop
* set explicit record type in for loop/ assignment to record type
* for constraints in create table, create domain
* funcall "."

Write tests to quickly check each bit of code which uses these using
the full typechecking:
update: sets, where, returning
select: tref -> select list, where, group by, order by
join: out to tref, into on expression
implicit variable in for loop
record type in for loop
record type in assignment
record type in select into
delete where and returning
block declarations
constraints in create table, create domain
parameters in function body
statementlist: pass on record updates?
insert: columns?, returning








-------------------------------

Tests for the local bindings lookup code, which is a bit convoluted in
places, particularly for joins.

The tests are also quite convoluted, but the main simplification to
try to make them understandable is that we load a bunch of
localbindingupdates, then we just check the entire lookup values that
tell us what each identifier or star expansion returns, rather than
looking individual items up and checking the results.

> {-# LANGUAGE ScopedTypeVariables #-}
>
> module Database.HsSqlPpp.Tests.LocalBindingsTests (localBindingsTests) where
>
> import Test.HUnit
> import Test.Framework
> import Test.Framework.Providers.HUnit
> --import Debug.Trace
> import Control.Monad
> import Control.Monad.Error
>
> import Database.HsSqlPpp.AstInternals.TypeChecking.LocalBindingsInternal
> import Database.HsSqlPpp.SqlTypes
> import Database.HsSqlPpp.Catalog
>
> data Item = Group String [Item]
>           | Item [(String
>                   ,[LocalBindingsUpdate]
>                   ,Either [TypeError] [LocalBindingsLookup])]
>
> localBindingsTests :: [Test.Framework.Test]
> localBindingsTests = itemToTft testData
>
> testData :: Item
> testData = Group "local bindings tests" [ Item [
>   ("test empty", [LBIds "source1" Nothing []]
>   ,Right [LocalBindingsLookup [] (Left [BadStarExpand])])
>  ,("test lbids no cor", [LBIds "source1" Nothing [("a", typeInt)
>                                                  ,("b", typeBool)]]
>   ,Right [LocalBindingsLookup [
>            (("a"), Right ("source1", ["a"], typeInt))
>           ,(("b"), Right ("source1", ["b"], typeBool))
>           ]
>           (Left [BadStarExpand])])
>  ,("test lbids cor", [LBIds "source1" (Just "c") [("a", typeInt)
>                                                  ,("b", typeBool)]]
>   ,Right [LocalBindingsLookup [
>            (("c"), Right ("source1", ["c"], CompositeType [("a", typeInt)
>                                                           ,("b", typeBool)]))
>           ,(("a"), Right ("source1", ["c","a"], typeInt))
>           ,(("b"), Right ("source1", ["c","b"], typeBool))]
>           (Left [BadStarExpand])])
>  ,("test tref", [LBTref "source2" "t1"
>                         [("a", typeInt)
>                         ,("b", typeBool)]
>                         [("c", ScalarType "text")
>                         ,("d", typeSmallInt)]
>                  ]
>   ,Right [LocalBindingsLookup [
>            (("t1"), Right ("source2", ["t1"], CompositeType [("a", typeInt)
>                                                             ,("b", typeBool)]))
>           ,(("a"), Right ("source2", ["t1","a"], typeInt))
>           ,(("b"), Right ("source2", ["t1","b"], typeBool))
>           ,(("c"), Right ("source2", ["t1","c"], ScalarType "text"))
>           ,(("d"), Right ("source2", ["t1","d"], typeSmallInt))]
>           (Right [("source2", ["t1","a"], typeInt)
>                  ,("source2", ["t1","b"], typeBool)])])

TODO: test lbjointrefs
cross join
natural join
using join
 - public and system attrs, ambiguous lookups
join columns incompatible: natural and using variants
three way join test

sys columns are accessible qualified only in joins

put . lookup logic into localbindings and test:
id not found
lbids using correlation name
lbids using record type
tref using correlation name
jointref using t1 name, t2 name, alias



>  {-,("test ids cor", [LBIds "source1" "c" [("a", typeInt)
>                                           ,("b", typeBool)]
>                                           [("ia", typeInt)
>                                           ,("ib", typeBool)]]
>   ,Right [LocalBindingsLookup [
>            (("", "a"), Right ("source1", "c", "a", typeInt))
>           ,(("", "b"), Right ("source1", "c", "b", typeBool))
>           ,(("", "ia"), Right ("source1", "c", "ia", typeInt))
>           ,(("", "ib"), Right ("source1", "c", "ib", typeBool))
>           ,(("c", "a"), Right ("source1", "c", "a", typeInt))
>           ,(("c", "b"), Right ("source1", "c", "b", typeBool))
>           ,(("c", "ia"), Right ("source1", "c", "ia", typeInt))
>           ,(("c", "ib"), Right ("source1", "c", "ib", typeBool))
>           ]
>           [("", Right [
>                    ("source1", "c", "a", typeInt)
>                   ,("source1", "c", "b", typeBool)])
>           ,("c", Right [
>                    ("source1", "c", "a", typeInt)
>                   ,("source1", "c", "b", typeBool)])
>            ]])
>
>  ,("test parallel", [LBParallel
>     (LBIds "source1" "c1" [("a", typeInt)
>                         ,("b", typeBool)]
>                         [("ia", typeInt)
>                         ,("ib", typeBool)])
>     (LBIds "source2" "c2" [("a1", typeInt)
>                         ,("b", typeBool)]
>                         [("ia1", typeInt)
>                         ,("ib", typeBool)])]
>   ,Right [LocalBindingsLookup [
>            (("", "a"), Right ("source1", "c1", "a", typeInt))
>           ,(("", "ia"), Right ("source1", "c1", "ia", typeInt))
>           ,(("c1", "a"), Right ("source1", "c1", "a", typeInt))
>           ,(("c1", "b"), Right ("source1", "c1", "b", typeBool))
>           ,(("c1", "ia"), Right ("source1", "c1", "ia", typeInt))
>           ,(("c1", "ib"), Right ("source1", "c1", "ib", typeBool))
>           ,(("", "a1"), Right ("source2", "c2", "a1", typeInt))
>           ,(("", "ia1"), Right ("source2", "c2", "ia1", typeInt))
>           ,(("c2", "a1"), Right ("source2", "c2", "a1", typeInt))
>           ,(("c2", "b"), Right ("source2", "c2", "b", typeBool))
>           ,(("c2", "ia1"), Right ("source2", "c2", "ia1", typeInt))
>           ,(("c2", "ib"), Right ("source2", "c2", "ib", typeBool))
>           ,(("", "b"), Left [AmbiguousIdentifier "b"])
>           ,(("", "ib"), Left [AmbiguousIdentifier "ib"])
>           ]
>           [("", Right [("source1", "c1", "a", typeInt)
>                       ,("source1", "c1", "b", typeBool)
>                       ,("source2", "c2", "a1", typeInt)
>                       ,("source2", "c2", "b", typeBool)])
>           ,("c1", Right [("source1", "c1", "a", typeInt)
>                         ,("source1", "c1", "b", typeBool)])
>           ,("c2", Right [("source2", "c2", "a1", typeInt)
>                         ,("source2", "c2", "b", typeBool)])
>                   ]])
>  ,("test join 1", [LBJoinIds
>                    (LBIds "s1" "c1" [("a", typeInt)] [("b", typeBool)])
>                    (LBIds "s2" "c2" [("c", typeInt)] [("d", typeBool)])
>                    (Right []) ""]
>   ,Right [LocalBindingsLookup
>            [(("", "a"), Right ("s1", "c1", "a", typeInt))
>            ,(("", "b"), Right ("s1", "c1", "b", typeBool))
>            ,(("c1", "a"), Right ("s1", "c1", "a", typeInt))
>            ,(("c1", "b"), Right ("s1", "c1", "b", typeBool))
>            ,(("", "c"), Right ("s2", "c2", "c", typeInt))
>            ,(("", "d"), Right ("s2", "c2", "d", typeBool))
>            ,(("c2", "c"), Right ("s2", "c2", "c", typeInt))
>            ,(("c2", "d"), Right ("s2", "c2", "d", typeBool))]
>            [("", Right [("s1", "c1", "a", typeInt)
>                        ,("s2", "c2", "c", typeInt)])
>            ,("c1", Right [("s1", "c1", "a", typeInt)])
>            ,("c2", Right [("s2", "c2", "c", typeInt)])
>            ]])
>
>  ,("test natural join", [LBJoinIds
>                    (LBIds "s1" "c1" [("a", typeInt), ("b", typeBool)] [])
>                    (LBIds "s2" "c2" [("a", typeInt), ("c", typeBool)] [])
>                    (Left ()) ""]
>   ,Right [LocalBindingsLookup
>            [(("", "a"), Right ("s1", "c1", "a", typeInt))
>            ,(("c1", "a"), Right ("s1", "c1", "a", typeInt))
>            ,(("c2", "a"), Right ("s2", "c2", "a", typeInt))
>            ,(("", "b"), Right ("s1", "c1", "b", typeBool))
>            ,(("c1", "b"), Right ("s1", "c1", "b", typeBool))
>            ,(("", "c"), Right ("s2", "c2", "c", typeBool))
>            ,(("c2", "c"), Right ("s2", "c2", "c", typeBool))]
>            [("", Right [("s1", "c1", "a", typeInt)
>                        ,("s1", "c1", "b", typeBool)
>                        ,("s2", "c2", "c", typeBool)])
>            ,("c1", Right [("s1", "c1", "a", typeInt)
>                          ,("s1", "c1", "b", typeBool)])
>            ,("c2", Right [("s2", "c2", "a", typeInt)
>                          ,("s2", "c2", "c", typeBool)])
>            ]])
>
>  ,("test natural join", [LBJoinIds
>                    (LBIds "s1" "c1" [("b", typeBool),("a", typeInt)] [])
>                    (LBIds "s2" "c2" [("c", typeBool),("a", typeInt)] [])
>                    (Left ()) ""]
>   ,Right [LocalBindingsLookup
>            [(("", "a"), Right ("s1", "c1", "a", typeInt))
>            ,(("c1", "a"), Right ("s1", "c1", "a", typeInt))
>            ,(("c2", "a"), Right ("s2", "c2", "a", typeInt))
>            ,(("", "b"), Right ("s1", "c1", "b", typeBool))
>            ,(("c1", "b"), Right ("s1", "c1", "b", typeBool))
>            ,(("", "c"), Right ("s2", "c2", "c", typeBool))
>            ,(("c2", "c"), Right ("s2", "c2", "c", typeBool))]
>            [("", Right [("s1", "c1", "a", typeInt)
>                        ,("s1", "c1", "b", typeBool)
>                        ,("s2", "c2", "c", typeBool)])
>            ,("c1", Right [("s1", "c1", "a", typeInt)
>                          ,("s1", "c1", "b", typeBool)])
>            ,("c2", Right [("s2", "c2", "a", typeInt)
>                          ,("s2", "c2", "c", typeBool)])
>            ]])
>
>  ,("test using join", [LBJoinIds
>                    (LBIds "s1" "c1" [("b", typeBool),("a", typeInt)] [])
>                    (LBIds "s2" "c2" [("c", typeBool),("a", typeInt)] [])
>                    (Right ["a"]) ""]
>   ,Right [LocalBindingsLookup
>            [(("", "a"), Right ("s1", "c1", "a", typeInt))
>            ,(("c1", "a"), Right ("s1", "c1", "a", typeInt))
>            ,(("c2", "a"), Right ("s2", "c2", "a", typeInt))
>            ,(("", "b"), Right ("s1", "c1", "b", typeBool))
>            ,(("c1", "b"), Right ("s1", "c1", "b", typeBool))
>            ,(("", "c"), Right ("s2", "c2", "c", typeBool))
>            ,(("c2", "c"), Right ("s2", "c2", "c", typeBool))]
>            [("", Right [("s1", "c1", "a", typeInt)
>                        ,("s1", "c1", "b", typeBool)
>                        ,("s2", "c2", "c", typeBool)])
>            ,("c1", Right [("s1", "c1", "a", typeInt)
>                          ,("s1", "c1", "b", typeBool)])
>            ,("c2", Right [("s2", "c2", "a", typeInt)
>                          ,("s2", "c2", "c", typeBool)])
>            ]])
>
>  ,("test join with alias", [LBJoinIds
>                    (LBIds "s1" "c1" [("b", typeBool),("a", typeInt)] [])
>                    (LBIds "s2" "c2" [("c", typeBool),("a", typeInt)] [])
>                    (Right ["a"]) "t3"]
>   ,Right [LocalBindingsLookup
>            [(("", "a"), Right ("s1", "t3", "a", typeInt))
>            ,(("", "b"), Right ("s1", "t3", "b", typeBool))
>            ,(("", "c"), Right ("s2", "t3", "c", typeBool))
>            ,(("t3", "a"), Right ("s1", "t3", "a", typeInt))
>            ,(("t3", "b"), Right ("s1", "t3", "b", typeBool))
>            ,(("t3", "c"), Right ("s2", "t3", "c", typeBool))]
>            [("", Right [("s1", "t3", "a", typeInt)
>                        ,("s1", "t3", "b", typeBool)
>                        ,("s2", "t3", "c", typeBool)])
>            ,("t3", Right [("s1", "t3", "a", typeInt)
>                          ,("s1", "t3", "b", typeBool)
>                          ,("s2", "t3", "c", typeBool)])
>            ]])
>
>  ,("test composite type expansion", [LBIds "s1" "c1" [("r", NamedCompositeType "pg_attrdef")] []]
>   ,ctExpand (NamedCompositeType "pg_attrdef"))
>  ,let t = CompositeType ctFields
>   in ("test composite type expansion ct", [LBIds "s1" "c1" [("r", t)] []]
>      ,ctExpand t)
>  ,let t = PgRecord $ Just $ NamedCompositeType "pg_attrdef"
>   in ("test composite type expansion rec", [LBIds "s1" "c1" [("r", t)] []]
>      ,ctExpand t)
>  ,let t = PgRecord $ Just $ CompositeType ctFields
>   in ("test composite type expansion rec", [LBIds "s1" "c1" [("r", t)] []]
>      ,ctExpand t)-}

~~~~

  join update: join col type tests + incompatible, missing ol
? 4 layers no aliases
? 3 way with alias on inner1
non join field ambiguous name

joinids no common, no alias, no using list
  4 layers no aliases
  error missing field in join list
    imcompatible types
  non join field ambiguous name
  3 way join with alias on inner

chaos=# create view t as select * from (select 1 as a, 2 as a) b;
ERROR:  column "a" specified more than once

but

chaos=# select * from (select 1 as a, 2 as a) b;
 a | a
---+---
 1 | 2
(1 row)

chaos=# select b.* from (select 1 as a, 2 as a) b;
 a | a
---+---
 1 | 2
(1 row)

~~~~

>   ]]
>   where
>     {-ctFields = [("adrelid",ScalarType "oid")
>                ,("adnum",ScalarType "int2")
>                ,("adbin",ScalarType "text")
>                ,("adsrc",ScalarType "text")]
>     ctExpand t =
>        Right [LocalBindingsLookup
>            [(("","r"),Right ("s1","c1","r",t))
>             ,(("c1","r"),Right ("s1","c1","r",t))
>             ,(("r","adrelid"),Right ("s1","r","adrelid",ScalarType "oid"))
>             ,(("r","adnum"),Right ("s1","r","adnum",ScalarType "int2"))
>             ,(("r","adbin"),Right ("s1","r","adbin",ScalarType "text"))
>             ,(("r","adsrc"),Right ("s1","r","adsrc",ScalarType "text"))]
>            [("",Right [("s1","c1","r",t)])
>            ,("c1",Right [("s1","c1","r",t)])
>            ,("r",Right [("s1","r","adrelid",ScalarType "oid")
>                        ,("s1","r","adnum",ScalarType "int2")
>                        ,("s1","r","adbin",ScalarType "text")
>                        ,("s1","r","adsrc",ScalarType "text")])
>            ]]-}

--------------------------------------------------------------------------------

> testLookups :: String
>             -> [LocalBindingsUpdate]
>             -> Either [TypeError] [LocalBindingsLookup]
>             -> Test.Framework.Test
> testLookups n lbus lkps = testCase n $ do
>     let lkps1 = do
>                 (LocalBindings _ lkpsx) <- foldM (flip $ lbUpdate defaultTemplate1Catalog) emptyBindings lbus
>                 return lkpsx
>     when (lkps /= lkps1) $ liftIO $ putStrLn $ "expected " ++ showRes lkps
>                                         ++ "\ngot: " ++ showRes lkps1
>     liftIO $ assertEqual ("check") lkps lkps1
>     where
>       showRes :: Either [TypeError] [LocalBindingsLookup] -> String
>       showRes e = either show showLkps e
>       showLkps :: [LocalBindingsLookup] -> String
>       showLkps ls = concatMap ppLbls ls
>
> itemToTft :: Item -> [Test.Framework.Test]
> itemToTft (Item es) = map (\(a,b,c) -> testLookups a b c) es
> itemToTft (Group s is) = [testGroup s $ concatMap itemToTft is]
