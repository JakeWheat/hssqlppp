
> module Database.HsSqlPpp.Tests.TypeChecking.TypeCheckTests
>     (typeCheckTests
>     ,typeCheckTestData
>     ,Item(..)) where

> import Test.Framework


> import Database.HsSqlPpp.Tests.TypeChecking.Utils

> import Database.HsSqlPpp.Tests.TypeChecking.TableRefTests
> import Database.HsSqlPpp.Tests.TypeChecking.TpchTests
> import Database.HsSqlPpp.Tests.TypeChecking.TypeInferenceTests
> import Database.HsSqlPpp.Tests.TypeChecking.Literals
> import Database.HsSqlPpp.Tests.TypeChecking.SimpleExpressions
> import Database.HsSqlPpp.Tests.TypeChecking.SpecialFunctions
> import Database.HsSqlPpp.Tests.TypeChecking.RowCtors
> import Database.HsSqlPpp.Tests.TypeChecking.CaseExpressions
> import Database.HsSqlPpp.Tests.TypeChecking.MiscExpressions
> import Database.HsSqlPpp.Tests.TypeChecking.SimpleSelects
> import Database.HsSqlPpp.Tests.TypeChecking.CombineSelects
> import Database.HsSqlPpp.Tests.TypeChecking.SelectFrom
> import Database.HsSqlPpp.Tests.TypeChecking.Joins
> import Database.HsSqlPpp.Tests.TypeChecking.Qualification
> import Database.HsSqlPpp.Tests.TypeChecking.MiscSelects
> import Database.HsSqlPpp.Tests.TypeChecking.Insert
> import Database.HsSqlPpp.Tests.TypeChecking.Update
> import Database.HsSqlPpp.Tests.TypeChecking.Delete
> import Database.HsSqlPpp.Tests.TypeChecking.Creates
> import Database.HsSqlPpp.Tests.TypeChecking.Plpgsql
> import Database.HsSqlPpp.Tests.TypeChecking.CatalogChaining
> import Database.HsSqlPpp.Tests.TypeChecking.Into
> import Database.HsSqlPpp.Tests.TypeChecking.Drops
> import Database.HsSqlPpp.Tests.TypeChecking.Triggers
> import Database.HsSqlPpp.Tests.TypeChecking.Misc

> typeCheckTestData :: Item
> typeCheckTestData = Group "typeCheckTests" typeCheckTestList

> typeCheckTestList :: [Item]
> typeCheckTestList = [tcLiteralTestData
>                     ,tcSimpleExpressionTestData
>                     ,tcSpecialFunctionsTestData
>                     ,tcRowCtorsTestData
>                     ,caseExpressionsTestData
>                     ,miscExpressionsTestData
>                     ,tcSimpleSelectsTestData
>                     ,tcCombineSelectsTestData
>                     ,tcSelectFromTestData
>                     ,tcJoinsTestData
>                     ,tcQualificationTestData
>                     ,tcMiscSelectTestData
>                     ,tcInsertTestData
>                     ,tcUpdateTestData
>                     ,tcDeleteTestData
>                     ,tcCreateTestData
>                     ,tcPlpgsqlTestData
>                     ,tcCatalogChainingTestData
>                     ,tcIntoTestData
>                     ,tcDropsTestData
>                     ,tcTriggersTestData
>                     ,tcMiscTestData]

todo:

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


~~~~
check insert returning, update returning, delete returning, one check each
check select into: multiple vars, record (then access fields to check),
  composite var
check errors: select into wrong number of vars, wrong types, and into
  composite wrong number and wrong type
~~~~




> typeCheckTests :: Test.Framework.Test
> typeCheckTests =
>   testGroup "typeChecking" $
>                 [tableRefTests
>                 --,tpchTests
>                 ,typeInferenceTests]
>                 ++ map itemToTft typeCheckTestList
