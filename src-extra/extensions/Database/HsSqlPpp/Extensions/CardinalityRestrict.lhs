
constraint shorthand to restrict cardinality of table

> {-# LANGUAGE QuasiQuotes #-}
>
> module Database.HsSqlPpp.Extensions.CardinalityRestrict
>     where
>
> import Data.Generics
> import Data.Generics.Uniplate.Data
>
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Extensions.ExtensionsUtils
> import Database.HsSqlPpp.Quote
> import Database.HsSqlPpp.Pretty
> import Database.HsSqlPpp.Extensions.CreateAssertion
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Extensions.AstUtils

challenge: make a constraint that works like this:

select restrict_cardinality('tablename', '<= 3');

or

select restrict_cardinality('tablename', '>=5 && <= 10');


> cardinalityRestrictExample :: ExtensionTest
> cardinalityRestrictExample = ExtensionTest
>   "cardinalityRestrict"
>   (createAssertion . cardinalityRestrict)
>   [sqlStmts|
>
>      create table tablename (
>        varname vartype
>      );
>
>      select restrict_cardinality('tablename', 1); |]
>   (createAssertion [sqlStmts|
>
>      create table tablename (
>        varname vartype
>      );
>
>      select create_assertion('tablename_card',
>                              '(select count(*) from tablename) <= 1');
>
>   |])
>
> cardinalityRestrict :: Data a => a -> a
> cardinalityRestrict =
>     transformBi $ \x ->
>       case x of
>         s@[sqlStmt|
>           select restrict_cardinality($s(tablename), $(num)); |]
>             -> let --i = [$sqlExpr| $(num) |]
>                    expr = printScalarExpr
>                             [sqlExpr| (select count(*) from $(tablename))
>                                         <= $(num) |]
>                    conname = tablename ++ "_card"
>                in replaceSourcePos1 s [sqlStmt|
>
>                   select create_assertion($s(conname), $s(expr));
>
>                    |]
>         x1 -> x1
>
