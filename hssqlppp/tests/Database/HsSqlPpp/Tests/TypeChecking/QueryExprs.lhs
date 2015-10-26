
New set of tests for new typechecking. Will catch more issues than the
old typechecker, and the tests are focus on lots of checks for bad
typechecks as well as correct queries compared to the old tests.

> {-# LANGUAGE OverloadedStrings #-}
> module Database.HsSqlPpp.Tests.TypeChecking.QueryExprs
>     (queryExprs) where

> --import Database.HsSqlPpp.Internals.TypesInternal
> import Database.HsSqlPpp.Tests.TestTypes
> --import Database.HsSqlPpp.Types
> --import Database.HsSqlPpp.Catalog
> --import Database.HsSqlPpp.TypeChecker
> --import Database.HsSqlPpp.Types

> --import Database.HsSqlPpp.Tests.TypeChecking.Utils

> queryExprs :: Item
> queryExprs =
>   Group "queryExprs" []


= basic select list

column name
*
aliases
correlation names

can embed in view to check additional stuff like no duplicate or
unnamed columns

= from

tableref
funtableref
subquery tref
join
parens
aliases
lateral

= set valued functions in select list

= aggregates

== simple
== with extra exprs
== in other contexts
== grouping set stuff
== having
= windows

= orderby
= where
= csq
= distinct
= set ops

= limit, offset


= additional scalar expr syntax tests

where, having, order by, limit, offset



= errors

what kind of errors can there be with a sql statement?

static errors: these are errors which can be detected with syntax
analysis and static checking without executing anything (and before
any cost based optimiser)

transaction errors: these are errors which occur because of
transactions and concurrency. Some other transaction can commit first
and then your transaction fails and cannot work (e.g. someone drops a
table you are referencing), and you can have serializable errors which
you can just retry and expect it to work.

'dynamic' errors: bad parses, arithmetic overflow, etc. These are
errors which only occur during statement execution. Are they all
related to data type conversion and/or scalar functions (maybe also
aggs and windows?). can other phys ops fail like this?

apparent hanging? is this an error? this means stuff like the cost
based optimiser takes forever (or even some other part of the
statement compiler), or the runtime takes forever (e.g. some
degenerate sort means it does a new io for each row and it never
completes). Real hangs should only occur from programmer errors. We
should have methods to detect and recover from these (cancelling a
statement or connection, and restarting the server process for
instance).

unexpected errors: these can come from programming errors, e.g. assert
or bad memory writes, or running out of memory (this is a programmer
error), or from hardware issues
