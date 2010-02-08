Copyright 2010 Jake Wheat

Tests to check extending the 'foreign key' support using regular
postgresql syntax.

This system allow fking to a view, and allows the source or target
attributes to not be a key.

If a foreign key cannot be implemented with pg, then the check fn,
trigger fns, and triggers get added as with the other extended
constraints.


> {-# LANGUAGE ViewPatterns, QuasiQuotes, ScopedTypeVariables #-}
> module Database.HsSqlPpp.Examples.Extensions.ExtendedConstraintFKTests
>     (extendedConstraintFKExamples) where
>
> import Database.HsSqlPpp.Ast
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.PrettyPrinter
> import Database.HsSqlPpp.Examples.Extensions.ExtensionsUtils
> import Database.HsSqlPpp.SqlQuote
> import Database.HsSqlPpp.Examples.Extensions.ExtendedConstraints

> extendedConstraintFKExamples :: [ExtensionTest]
> extendedConstraintFKExamples = []

first check a regular fk isn't touched
check multiple field isn't touched

check non keys in source table

check non keys in target table

check to view
