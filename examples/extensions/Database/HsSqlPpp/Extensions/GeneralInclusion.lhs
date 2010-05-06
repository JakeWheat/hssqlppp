
Two aspects to general inclusions

1) want to pick up normal foreign key syntax, but where it references
a view, and pull this out to seperate create_foreign_key or whatever
it's going to be called

2) not going to try to do the reverse for now, so just need to convert
the create_fk call into a create_assertion call

> {-# LANGUAGE ViewPatterns, QuasiQuotes, ScopedTypeVariables #-}
> module Database.HsSqlPpp.Extensions.GeneralInclusion
>     (generalInclusionsExamples) where
>
> --import Database.HsSqlPpp.Ast
> --import Database.HsSqlPpp.Annotation
> --import Database.HsSqlPpp.PrettyPrinter
> import Database.HsSqlPpp.Extensions.ExtensionsUtils
> --import Database.HsSqlPpp.SqlQuote
> --import Database.HsSqlPpp.Examples.Extensions.CreateAssertion

> generalInclusionsExamples :: [ExtensionTest]
> generalInclusionsExamples = []
