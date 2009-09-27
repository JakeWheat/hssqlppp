Copyright 2009 Jake Wheat

This module contains some utilities and generic code for working with
asts and annotations which depend on the ast types.

> module Database.HsSqlPpp.AstInternals.AnnotationUtils
>     (
>      getStatementAnnotations
>     ) where

> import Data.Generics

> import Database.HsSqlPpp.AstInternals.AstInternal
> import Database.HsSqlPpp.AstInternals.AstAnnotation


> getStatementAnnotations :: Data a => a -> [Annotation]
> getStatementAnnotations st =
>     everything (++) (mkQ [] ga) st
>     where
>       ga :: Statement -> [Annotation]
>       ga s = [getAnnotation s]
