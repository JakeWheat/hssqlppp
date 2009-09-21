Copyright 2009 Jake Wheat

This is the public module for the type checking functionality.

> {-# OPTIONS -fglasgow-exts #-}
> {-# LANGUAGE ScopedTypeVariables #-}

> {- | Contains the data types and functions for annotating
>  an ast and working with annotated trees, including the
>  representations of SQL data types.
>
> Annotations:
>
> * are attached to some of the ast node data types, but not all of them (yet?);
>
> * types annotations are attached to most nodes;
>
> * type errors are attached to the lowest down node that the type error is detected at;
>
> * nodes who fail the type check or whose type depends on a node with a type error are
>   given the type 'TypeCheckFailed';
>
> * each statement has an additional 'StatementInfo' annotation attached to it;
>
> * the parser fills in the source position nodes, but doesn't do a great job yet.
>
> -}
> module Database.HsSqlPpp.TypeChecking.TypeChecker
>     (
>      -- * Annotation type
>      Annotation
>     ,AnnotationElement(..)
>      -- * SQL types
>     ,Type (..)
>     ,PseudoType (..)
>      -- * Type errors
>     ,TypeError (..)
>      -- * Statement info
>      -- | This is the main annotation attached to each statement. Early days at the moment
>      -- but will be expanded to provide any type errors lurking inside a statement, any useful
>      -- types, e.g. the types of each select and subselect/sub query in a statement,
>      -- any changes to the catalog the statement makes, and possibly much more information.
>     ,StatementInfo(..)
>      -- * Annotation functions
>     ,annotateAst
>     ,annotateAstEnv
>     ,annotateExpression
>      -- * Annotated tree utils
>     ,getTopLevelTypes
>     ,getTopLevelInfos
>     ,getTopLevelEnvUpdates
>     ,getTypeErrors
>     ,stripAnnotations
>     ) where

> import Data.Generics

> import Database.HsSqlPpp.TypeChecking.AstInternal
> import Database.HsSqlPpp.TypeChecking.TypeType
> import Database.HsSqlPpp.TypeChecking.AstAnnotation


> getAnnotationsRecurse :: (Data a,Annotated a) => a -> [AnnotationElement]
> getAnnotationsRecurse st = listify (\(_::AnnotationElement) -> True) st


hack job, often not interested in the source positions when testing
the asts produced, so this function will reset all the source
positions to empty ("", 0, 0) so we can compare them for equality, etc.
without having to get the positions correct.

> -- | strip all the annotations from a tree. E.g. can be used to compare
> -- two asts are the same, ignoring any source position annotation differences.
> stripAnnotations :: (Data a,Annotated a) => a -> a
> stripAnnotations = everywhere (mkT stripAn)
>                    where
>                      stripAn :: [AnnotationElement] -> [AnnotationElement]
>                      stripAn _ = []


> -- | runs through the ast given and returns a list of all the type errors
> -- in the ast. Recurses into all ast nodes to find type errors.
> -- This is the function to use to see if an ast has passed the type checking process.
> -- Source position information will be added to the return type at some point
> getTypeErrors :: (Data a,Annotated a) => a -> [TypeError]
> getTypeErrors sts =
>     gte $ getAnnotationsRecurse sts
>     where
>       gte (a:as) = case a of
>                     TypeErrorA e -> e:gte as
>                     _ -> gte as
>       gte _ = []

