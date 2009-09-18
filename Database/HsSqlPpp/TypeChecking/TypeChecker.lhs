Copyright 2009 Jake Wheat

This is the public module for the type checking functionality.

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
>      -- * Additional types
>      -- | Used in Scope and type checking.
>     ,DomainDefinition
>     ,FunctionPrototype
>     ,CastContext
>     ,CompositeDef
>     ,CompositeFlavour
>      -- * Annotation functions
>     ,annotateAst
>     ,annotateAstScope
>     ,annotateExpression
>      -- * Annotated tree utils
>     ,getTopLevelTypes
>     ,getTopLevelInfos
>     ,getTypeErrors
>     ,stripAnnotations
>     ) where

> import Database.HsSqlPpp.TypeChecking.AstInternal
> import Database.HsSqlPpp.TypeChecking.TypeType