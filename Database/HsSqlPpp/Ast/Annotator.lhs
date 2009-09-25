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
> module Database.HsSqlPpp.Ast.Annotator
>     (
>      -- * Annotation functions
>     annotateAst
>     ,annotateAstEnv
>     ,annotateExpression
>     ,annotateAstsEnv
>     ,annotateAstEnvEnv
>      -- * Annotated tree utils
>     ,getTopLevelTypes
>     ,getTopLevelInfos
>     ,getTopLevelEnvUpdates
>     ,getTypeErrors
>     ) where

> import Database.HsSqlPpp.AstInternals.AstInternal
> import Database.HsSqlPpp.AstInternals.AstAnnotation
