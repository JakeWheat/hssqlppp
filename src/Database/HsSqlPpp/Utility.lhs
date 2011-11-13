
> -- | This module contains a collection of utility functions

> module Database.HsSqlPpp.Utility
>     (-- * ast utils
>      resetAnnotations
>     ,queryType
>      -- * typechecked ast utils
>     ,addExplicitCasts
>     ,canonicalizeTypeName
>     ,canonicalizeTypeNames
>     ,tcTreeInfo
>     ,emacsShowErrors
>      -- * lexing utils
>      -- The main use is to help diagnose issues where hssqlppp
>      -- fails to parse something correctly
>     ,lexSql
>     ,Token
>     ,Tok(..)
>     ) where

> import Data.Generics.Uniplate.Data
> import Data.Data
> import Data.List

> import Database.HsSqlPpp.Internals.AstInternal
> import Database.HsSqlPpp.Internals.TypesInternal
> import Database.HsSqlPpp.Parsing.Lexer
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.Parser

> -- | replace all the annotations in a tree with 'emptyAnnotation'
> resetAnnotations :: Data a => a -> a
> resetAnnotations = transformBi (const emptyAnnotation)

> -- | Gets the type of the sql source passed in. Expects the string to contain
> -- a query expr
> queryType :: Catalog -> String -> Maybe Type
> queryType cat src = do
>   ast <- either (const Nothing) Just $ parseQueryExpr defaultParseFlags "" Nothing src
>   anType $ getAnnotation $ typeCheckQueryExpr defaultTypeCheckingFlags cat ast

> -- | Gets some information useful for checking a typechecked tree
> -- returns the type of the top level node, a list of type errors from
> -- the tree, a list of queryexpr nodes which don't have a type set, and a
> -- list of scalarexpr nodes which don't have a type set.
> tcTreeInfo :: Data a =>
>               a
>            -> (Maybe Type,[([TypeError],Maybe SourcePosition)]
>               ,[QueryExpr],[ScalarExpr])
> tcTreeInfo ast =
>   let noTypeSEs :: [ScalarExpr]
>       noTypeSEs = [x | x <- universeBi ast
>                      , anType (getAnnotation x) == Nothing]
>       noTypeQEs :: [QueryExpr]
>       noTypeQEs = [x | x <- universeBi ast
>                      , anType (getAnnotation x) == Nothing]
>       -- get the list of type errors with source positions
>       -- from the typechecked ast
>       tes :: [([TypeError],Maybe SourcePosition)]
>       tes = [(e,sp) | a@(Annotation {}) <- universeBi ast
>                     , let e = anErrs a
>                     , let sp = anSrc a
>                     , not (null e)]
>       ty =  anType $ getAnnotation ast
>   in (ty,tes,noTypeQEs,noTypeSEs)

> -- | show a type error list in emacs format
> emacsShowErrors :: [([TypeError],Maybe SourcePosition)] -> String
> emacsShowErrors tes =
>   intercalate "\n" $ map se tes
>   where
>     se (es,sp) =
>       (case sp of
>          Nothing -> "unknown source"
>          Just (fn,l,c) -> fn ++ ":" ++ show l ++ ":" ++ show c ++ ":")
>       ++ " " ++ intercalate "\n" (map show es)
