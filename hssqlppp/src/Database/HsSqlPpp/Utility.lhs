
> -- | This module contains a collection of utility functions

> {-# LANGUAGE FlexibleContexts #-}
> module Database.HsSqlPpp.Utility
>     (-- * ast utils
>      resetAnnotations
>     ,queryType
>     --,canonicalizeTypeName
>     --,canonicalizeTypeNames
>      -- * typechecked ast utils
>     ,addExplicitCasts
>     ,addImplicitCasts
>     ,tcTreeInfo
>     ,emacsShowErrors
>     ) where

> import Data.Generics.Uniplate.Data
> import Data.Data
> import Data.List
> import Data.Maybe

> import Database.HsSqlPpp.Internals.AstInternal
> import Database.HsSqlPpp.Internals.TypesInternal
> import Database.HsSqlPpp.Catalog
> import Database.HsSqlPpp.Parse
> --import Text.Parsec.Prim
> --import Control.Monad.Identity
> import qualified Data.Text.Lazy as L

> -- | replace all the annotations in a tree with 'emptyAnnotation'
> resetAnnotations :: Data a => a -> a
> resetAnnotations = transformBi (const emptyAnnotation)

> -- | Gets the type of the sql source passed in. Expects the string to contain
> -- a query expr
> queryType :: Catalog -> L.Text -> Maybe Type
> queryType cat src = do
>   ast <- either (const Nothing) Just $ parseQueryExpr defaultParseFlags "" Nothing src
>   fmap teType $ anType $ getAnnotation $ typeCheckQueryExpr defaultTypeCheckFlags cat ast

> -- | Gets some information useful for checking a typechecked tree
> -- returns the type of the top level node, a list of type errors from
> -- the tree, a list of the queryexpr nodes, and a list of the scalar
> -- exprs respectively, which have their type as nothing which indicates
> -- that the typechecking didn't complete successfully
> tcTreeInfo :: Data a =>
>               a
>            -> (Maybe TypeExtra,[([TypeError],Maybe SourcePosition)]
>               ,[QueryExpr],[ScalarExpr])
> tcTreeInfo ast =
>   let noTypeSEs :: [ScalarExpr]
>       noTypeSEs = [x | x <- universeBi ast
>                      , isNothing (anType (getAnnotation x))]
>       noTypeQEs :: [QueryExpr]
>       noTypeQEs = [x | x <- universeBi ast
>                      , isNothing (anType (getAnnotation x))]
>       -- get the list of type errors with source positions
>       -- from the typechecked ast
>       tes :: [([TypeError],Maybe SourcePosition)]
>       tes = [(e,sp) | a@(Annotation {}) <- universeBi ast
>                     , let e = anErrs a
>                     , let sp = anSrc a
>                     , not (null e)]
>       ty = anType $ getAnnotation ast
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
