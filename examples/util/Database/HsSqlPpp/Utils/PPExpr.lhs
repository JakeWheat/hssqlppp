> module Database.HsSqlPpp.Utils.PPExpr where

> import qualified Language.Haskell.Exts as Exts

> -- dodgy code to pretty print a value using haskell-src-exts to try
> -- and format it nicely
>
> ppExpr :: Show s => s -> String
> ppExpr s =
>   case Exts.parseExp (show s) of
>     Exts.ParseOk ast -> Exts.prettyPrint ast
>     x -> error $ show x

> ppExprT :: Show a => (Exts.Exp -> Exts.Exp) -> a -> String
> ppExprT t s =
>   case Exts.parseExp (show s) of
>     Exts.ParseOk ast -> Exts.prettyPrint $ t ast
>     x -> error $ show x
