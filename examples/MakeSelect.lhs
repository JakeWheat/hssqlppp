
> {-# LANGUAGE OverloadedStrings #-}
> import Database.HsSqlPpp.Annotation
> import Database.HsSqlPpp.Syntax
> import Database.HsSqlPpp.Pretty
> import qualified Data.Text as T
> import qualified Data.Text.Lazy as LT

> data MakeSelect = MakeSelect [T.Text] T.Text

> sqlGen :: MakeSelect -> QueryExpr
> sqlGen (MakeSelect cols tb) =
>   makeSelect
>   {selSelectList = SelectList emptyAnnotation
>                    $ map si cols
>   ,selTref = [Tref emptyAnnotation
>               (Name emptyAnnotation [Nmc $ T.unpack tb])]}
>   where
>     si i = SelExp emptyAnnotation
>              (Identifier emptyAnnotation
>              (Name emptyAnnotation [Nmc $ T.unpack i]))

> main :: IO ()
> main = do
>   let s = MakeSelect ["a", "b"] "t"
>   putStrLn $ LT.unpack $ prettyQueryExpr defaultPrettyFlags $ sqlGen s

