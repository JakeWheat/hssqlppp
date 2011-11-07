

> module Database.HsSqlPpp.Utils.GroomNoAnns where

> import Language.Haskell.Exts
> import Data.Generics.Uniplate.Data

> groomNoAnns :: Show a => a -> String
> groomNoAnns = p stripA
>   where
>     stripA :: Exp -> Exp
>     stripA = transformBi $ \x ->
>                case x of
>                  (Paren
>                   (App
>                    (App
>                     (App
>                      (App
>                       (App
>                        (App (Con (UnQual (Ident "Annotation")))
>                         _) _) _) _) _) _))
>                       ->
>                           Con $ UnQual $ Ident "A"
>                  x1 -> x1
>     p f s =
>         case parseExp (show s) of
>           ParseOk ast -> prettyPrint (f ast)
>           x -> error $ show x

> groomF :: Show a => (Exp -> Exp) -> a -> String
> groomF g = p stripA
>   where
>     stripA :: Exp -> Exp
>     stripA = transformBi $ \x ->
>                case x of
>                  (Paren
>                   (App
>                    (App
>                     (App
>                      (App
>                       (App
>                        (App (Con (UnQual (Ident "Annotation")))
>                         _) _) _) _) _) _))
>                       -> g x
>                  x1 -> x1
>     p f s =
>         case parseExp (show s) of
>           ParseOk ast -> prettyPrint (f ast)
>           x -> error $ show x
