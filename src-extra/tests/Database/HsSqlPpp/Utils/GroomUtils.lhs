
Utilities to show asts more nicely

groomAAnns: show anns as A
groomTypes: show anns as Type|TypeErrors


> module Database.HsSqlPpp.Utils.GroomUtils
>     (groomNoAnns
>     --,groomAAnns
>     ,groomTypes) where
>
> --import qualified Text.Groom as G
> import Language.Haskell.Exts
> import Data.Generics.Uniplate.Data

> --groomAAnns = undefined


> groomNoAnns :: Show a => a -> String
> groomNoAnns = groomF $ const $ Con $ UnQual $ Ident "A"

> groomF :: Show a => (Exp -> Exp) -> a -> String
> groomF f s =
>   case parseExp (show s) of
>     ParseOk ast -> prettyPrint (g ast)
>     x -> error $ show x
>   where
>     g = transformBi $ \x ->
>                case x of
>                  RecConstr (UnQual (Ident "Annotation")) _ ->
>                           f x
>                  x' -> x'


> groomTypes :: Show a => a -> String
> groomTypes = groomF $ \x -> case x of
>   RecConstr (UnQual (Ident "Annotation"))
>    [FieldUpdate _ _,
>     FieldUpdate (UnQual (Ident "atype")) t,
>     FieldUpdate (UnQual (Ident "errs")) (List errs),
>     FieldUpdate _ _,
>     FieldUpdate _ _] -> case (t,errs) of
>                              (Con (UnQual (Ident "Nothing")),[]) ->
>                                  Con (UnQual (Ident "A"))
>                              (y,[]) -> y
>                              (_,z) -> List z
>   x' -> x'




 (RecConstr (UnQual (Ident "Annotation"))
      [FieldUpdate (UnQual (Ident "asrc"))
         (Con (UnQual (Ident "Nothing"))),
       FieldUpdate (UnQual (Ident "atype"))
         (Con (UnQual (Ident "Nothing"))),
       FieldUpdate (UnQual (Ident "errs")) (List []),
       FieldUpdate (UnQual (Ident "implicitCast"))
         (Con (UnQual (Ident "Nothing"))),
       FieldUpdate (UnQual (Ident "catUpd")) (List [])])


>    {-
>   groomF tte d
>   where
>     tte :: Exp -> Exp
>     tte (Paren
>                   (App
>                    (App
>                     (App
>                      (App
>                       (App
>                        (App (Con (UnQual (Ident "Annotation")))
>                         _) t) te) _) _) _))
>          = case (t,te) of
>               (Con (UnQual (Ident "Nothing")) ,List []) ->
>                  Con (UnQual (Ident "A"))
>               (y,List []) -> y
>               (_,x) -> x
>            {-trace ("\n*************\n"
>                   ++ groom t
>                   ++ "\n*************\n"
>                   ++ groom te
>                   ++ "\n*************\n") $ Tuple [t,te]-}
>     tte x = x-}