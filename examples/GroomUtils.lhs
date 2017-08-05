Utilities to show asts more nicely

groomAAnns: show anns as A
groomTypes: show anns as Type|TypeErrors


> module GroomUtils
>     (groomNoAnns
>     --,groomAAnns
>     ,groomTypes) where
>
> --import qualified Text.Groom as G
> import Language.Haskell.Exts
> import Data.Generics.Uniplate.Data

> --groomAAnns = undefined

> groomNoAnns :: Show a => a -> String
> groomNoAnns = groomF $ const $ Con noSrcSpan $ UnQual noSrcSpan $ Ident noSrcSpan "A"

> groomF :: Show a => (Exp SrcSpanInfo -> Exp SrcSpanInfo) -> a -> String
> groomF f s =
>   case parseExp (show s) of
>     ParseOk ast -> prettyPrint (g ast)
>     x -> error $ show x
>   where
>     g = transformBi $ \x ->
>                case x of
>                  RecConstr _ (UnQual _ (Ident _ "Annotation")) _ ->
>                           f x
>                  x' -> x'


> groomTypes :: Show a => a -> String
> groomTypes = groomF $ \x -> case x of
>   RecConstr _ (UnQual _ (Ident _ "Annotation"))
>    [FieldUpdate _ _ _,
>     FieldUpdate _ (UnQual _ (Ident _ "anType")) t,
>     FieldUpdate _ (UnQual _ (Ident _ "anErrs")) (List _ errs),
>     FieldUpdate _ _ _,
>     FieldUpdate _ _ _] -> case (t,errs) of
>                              (Con _ (UnQual _ (Ident _ "Nothing")),[]) ->
>                                  Con noSrcSpan (UnQual noSrcSpan (Ident noSrcSpan "A"))
>                              (y,[]) -> y
>                              (_,z) -> List noSrcSpan z
>   x' -> x'

