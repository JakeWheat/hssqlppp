
Multiline string support, mainly used to enter multiline sql strings

robbed from http://old.nabble.com/Multi-line-string-literals-are-both-easy--and--elegant-in-Haskell-td19959973.html

> {-# OPTIONS_HADDOCK hide #-}
>
> module Database.HsSqlPpp.Utils.Here where
>
> import Language.Haskell.TH.Quote
> --import Language.Haskell.TH.Syntax
> import Language.Haskell.TH.Lib
>
> here :: QuasiQuoter
> here = QuasiQuoter (litE . stringL) (litP . stringL)
