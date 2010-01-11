
Multiline string support, mainly used to enter multiline sql strings
without loads of \ crap

robbed from http://groups.google.com/group/fa.haskell/msg/6a63dc4540f0486d

> {-# OPTIONS_HADDOCK hide #-}

> module Database.HsSqlPpp.Here where

> import Language.Haskell.TH.Quote
> --import Language.Haskell.TH.Syntax
> import Language.Haskell.TH.Lib

> here :: QuasiQuoter
> here = QuasiQuoter (litE . stringL) (litP . stringL)
