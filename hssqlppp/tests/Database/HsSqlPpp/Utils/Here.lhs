Simple support for multiline strings

> {-# LANGUAGE TemplateHaskell #-}
> module Database.HsSqlPpp.Utils.Here where
> import Language.Haskell.TH.Quote
> import Language.Haskell.TH.Lib

> here :: QuasiQuoter
> here = QuasiQuoter {quoteExp = litE . stringL
>                    ,quotePat = litP . stringL
>                    ,quoteType = error "no quoteType for here"
>                    ,quoteDec = error "no quoteDec for here"}
