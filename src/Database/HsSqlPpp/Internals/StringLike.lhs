

> {-# LANGUAGE TypeSynonymInstances,FlexibleInstances #-}
> module Database.HsSqlPpp.Internals.StringLike where

> import qualified Data.Text as T
> import qualified Data.Text.Lazy as TL
> import qualified Data.ByteString.Char8 as B
> import qualified Data.ByteString.Lazy.Char8 as BL
> import Data.String

> class (IsString a, Eq a) => StringLike a where
>     pack :: String -> a
>     unpack :: a -> String

> instance StringLike String where
>     pack = id
>     unpack = id
> instance StringLike T.Text where
>     pack = T.pack
>     unpack = T.unpack
> instance StringLike TL.Text where
>     pack = TL.pack
>     unpack = TL.unpack
> instance StringLike B.ByteString where
>     pack = B.pack
>     unpack = B.unpack
> instance StringLike BL.ByteString where
>     pack = BL.pack
>     unpack = BL.unpack
