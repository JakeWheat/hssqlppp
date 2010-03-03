
put this in separate module to stop circular dependencies:
the lexer, localbindings, need this

> {-# LANGUAGE DeriveDataTypeable #-}
> module Database.HsSqlPpp.AstInternals.Name where
> import Data.Generics
> data Name = Name String
>           | QName String Name
>           | Star
>           | BName String Name -- (p).x
>           | PosArg Int -- $1
>           | BPosArg Int Name -- ($1).x
>           | Symbol String
>             deriving (Show,Eq,Typeable,Data)

> simplePositionalArg :: Name -> Bool
> simplePositionalArg (PosArg _) = True
> simplePositionalArg _ = False
