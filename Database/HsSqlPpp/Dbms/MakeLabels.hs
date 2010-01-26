{-# LANGUAGE TemplateHaskell, FlexibleInstances, EmptyDataDecls #-}

-- robbed from the darcs version of hlist

-- Making labels

{-
 The following TH splice
      $(makeLabels ["getX","getY","draw"])

should expand into the following declarations

data GetX;     getX     = proxy::Proxy GetX
data GetY;     getY     = proxy::Proxy GetY
data Draw;     draw     = proxy::Proxy Draw

-}

module Database.HsSqlPpp.Dbms.MakeLabels (makeLabels,label) where

import Data.HList.FakePrelude

import Language.Haskell.TH.Ppr (pprint)
import Language.Haskell.TH.Syntax

import Data.Char (toUpper, toLower)
import Control.Monad (liftM)

capitalize, uncapitalize :: String -> String
capitalize   (c:rest) = toUpper c : rest
uncapitalize (c:rest) = toLower c : rest


-- Make the name of the type constructor whose string representation
-- is capitalized str
make_tname str = mkName $ capitalize str

-- Make the name of the value identifier whose string representation
-- is uncapitalized str
make_dname str = mkName $ uncapitalize str

-- The template of our declaration. We will then replace all occurences
-- of Foo with the desired name
dcl_template = [d| data Foo; foo = proxy::Proxy Foo |]

-- A very dirty traversal/replacement...

class ReplaceSyntax a where
    replace_name :: (Name,Name) -> (Name,Name) -> a -> a

instance ReplaceSyntax [Dec] where
    replace_name frm to dcls = map (replace_name frm to) dcls

instance ReplaceSyntax Dec where
    replace_name (tfrom,dfrom) (tto,dto)
                 dcl@(DataD ctx n parms con othern) =
                     if tfrom == n then
                        DataD ctx tto parms con othern
                        else dcl
    replace_name (tfrom,dfrom) (tto,dto)
                 dcl@(ValD (VarP n) (NormalB body) []) =
          let n' = if n == dfrom then dto else n
          in ValD (VarP n')
                  (NormalB (replace_name (tfrom,dfrom) (tto,dto) body)) []

    replace_name (tfrom,dfrom) (tto,dto) dcl =
        error $ "Can't handle: " ++ show dcl


instance ReplaceSyntax Exp where
    replace_name from to (SigE exp tp) =
                     SigE (replace_name from to exp)
                          (replace_name from to tp)
    replace_name from to exp = exp


instance ReplaceSyntax Type where
    replace_name (tfrom,dfrom) (tto,dto) tp@(ConT n) =
        if n == tfrom then (ConT tto) else tp
    replace_name from to (AppT t1 t2) =
        (AppT (replace_name from to t1) (replace_name from to t2))

-- Our main function
makeLabels :: [String] -> Q [Dec]
makeLabels = liftM concat . sequence . map repl
 where
 repl n = liftM (replace_name from (to n)) dcl_template
 from = (make_tname "foo",make_dname "foo")
 to n = (make_tname n,make_dname n)

label :: String -> Q [Dec]
label s = makeLabels [s]


-- Show the code expression
show_code cde = runQ cde >>= putStrLn . pprint

t1 = show_code [d| data Foo |]

t2 = showName $ mkName "Foo"

t3 = show_code $
     liftM (replace_name
            (make_tname "foo",make_dname "foo")
            (make_tname "bar",make_dname "bar")) dcl_template

t4 = show_code $ makeLabels ["getX","getY","draw"]
