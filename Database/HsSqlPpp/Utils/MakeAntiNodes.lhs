Copyright 2010 Jake Wheat

The purpose of this code is to automatically generate a copy of the
ast data structures from AstInternal.ag, and produce a new set of data
types with some support for anti quotation nodes stuck in.

Then, generate a transform which takes the new nodes and converts them
to the original nodes, returning an error if an antinodes are in the
tree.

> module Database.HsSqlPpp.Utils.MakeAntiNodes
>     (makeAntiNodes) where
>
> import Language.Haskell.Exts hiding (String)
> --import qualified Language.Haskell.Exts as Exts
> --import Data.Generics
> import Data.Generics.Uniplate.Data
> import Debug.Trace
> import Database.HsSqlPpp.Utils.Utils

> --import Database.HsSqlPpp.Utils.Utils

> makeAntiNodes :: IO String
> makeAntiNodes = do
>   ast <- pf "Database/HsSqlPpp/AstInternals/AstInternal.hs"
>   --ast1 <- pf "Database/HsSqlPpp/AstInternals/AstAnti.hs"
>   --trace (ppExpr ast1) $ return ()
>   let tyNs = getExportedTypeNames ast
>   let ds = getDeclarations ast tyNs
>   return $ prettyPrint $ makeModule ds  -- ppExpr tyNs

> pf :: String -> IO Module
> pf f = do
>   x <- parseFile f
>   case x of
>         ParseOk ast -> return ast
>         e -> error $ show e


> makeModule :: [Decl] -> Module
> makeModule ds =
>     Module nsrc
>         (ModuleName "Database.HsSqlPpp.AstInternals.AstAnti")
>         [LanguagePragma nsrc
>          [Ident "DeriveDataTypeable"]]
>         Nothing Nothing
>         [ImportDecl{importLoc = nsrc,
>                     importModule = ModuleName "Data.Generics",
>                     importQualified = False,
>                     importSrc = False, importPkg = Nothing, importAs = Nothing,
>                     importSpecs = Nothing},
>          ImportDecl{importLoc = nsrc,
>                     importModule = ModuleName "Database.HsSqlPpp.AstInternals.AstAnnotation",
>                     importQualified = False, importSrc = False, importPkg = Nothing,
>                     importAs = Nothing, importSpecs = Nothing}]
>         ds


Module
  (SrcLoc{srcFilename = "Database/HsSqlPpp/AstInternals/AstAnti.hs",
          srcLine = 1, srcColumn = 1})
  (ModuleName "Database.HsSqlPpp.AstInternals.AstAnti")
  [LanguagePragma
     (SrcLoc{srcFilename = "Database/HsSqlPpp/AstInternals/AstAnti.hs",
             srcLine = 1, srcColumn = 1})
     [Ident "DeriveDataTypeable"]]
  Nothing
  Nothing
  [ImportDecl{importLoc =
                SrcLoc{srcFilename = "Database/HsSqlPpp/AstInternals/AstAnti.hs",
                       srcLine = 5, srcColumn = 1},
              importModule = ModuleName "Data.Generics", importQualified = False,
              importSrc = False, importPkg = Nothing, importAs = Nothing,
              importSpecs = Nothing},
   ImportDecl{importLoc =
                SrcLoc{srcFilename = "Database/HsSqlPpp/AstInternals/AstAnti.hs",
                       srcLine = 7, srcColumn = 1},
              importModule =
                ModuleName "Database.HsSqlPpp.AstInternals.AstAnnotation",
              importQualified = False, importSrc = False, importPkg = Nothing,
              importAs = Nothing, importSpecs = Nothing}]


> nsrc :: SrcLoc
> nsrc = SrcLoc "" 0 0

> getExportedTypeNames :: Module -> [String]
> getExportedTypeNames m =
>                        [n | EThingAll (UnQual ( Ident n)) <- universeBi m] ++
>                        [n | EAbs (UnQual (Ident n)) <- universeBi m]

> getDeclarations :: Module -> [String] -> [Decl]
> getDeclarations m nms =
>     [d | d@(DataDecl _ _ _ (Ident n) _ _ _) <- universeBi m
>          ,n `elem` nms]
>     ++
>     [d | d@(TypeDecl _ (Ident n) _ _) <- universeBi m
>          ,n `elem` nms]

TypeDecl SrcLoc Name [TyVarBind] Type
import Data.Maybe
import Data.List
import Debug.Trace
import Data.Either
import Control.Applicative
import Data.Generics
import Data.Char
import Control.Monad.State

import Database.HsSqlPpp.AstInternals.TypeType
import Database.HsSqlPpp.AstInternals.TypeChecking.TypeConversion
import Database.HsSqlPpp.AstInternals.TypeChecking.ErrorUtils
import Database.HsSqlPpp.AstInternals.AstAnnotation
import Database.HsSqlPpp.AstInternals.Catalog.CatalogInternal
import Database.HsSqlPpp.AstInternals.TypeChecking.LocalBindings
import Database.HsSqlPpp.Utils.Utils
import Data.Generics.PlateData


TypeDecl
     (SrcLoc{srcFilename =
               "Database/HsSqlPpp/AstInternals/AstInternal.hs",
             srcLine = 7808, srcColumn = 1})
     (Ident "RowConstraintList")
     []
     (TyList (TyParen (TyCon (UnQual (Ident "RowConstraint"))))),

   DataDecl
     (SrcLoc{srcFilename =
               "Database/HsSqlPpp/AstInternals/AstInternal.hs",
             srcLine = 6854, srcColumn = 1})
     DataType
     []
     (Ident "ParamDef")
     []
     [QualConDecl
        (SrcLoc{srcFilename =
                  "Database/HsSqlPpp/AstInternals/AstInternal.hs",
                srcLine = 6854, srcColumn = 18})
        []
        []
        (ConDecl (Ident "ParamDef")
           [UnBangedTy (TyParen (TyCon (UnQual (Ident "Annotation")))),
            UnBangedTy (TyParen (TyCon (UnQual (Ident "String")))),
            UnBangedTy (TyParen (TyCon (UnQual (Ident "TypeName"))))]),
      QualConDecl
        (SrcLoc{srcFilename =
                  "Database/HsSqlPpp/AstInternals/AstInternal.hs",
                srcLine = 6855, srcColumn = 18})
        []
        []
        (ConDecl (Ident "ParamDefTp")
           [UnBangedTy (TyParen (TyCon (UnQual (Ident "Annotation")))),
            UnBangedTy (TyParen (TyCon (UnQual (Ident "TypeName"))))])]
     [(UnQual (Ident "Data"), []), (UnQual (Ident "Eq"), []),
      (UnQual (Ident "Show"), []), (UnQual (Ident "Typeable"), [])],