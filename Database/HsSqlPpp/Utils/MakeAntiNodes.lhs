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
> import Data.Char

> --import Database.HsSqlPpp.Utils.Utils

> makeAntiNodes :: IO String
> makeAntiNodes = do
>   ast <- pf "Database/HsSqlPpp/AstInternals/AstInternal.hs"
>   --ast1 <- pf "Database/HsSqlPpp/AstInternals/AstAnti.hs"
>   --trace (ppExpr ast1) $ return ()
>   let tyNs = getExportedTypeNames ast
>   let ds = getDeclarations ast tyNs
>   let lts = getListTypes ds
>   let convs = concatMap (\n -> [makeTypeSig n, makeConvertor tyNs lts n]) tyNs
>   return $ prettyPrint $ makeModule (ds ++ convs)  -- ppExpr tyNs

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
>                     importAs = Nothing, importSpecs = Nothing},
>          ImportDecl{importLoc =
>                     nsrc,
>                     importModule = ModuleName "Database.HsSqlPpp.AstInternals.AstInternal",
>                     importQualified = True, importSrc = False, importPkg = Nothing,
>                     importAs = Just (ModuleName "A"), importSpecs = Nothing}]
>         ds



> makeTypeSig :: String -> Decl
> makeTypeSig s =
>   TypeSig nsrc
>           [Ident $ lowerFirst s]
>           (TyFun (TyCon (UnQual (Ident s)))
>            (TyCon (Qual (ModuleName "A") (Ident s))))

> makeConvertor :: [String] -> [(String,String)] -> String -> Decl
> makeConvertor tns listTypes t =
>     case lookup t listTypes of
>       Just t1 -> makeLc t t1
>       Nothing -> makeUndefined t
>     where
>       makeLc t t1 =
>           if t1 `elem` tns
>           then listConvertor (lowerFirst t) (lowerFirst t1)
>           else listID (lowerFirst t)

> makeUndefined :: String -> Decl
> makeUndefined s =
>   PatBind nsrc
>           (PVar (Ident $ lowerFirst s))
>           Nothing
>           (UnGuardedRhs (Var (UnQual (Ident "undefined"))))
>           (BDecls [])

> listConvertor :: String -> String -> Decl
> listConvertor fn subfn =
>  PatBind nsrc
>      (PVar (Ident fn))
>      Nothing
>      (UnGuardedRhs
>         (App (Var (UnQual (Ident "map")))
>            (Var (UnQual (Ident subfn)))))
>      (BDecls [])

> listID :: String -> Decl
> listID fn = PatBind nsrc
>      (PVar (Ident fn))
>      Nothing
>      (UnGuardedRhs (Var (UnQual (Ident "id"))))
>      (BDecls [])


> getListTypes :: [Decl] -> [(String,String)] --[Decl]
> getListTypes ds =
>   [(n,n1) | d@(TypeDecl _ (Ident n) _
>           (TyList (TyParen (TyCon (UnQual (Ident n1)))))) <- universeBi ds]

   TypeDecl
     (SrcLoc{srcFilename =
               "Database/HsSqlPpp/AstInternals/AstInternal.hs",
             srcLine = 1070, srcColumn = 1})
     (Ident "CaseExpressionList")
     []
     (TyList (TyParen (TyCon (UnQual (Ident "Expression"))))),


> lowerFirst :: String -> String
> lowerFirst s = toLower (head s):tail s



  [TypeSig
     (SrcLoc{srcFilename = "Database/HsSqlPpp/AstInternals/AstAnti.hs",
             srcLine = 8, srcColumn = 1})
     [Ident "alterTableAction"]
     (TyFun (TyCon (UnQual (Ident "AlterTableAction")))
        (TyCon (Qual (ModuleName "A") (Ident "AlterTableAction")))),
   PatBind
     (SrcLoc{srcFilename = "Database/HsSqlPpp/AstInternals/AstAnti.hs",
             srcLine = 9, srcColumn = 1})
     (PVar (Ident "alterTableAction"))
     Nothing
     (UnGuardedRhs (Var (UnQual (Ident "undefined"))))
     (BDecls []),
   TypeSig
     (SrcLoc{srcFilename = "Database/HsSqlPpp/AstInternals/AstAnti.hs",
             srcLine = 11, srcColumn = 1})
     [Ident "copySource"]
     (TyFun (TyCon (UnQual (Ident "CopySource")))
        (TyCon (Qual (ModuleName "A") (Ident "CopySource")))),
   FunBind
     [Match
        (SrcLoc{srcFilename = "Database/HsSqlPpp/AstInternals/AstAnti.hs",
                srcLine = 12, srcColumn = 1})
        (Ident "copySource")
        [PVar (Ident "c")]
        Nothing
        (UnGuardedRhs
           (Case (Var (UnQual (Ident "c")))
              [Alt
                 (SrcLoc{srcFilename = "Database/HsSqlPpp/AstInternals/AstAnti.hs",
                         srcLine = 13, srcColumn = 18})
                 (PApp (UnQual (Ident "CopyFilename")) [PVar (Ident "s")])
                 (UnGuardedAlt (Var (UnQual (Ident "undefined"))))
                 (BDecls []),
               Alt
                 (SrcLoc{srcFilename = "Database/HsSqlPpp/AstInternals/AstAnti.hs",
                         srcLine = 14, srcColumn = 18})
                 (PApp (UnQual (Ident "Stdin")) [])
                 (UnGuardedAlt (Con (Qual (ModuleName "A") (Ident "Stdin"))))
                 (BDecls [])]))
        (BDecls [])],


> a =
>    TypeSig
>      nsrc
>      [Ident "cascade"]
>      (TyFun (TyCon (UnQual (Ident "Cascade")))
>         (TyCon (Qual (ModuleName "A") (Ident "Cascade"))))
>
> b =
>    FunBind
>      [Match
>         nsrc
>         (Ident "cascade")
>         [PVar (Ident "c")]
>         Nothing
>         (UnGuardedRhs
>            (Case (Var (UnQual (Ident "c")))
>               [Alt nsrc
>                  (PApp (UnQual (Ident "Cascade")) [])
>                  (UnGuardedAlt (Con (Qual (ModuleName "A") (Ident "Cascade"))))
>                  (BDecls []),
>                Alt
>                  nsrc
>                  (PApp (UnQual (Ident "Restrict")) [])
>                  (UnGuardedAlt (Con (Qual (ModuleName "A") (Ident "Restrict"))))
>                  (BDecls [])]))
>         (BDecls [])]


   DataDecl
     (SrcLoc{srcFilename =
               "Database/HsSqlPpp/AstInternals/AstInternal.hs",
             srcLine = 972, srcColumn = 1})
     DataType
     []
     (Ident "Cascade")
     []
     [QualConDecl
        (SrcLoc{srcFilename =
                  "Database/HsSqlPpp/AstInternals/AstInternal.hs",
                srcLine = 972, srcColumn = 17})
        []
        []
        (ConDecl (Ident "Cascade") []),
      QualConDecl
        (SrcLoc{srcFilename =
                  "Database/HsSqlPpp/AstInternals/AstInternal.hs",
                srcLine = 973, srcColumn = 17})
        []
        []
        (ConDecl (Ident "Restrict") [])]
     [(UnQual (Ident "Data"), []), (UnQual (Ident "Eq"), []),
      (UnQual (Ident "Show"), []), (UnQual (Ident "Typeable"), [])],


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