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
> import Data.Maybe

> --import Database.HsSqlPpp.Utils.Utils

> makeAntiNodes :: IO String
> makeAntiNodes = do
>   ast <- pf "Database/HsSqlPpp/AstInternals/AstInternal.hs"
>   --ast1 <- pf "Database/HsSqlPpp/AstInternals/AstAnti.hs"
>   --trace (ppExpr ast1) $ return ()
>   let tyNs = getExportedTypeNames ast
>       ds = getDeclarations ast tyNs
>       tm = map (\d -> (extractName d, d)) ds
>       lts = getListTypes ds
>       convs = concatMap (\n -> [makeTypeSig n, makeConvertor tm lts n]) tyNs
>   return $ prettyPrint $ makeModule (ds ++ convs)  -- ppExpr tyNs
>   where
>     extractName (DataDecl _ _ _ (Ident n) _ _ _) = n
>     extractName (TypeDecl _ (Ident n) _ _) = n
>     extractName a = error $ "extractName" ++ show a

     (SrcLoc{srcFilename =
               "Database/HsSqlPpp/AstInternals/AstInternal.hs",
             srcLine = 6854, srcColumn = 1})
     DataType
     []
     (Ident "ParamDef")

TypeDecl
     (SrcLoc{srcFilename =
               "Database/HsSqlPpp/AstInternals/AstInternal.hs",
             srcLine = 7808, srcColumn = 1})
     (Ident "RowConstraintList")
     []
     (TyList (TyParen (TyCon (UnQual (Ident "RowConstraint"))))),


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

> getCtors :: Decl -> [(String,[String])]
> getCtors t =
>   case t of
>        DataDecl _ _ _ _ _ ctors _ -> map ctorInfo ctors
>        _ -> []
>   where
>     ctorInfo (QualConDecl _ _ _ (ConDecl (Ident n) as)) = (n, map aInfo as)
>     ctorInfo a = error $ "ctorInfo " ++ show a
>     aInfo (UnBangedTy (TyParen (TyCon (UnQual (Ident m))))) = m
>     aInfo a = error $ "aInfo " ++ show a

[](ConDecl (Ident "ParamDef")
           [UnBangedTy (TyParen (TyCon (UnQual (Ident "Annotation")))),
            UnBangedTy (TyParen (TyCon (UnQual (Ident "String")))),
            UnBangedTy (TyParen (TyCon (UnQual (Ident "TypeName"))))]),
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


FunBind
     [Match
        (SrcLoc{srcFilename = "Database/HsSqlPpp/AstInternals/AstAnti.hs",
                srcLine = 8, srcColumn = 1})
        (Ident "cascade")
        [PVar (Ident "a")]
        Nothing
        (UnGuardedRhs
           (Case (Var (UnQual (Ident "a")))
              [Alt
                 (SrcLoc{srcFilename = "Database/HsSqlPpp/AstInternals/AstAnti.hs",
                         srcLine = 9, srcColumn = 15})
                 (PApp (UnQual (Ident "Cascade")) [])
                 (UnGuardedAlt (Var (UnQual (Ident "undefined"))))
                 (BDecls []),
               Alt
                 (SrcLoc{srcFilename = "Database/HsSqlPpp/AstInternals/AstAnti.hs",
                         srcLine = 10, srcColumn = 15})
                 (PApp (UnQual (Ident "Restrict")) [])
                 (UnGuardedAlt (Var (UnQual (Ident "undefined"))))
                 (BDecls [])]))
        (BDecls [])],

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


FunBind
     [Match
        (SrcLoc{srcFilename = "Database/HsSqlPpp/AstInternals/AstAnti.hs",
                srcLine = 8, srcColumn = 1})
        (Ident "paramDef")
        [PVar (Ident "x")]
        Nothing
        (UnGuardedRhs
           (Case (Var (UnQual (Ident "x")))
              [Alt
                 (SrcLoc{srcFilename = "Database/HsSqlPpp/AstInternals/AstAnti.hs",
                         srcLine = 9, srcColumn = 16})
                 (PApp (UnQual (Ident "ParamDef"))
                    [PVar (Ident "a"), PVar (Ident "b"), PVar (Ident "c")])
                 (UnGuardedAlt (Var (UnQual (Ident "undefined"))))
                 (BDecls []),
               Alt
                 (SrcLoc{srcFilename = "Database/HsSqlPpp/AstInternals/AstAnti.hs",
                         srcLine = 10, srcColumn = 16})
                 (PApp (UnQual (Ident "ParamDefTp"))
                    [PVar (Ident "a"), PVar (Ident "b")])
                 (UnGuardedAlt (Var (UnQual (Ident "undefined"))))
                 (BDecls [])]))
        (BDecls [])],










     (SrcLoc{srcFilename =
               "Database/HsSqlPpp/AstInternals/AstInternal.hs",
             srcLine = 4622, srcColumn = 1})
     (Ident "ExpressionListStatementListPair")
     []
     (TyTuple Boxed
        [TyParen (TyCon (UnQual (Ident "ExpressionList"))),
         TyParen (TyCon (UnQual (Ident "StatementList")))]),






> makeTypeSig :: String -> Decl
> makeTypeSig s =
>   TypeSig nsrc
>           [Ident $ lowerFirst s]
>           (TyFun (TyCon (UnQual (Ident s)))
>            (TyCon (Qual (ModuleName "A") (Ident s))))

> makeConvertor :: [(String,Decl)] -> [(String,String)] -> String -> Decl
> makeConvertor tns listTypes t =
>     case lookup t listTypes of
>       Just t1 -> makeLc t t1
>       Nothing -> let d = fromJust $ lookup t tns
>                  in case (isPair d, isMaybe d) of
>                       (Just (p1,p2), _) -> makePair ts t p1 p2
>                       (_, Just t1) -> makeMaybe ts t t1 d
>                       _ -> makeCase (t,d)
>     where
>       makeLc tb t1 =
>           if t1 `elem` (map fst tns)
>           then listConvertor (lowerFirst tb) (lowerFirst t1)
>           else listID (lowerFirst tb)
>       ts = map fst tns

> isPair :: Decl -> Maybe (String,String)
> isPair (TypeDecl _ _ _
>         (TyTuple Boxed
>          [TyParen (TyCon (UnQual (Ident a))),
>           TyParen (TyCon (UnQual (Ident b)))])) = Just (a,b)
> isPair _ = Nothing

> makePair :: [String] -> String -> String -> String -> Decl
> makePair ts t p1 p2 =
>    FunBind
>      [Match nsrc
>         (Ident $ lowerFirst t)
>         [PTuple [PVar (Ident "a"), PVar (Ident "b")]]
>         Nothing
>         (UnGuardedRhs
>            (Tuple
>               [conv p1 "a"
>               ,conv p2 "b"]))
>         (BDecls [])]
>    where
>      conv tn l = if tn `elem` ts
>                  then App (Var (UnQual (Ident $ lowerFirst tn)))
>                           (Var (UnQual (Ident l)))
>                  else Var (UnQual (Ident l))



     (SrcLoc{srcFilename =
               "Database/HsSqlPpp/AstInternals/AstInternal.hs",
             srcLine = 4622, srcColumn = 1})
     (Ident "ExpressionListStatementListPair")
     []
     (TyTuple Boxed
        [TyParen (TyCon (UnQual (Ident "ExpressionList"))),
         TyParen (TyCon (UnQual (Ident "StatementList")))]),

> isMaybe :: Decl -> Maybe String
> isMaybe (TypeDecl _ _ _
>          (TyParen
>           (TyApp (TyCon (UnQual (Ident "Maybe")))
>            (TyParen (TyCon (UnQual (Ident t1))))))) = Just t1
> isMaybe _ = Nothing

> makeMaybe :: [String] -> String -> String -> Decl -> Decl
> makeMaybe ts t t1 d =
>   PatBind nsrc
>      (PVar (Ident $ lowerFirst t))
>      Nothing
>      (UnGuardedRhs
>         (App (Var (UnQual (Ident "fmap")))
>            (Var (UnQual (Ident $ lowerFirst t1)))))
>      (BDecls [])


)= undefined

   TypeDecl
     (SrcLoc{srcFilename =
               "Database/HsSqlPpp/AstInternals/AstInternal.hs",
             srcLine = 6400, srcColumn = 1})
     (Ident "MaybeExpression")
     []
     (TyParen
        (TyApp (TyCon (UnQual (Ident "Maybe")))
           (TyParen (TyCon (UnQual (Ident "Expression")))))),


> makeUndefined :: String -> Decl
> makeUndefined s =
>   PatBind nsrc
>           (PVar (Ident $ lowerFirst s))
>           Nothing
>           (UnGuardedRhs (Var (UnQual (Ident "undefined"))))
>           (BDecls [])

> makeCase :: (String,Decl) -> Decl
> makeCase (f, d) =
>   let is = getCtors d -- [("A", ["A1","A2"])]
>   in FunBind
>      [Match nsrc
>         (Ident $ lowerFirst f)
>         [PVar (Ident "x")]
>         Nothing
>         (UnGuardedRhs
>            (Case (Var (UnQual (Ident "x")))
>               (map (uncurry mkAlt) is)))
>         (BDecls [])]
>   where
>     mkAlt c ts = let anames = map (("a"++) . show . snd) $ zip ts [(1::Int)..]
>                  in Alt nsrc
>                  (PApp (UnQual (Ident c))
>                     (map (PVar . Ident) anames))
>                  (UnGuardedAlt (Var (UnQual (Ident "undefined"))))
>                  (BDecls [])


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
>   [(n,n1) | (TypeDecl _ (Ident n) _
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


> {-a =
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
>         (BDecls [])]-}


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