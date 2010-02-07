Copyright 2010 Jake Wheat

The purpose of this code is to automatically generate a copy of the
ast data structures from AstInternal.ag, and produce a new set of data
types with some support for anti quotation nodes stuck in.

Then, generate a transform which takes the new nodes and converts them
to the original nodes, returning an error if an antinodes are in the
tree.

> module Database.HsSqlPpp.DevelTools.MakeAntiNodes
>     (makeAntiNodes) where
>
> import Language.Haskell.Exts hiding (String)
> import qualified Language.Haskell.Exts as Exts
> --import Data.Generics
> import Data.Generics.Uniplate.Data
> --import Debug.Trace
> --import Database.HsSqlPpp.Utils.Utils
> import Data.Char
> import Data.Maybe
> --import Database.HsSqlPpp.Utils.Utils
>
> nodesToAntificate :: [String]
> nodesToAntificate = ["Expression", "TriggerEvent", "Statement"]
>
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
>   return $ prettyPrint $ makeModule (exports ast) (wrappers ++ (addAntis ds) ++ (addAntis convs))
>   where
>     extractName (DataDecl _ _ _ (Ident n) _ _ _) = n
>     extractName (TypeDecl _ (Ident n) _ _) = n
>     extractName a = error $ "extractName" ++ show a
>     exports ast = [EVar (UnQual (Ident "convertStatements")),
>                    EVar (UnQual (Ident "convertExpression"))] ++
>                   [ex| ex@(EThingAll _) <- universeBi ast] ++
>                   [ex| ex@(EAbs _) <- universeBi ast]
>     addAntis = map antiize
>     antiTargFns = map lowerFirst nodesToAntificate
>     antiize d@(FunBind
>               [Match _ (Ident n) _ _ _ _]) |
>                 n `elem` antiTargFns =
>                     addAntiError d
>     antiize d@(DataDecl _ _ _ (Ident n) _ _ _) |
>                 n `elem` nodesToAntificate =
>                     addAntiCtor d
>     antiize x = x

> wrappers :: [Decl]
> wrappers =
>   [TypeSig
>      (SrcLoc{srcFilename = "Database/HsSqlPpp/AstInternals/AstAnti.hs",
>              srcLine = 31, srcColumn = 1})
>      [Ident "convertStatements"]
>      (TyFun (TyList (TyCon (UnQual (Ident "Statement"))))
>         (TyList (TyCon (Qual (ModuleName "A") (Ident "Statement"))))),
>    PatBind
>      (SrcLoc{srcFilename = "Database/HsSqlPpp/AstInternals/AstAnti.hs",
>              srcLine = 32, srcColumn = 1})
>      (PVar (Ident "convertStatements"))
>      Nothing
>      (UnGuardedRhs (Var (UnQual (Ident "statementList"))))
>      (BDecls []),
>    TypeSig
>      (SrcLoc{srcFilename = "Database/HsSqlPpp/AstInternals/AstAnti.hs",
>              srcLine = 34, srcColumn = 1})
>      [Ident "convertExpression"]
>      (TyFun (TyCon (UnQual (Ident "Expression")))
>         (TyCon (Qual (ModuleName "A") (Ident "Expression")))),
>    PatBind
>      (SrcLoc{srcFilename = "Database/HsSqlPpp/AstInternals/AstAnti.hs",
>              srcLine = 35, srcColumn = 1})
>      (PVar (Ident "convertExpression"))
>      Nothing
>      (UnGuardedRhs (Var (UnQual (Ident "expression"))))
>      (BDecls [])]

> addAntiCtor :: Decl -> Decl
> addAntiCtor (DataDecl sl dn ct nm@(Ident n) tyv qcd d) =
>   (DataDecl sl dn ct nm tyv (qcd ++ [antiCtor]) d)
>   where
>     antiCtor =
>       QualConDecl nsrc [] []
>         (ConDecl (Ident $ "Anti" ++ n)
>            [UnBangedTy (TyCon (UnQual (Ident "String")))])

> addAntiCtor e = error $ "addAntiCtor " ++ show e

> addAntiError :: Decl -> Decl
> addAntiError (FunBind
>               [Match sl nm@(Ident n) pt ty
>                (UnGuardedRhs
>                 (Case (Var (UnQual (Ident "x"))) alts))
>                   bnd]) =
>   (FunBind
>    [Match sl nm pt ty
>     (UnGuardedRhs
>      (Case (Var (UnQual (Ident "x"))) (alts ++ [antiAlt])))
>     bnd])
>   where
>     antiAlt :: Alt
>     antiAlt = Alt nsrc
>                  (PApp (UnQual (Ident $ "Anti" ++ upperFirst n)) [PVar (Ident "s")])
>                  (UnGuardedAlt
>                     (App (Var (UnQual (Ident "error")))
>                        (Lit (Exts.String $ "can't convert anti " ++ n))))
>                  (BDecls [])
> addAntiError e = error $ "addAntiError " ++ show e

> pf :: String -> IO Module
> pf f = do
>   x <- parseFile f
>   case x of
>         ParseOk ast -> return ast
>         e -> error $ show e

> makeModule :: [ExportSpec] -> [Decl] -> Module
> makeModule es ds =
>     Module nsrc
>         (ModuleName "Database.HsSqlPpp.AstInternals.AstAnti")
>         [LanguagePragma nsrc
>          [Ident "DeriveDataTypeable"]]
>         Nothing (Just es)
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
>     aInfo (UnBangedTy (TyParen (TyList (TyCon (UnQual (Ident m)))))) = m ++ "List"
>     aInfo (UnBangedTy (TyParen (TyApp (TyCon (UnQual (Ident "Maybe"))) (TyCon (UnQual (Ident m))))))
>           = "Maybe" ++ m
>     aInfo a = error $ "aInfo " ++ show a

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
>                       (_, Just t1) -> makeMaybe t t1
>                       _ -> makeCase ts (t,d)
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

> isMaybe :: Decl -> Maybe String
> isMaybe (TypeDecl _ _ _
>          (TyParen
>           (TyApp (TyCon (UnQual (Ident "Maybe")))
>            (TyParen (TyCon (UnQual (Ident t1))))))) = Just t1
> isMaybe _ = Nothing

> makeMaybe :: String -> String -> Decl
> makeMaybe t t1 =
>   PatBind nsrc
>      (PVar (Ident $ lowerFirst t))
>      Nothing
>      (UnGuardedRhs
>         (App (Var (UnQual (Ident "fmap")))
>            (Var (UnQual (Ident $ lowerFirst t1)))))
>      (BDecls [])

> {-makeUndefined :: String -> Decl
> makeUndefined s =
>   PatBind nsrc
>           (PVar (Ident $ lowerFirst s))
>           Nothing
>           (UnGuardedRhs (Var (UnQual (Ident "undefined"))))
>           (BDecls [])-}

> makeCase :: [String] -> (String,Decl) -> Decl
> makeCase ts (f, d) =
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
>     mkAlt c cis = let anames = map (("a"++) . show . snd) $ zip cis [(1::Int)..]
>                       ant = zip anames cis
>                   in Alt nsrc
>                   (PApp (UnQual (Ident c))
>                      (map (PVar . Ident) anames))
>                   (mkCtor c ant)
>                   (BDecls [])
>     mkCtor c ant =
>         let elems = flip map ant (\(a,t) -> conv t a)
>         in UnGuardedAlt $ foldl App (Con (Qual (ModuleName "A") (Ident c))) $ elems
>     conv tn l = if tn `elem` ts
>                  then (Paren (App (Var (UnQual (Ident $ lowerFirst tn)))
>                                       (Var (UnQual (Ident l)))))
>                  else Var (UnQual (Ident l))


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

> lowerFirst :: String -> String
> lowerFirst s = toLower (head s):tail s

> upperFirst :: String -> String
> upperFirst s = toUpper (head s):tail s


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
