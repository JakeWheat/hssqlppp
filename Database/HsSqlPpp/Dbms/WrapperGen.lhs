Copyright 2010 Jake Wheat

demo code for using the hssqlppp typechecker to generate typesafe
database access code

could probably use some quasi quotation

> {-# LANGUAGE FlexibleContexts #-}

> {-# OPTIONS_HADDOCK hide #-}

> module Database.HsSqlPpp.Dbms.WrapperGen
>     (wrapperGen) where

> import Text.Show.Pretty (ppShow)
> import Language.Haskell.Exts hiding (String)
> import qualified Language.Haskell.Exts as Exts
> import Data.Generics.PlateData
> import Data.Generics hiding (Prefix,Infix)
> import Control.Monad.Error
> import Data.Maybe

> import Database.HsSqlPpp.Ast.SqlTypes as Sql
> import Database.HsSqlPpp.Ast.Environment
> import Database.HsSqlPpp.Ast.TypeChecker
> import Database.HsSqlPpp.Parsing.Parser
> import Database.HsSqlPpp.Ast.Annotation

> wrapperGen :: String -> String -> IO String
> wrapperGen db fn = do
>   p <- parseFile fn
>   case p of
>     ParseOk ast -> do
>                    envU <- readEnvironmentFromDatabase db
>                    case updateEnvironment defaultEnvironment envU of
>                      Left er -> return $ show er
>                      Right env ->
>                          return $ {-ppShow ast ++  "\n\n" ++ -} prettyPrint (processTree env (addImports ast))
>     x -> return $ ppShow x

> processTree :: Data a => Environment -> a -> a
> processTree env =
>     transformBi $ \x ->
>       case x of
>         (PatBind _
>              (PVar (Ident fnName))
>              Nothing
>              (UnGuardedRhs(Lit (Exts.String sqlSrc)))
>              (BDecls [])) : tl
>           -> createWrapper env fnName sqlSrc ++ tl
>         x1 -> x1

> addImports :: Data a => a -> a
> addImports =
>     transformBi $ \x ->
>       case x of
>         Module sl mn o wt es im d -> Module sl mn o wt es (imports ++ im) d


> noSrcLoc :: SrcLoc
> noSrcLoc = (SrcLoc "" 0 0)

> imports :: [ImportDecl]
> imports = [ImportDecl {importLoc = noSrcLoc
>                       ,importModule = ModuleName "Database.HDBC"
>                       ,importQualified = False
>                       ,importSrc = False
>                       ,importPkg = Nothing
>                       ,importAs = Nothing
>                       ,importSpecs = Nothing
>                       }
>           ,ImportDecl {importLoc = noSrcLoc
>                       ,importModule = ModuleName "Database.HsSqlPpp.Dbms.WrapLib"
>                       ,importQualified = False
>                       ,importSrc = False
>                       ,importPkg = Nothing
>                       ,importAs = Nothing
>                       ,importSpecs = Nothing
>                       }]


> createWrapper :: Environment
>               -> String
>               -> String
>               -> [Decl]
> createWrapper env fnName sql =
>     let rt = getStatementType env sql
>     in case rt of
>       Left e -> error e
>       Right (StatementType pt ts) ->
>           let pts = map sqlTypeToHaskellTypeName pt
>               tns = map (sqlTypeToHaskellTypeName . snd) ts
>           in [makeTypeSig fnName pts tns
>              ,makeFn fnName sql pts tns]

> makeFn :: String -> String -> [String] -> [String] -> Decl
> makeFn fnName sql pts typeNames = FunBind
>       [ Match noSrcLoc(
>           Ident fnName )
>           (PVar (Ident "conn") : map (PVar . Ident) pNames)
>           Nothing (
>           UnGuardedRhs (
>             Do
>               [ Generator noSrcLoc (
>                   PVar ( Ident "r" ) ) (
>                   App (
>                     App (
>                       App (
>                         Var ( UnQual ( Ident "selectRelation" ) ) ) (
>                         Var ( UnQual ( Ident "conn" ) ) ) ) (
>                       Lit ( Exts.String sql ) ) ) (
>                     List $ map (\l -> App (
>                         Var ( UnQual ( Ident "toSql" ) ) ) (
>                         Var ( UnQual ( Ident l ) ) )) pNames
>                     ))
>               , Qualifier (
>                   InfixApp (
>                     Var ( UnQual ( Ident "return" ) ) ) (
>                     QVarOp ( UnQual ( Symbol "$" ) ) ) (
>                     InfixApp (
>                       App (
>                         App (
>                           Var ( UnQual ( Ident "flip" ) ) ) (
>                           Var ( UnQual ( Ident "map" ) ) ) ) (
>                         Var ( UnQual ( Ident "r" ) ) ) ) (
>                       QVarOp ( UnQual ( Symbol "$" ) ) ) (
>                       Lambda noSrcLoc
>                         [ PList (map (PVar . Ident) vns)
>                         ] (
>                         Tuple (map (\n -> App (vui "fromSql") (vui n)) vns)
>                         ) ) ) )
>               ] ) ) (
>           BDecls [] )
>       ]
>       where
>         varName n = "a" ++ show n
>         vns = map varName [0..length typeNames - 1]
>         vui = Var . UnQual . Ident
>         pName n = "b" ++ show n
>         pNames = map pName [0..length pts - 1]



> makeTypeSig :: String -> [String] -> [String] -> Decl
> makeTypeSig fnName argTypes typeNames =
>   TypeSig noSrcLoc [Ident fnName] $
>     TyForall Nothing [ClassA (UnQual (Ident "IConnection")) [TyVar(Ident "conn")]] $
>       foldr TyFun lastArg args
>   where
>     tc = TyCon . UnQual . Ident
>     tntt = (TyApp (tc "Maybe")) . tc
>     args = ((TyVar (Ident "conn")) : map tntt argTypes)
>     lastArg = (TyApp (tc "IO") (TyList (TyTuple Boxed $ map tntt typeNames)))

> sqlTypeToHaskellTypeName :: Sql.Type -> String
> sqlTypeToHaskellTypeName t =
>     case t of
>        ScalarType "text" -> "String"
>        ScalarType "int4" -> "Int"
>        _ -> "a"


> getStatementType :: Environment -> String -> Either String StatementType
> getStatementType env sql = do
>     ast <- tsl $ parseSql "" sql
>     let (_,aast) = typeCheck env ast
>     let a = getTopLevelInfos aast
>     return $ fromJust $ head a

> tsl :: (MonadError String m, Show t) => Either t a -> m a
> tsl x = case x of
>                Left s -> throwError $ show s
>                Right b -> return b
