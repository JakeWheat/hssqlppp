Copyright 2010 Jake Wheat

demo code for using the hssqlppp typechecker to generate typesafe
database access code

could probably use some quasi quotation

> {-# LANGUAGE FlexibleContexts #-}

> module Database.HsSqlPpp.Dbms.WrapperGen
>     (wrapperGen) where

> import Text.Show.Pretty (ppShow)
> import Language.Haskell.Exts hiding (String)
> import qualified Language.Haskell.Exts as Exts
> -- import Control.Monad
> import Data.Generics.PlateData
> import Data.Generics hiding (Prefix,Infix)
> import Control.Monad.Error
> import Data.Maybe
> -- import System.IO.Unsafe

> import Database.HsSqlPpp.Ast.SqlTypes as Sql
> import Database.HsSqlPpp.Ast.Environment
> import Database.HsSqlPpp.Ast.TypeChecker
> import Database.HsSqlPpp.Parsing.Parser
> import Database.HsSqlPpp.Ast.Annotation

> wrapperGen :: String -> String -> IO String
> wrapperGen db fn = do
>   -- putStrLn $ "looking for " ++ s
>   p <- parseFile fn
>   case p of
>     ParseOk ast -> do
>                    envU <- readEnvironmentFromDatabase db
>                    case updateEnvironment defaultEnvironment envU of
>                      Left er -> return $ show er
>                      Right env ->
>                          return $ {- ppShow ast ++  "\n\n" ++ -} prettyPrint (processTree env (addImports ast))
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
>       Right (StatementType _ ts) ->
>           let tns = map (sqlTypeToHaskellTypeName . snd) ts
>           in [makeTypeSig fnName tns
>              ,makeFn fnName sql tns]

> makeFn :: String -> String -> [String] -> Decl
> makeFn fnName sql typeNames = FunBind
>       [ Match noSrcLoc(
>           Ident fnName )
>           [ PVar ( Ident "conn" )
>           ]
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
>                     List [] ) )
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


> makeTypeSig :: String -> [String] -> Decl
> makeTypeSig fnName typeNames =
>   TypeSig noSrcLoc [Ident fnName] $
>     TyForall Nothing [ClassA (UnQual (Ident "IConnection")) [TyVar(Ident "conn")]] $
>       TyFun (TyVar (Ident "conn")) $
>         TyApp (tc "IO") $ TyList (TyTuple Boxed $ map tntt typeNames)
>   where
>     tc = TyCon . UnQual . Ident
>     tntt = (TyApp (tc "Maybe")) . tc

> sqlTypeToHaskellTypeName :: Sql.Type -> String
> sqlTypeToHaskellTypeName t =
>     case t of
>        ScalarType "text" -> "String"
>        ScalarType "int4" -> "Int"
>        _ -> "a"


> getStatementType :: Environment -> String -> Either String StatementType
> getStatementType env sql = do
>   --runErrorT $ do
>     --let ast = parseSql "" sql
>     ast <- tsl $ parseSql "" sql
>     --envU <- liftIO (readEnvironmentFromDatabase "chaos")
>     --env <- tsl $ updateEnvironment defaultEnvironment envU
>     let (_,aast) = typeCheck env ast
>     let a = getTopLevelInfos aast
>     return $ fromJust $ head a

 >       _ -> throwError $ "string didn't contain onne sql statement: " ++ sql

 >     ast <- parseSql "" sql
 >     return []

 >     Left "x"

 > tsl :: (Show a) => Either a b -> Either String b

 > tsl :: (MonadError String m, Show t) => Either t a -> m a

> tsl :: (MonadError String m, Show t) => Either t a -> m a
> tsl x = case x of
>                Left s -> throwError $ show s
>                Right b -> return b

PatBind
      SrcLoc
        { srcFilename = "Test.hesql"
        , srcLine = 3
        , srcColumn = 1
        } (
      PVar ( Ident "pieces" ) )
      Nothing (
      UnGuardedRhs ( Lit ( String "select * from pieces;" ) ) ) (
      BDecls [] )

>{-  , TypeSig
>       SrcLoc
>         { srcFilename = "TestHesql.hs"
>         , srcLine = 19
>         , srcColumn = 1
>         }
>       [ Ident "pieces2"
>       ] (
>       TyForall
>         Nothing
>         [ ClassA (
>             UnQual ( Ident "IConnection" ) )
>             [ TyVar ( Ident "conn" )
>             ]
>         ] (
>         TyFun (
>           TyVar ( Ident "conn" ) ) (
>           TyApp (
>             TyCon ( UnQual ( Ident "IO" ) ) ) (
>             TyList (
>               TyTuple
>                 Boxed
>                 [ TyApp (
>                     TyCon ( UnQual ( Ident "Maybe" ) ) ) (
>                     TyCon ( UnQual ( Ident "String" ) ) )
>                 , TyApp (
>                     TyCon ( UnQual ( Ident "Maybe" ) ) ) (
>                     TyCon ( UnQual ( Ident "String" ) ) )
>                 , TyApp (
>                     TyCon ( UnQual ( Ident "Maybe" ) ) ) (
>                     TyCon ( UnQual ( Ident "Int" ) ) )
>                 , TyApp (
>                     TyCon ( UnQual ( Ident "Maybe" ) ) ) (
>                     TyCon ( UnQual ( Ident "Int" ) ) )
>                 , TyApp (
>                     TyCon ( UnQual ( Ident "Maybe" ) ) ) (
>                     TyCon ( UnQual ( Ident "Int" ) ) )
>                 ] ) ) ) ) )-}

FunBind
      [ Match
          SrcLoc
            { srcFilename = "TestHesql.hs"
            , srcLine = 24
            , srcColumn = 1
            } (
          Ident "pieces2" )
          [ PVar ( Ident "conn" )
          ]
          Nothing (
          UnGuardedRhs (
            Do
              [ Generator
                  SrcLoc
                    { srcFilename = "TestHesql.hs"
                    , srcLine = 25
                    , srcColumn = 16
                    } (
                  PVar ( Ident "r" ) ) (
                  App (
                    App (
                      App (
                        Var ( UnQual ( Ident "selectRelation" ) ) ) (
                        Var ( UnQual ( Ident "conn" ) ) ) ) (
                      Lit ( String "select * from pieces;" ) ) ) (
                    List [] ) )
              , Qualifier (
                  InfixApp (
                    Var ( UnQual ( Ident "return" ) ) ) (
                    QVarOp ( UnQual ( Symbol "$" ) ) ) (
                    InfixApp (
                      App (
                        App (
                          Var ( UnQual ( Ident "flip" ) ) ) (
                          Var ( UnQual ( Ident "map" ) ) ) ) (
                        Var ( UnQual ( Ident "r" ) ) ) ) (
                      QVarOp ( UnQual ( Symbol "$" ) ) ) (
                      Lambda
                        SrcLoc
                          { srcFilename = "TestHesql.hs"
                          , srcLine = 26
                          , srcColumn = 38
                          }
                        [ PList
                            [ PVar ( Ident "a" )
                            , PVar ( Ident "b" )
                            , PVar ( Ident "c" )
                            , PVar ( Ident "d" )
                            , PVar ( Ident "e" )
                            ]
                        ] (
                        Tuple
                          [ App (
                              Var ( UnQual ( Ident "fromSql" ) ) ) (
                              Var ( UnQual ( Ident "a" ) ) )
                          , App (
                              Var ( UnQual ( Ident "fromSql" ) ) ) (
                              Var ( UnQual ( Ident "b" ) ) )
                          , App (
                              Var ( UnQual ( Ident "fromSql" ) ) ) (
                              Var ( UnQual ( Ident "c" ) ) )
                          , App (
                              Var ( UnQual ( Ident "fromSql" ) ) ) (
                              Var ( UnQual ( Ident "d" ) ) )
                          , App (
                              Var ( UnQual ( Ident "fromSql" ) ) ) (
                              Var ( UnQual ( Ident "e" ) ) )
                          ] ) ) ) )
              ] ) ) (
          BDecls [] )
      ]


 , TypeSig
      SrcLoc
        { srcFilename = "TestHesql.hs"
        , srcLine = 19
        , srcColumn = 1
        }
      [ Ident "pieces2"
      ] (
      TyForall
        Nothing
        [ ClassA (
            UnQual ( Ident "IConnection" ) )
            [ TyVar ( Ident "conn" )
            ]
        ] (
        TyFun (
          TyVar ( Ident "conn" ) ) (
          TyApp (
            TyCon ( UnQual ( Ident "IO" ) ) ) (
            TyList (
              TyTuple
                Boxed
                [ TyApp (
                    TyCon ( UnQual ( Ident "Maybe" ) ) ) (
                    TyCon ( UnQual ( Ident "String" ) ) )
                , TyApp (
                    TyCon ( UnQual ( Ident "Maybe" ) ) ) (
                    TyCon ( UnQual ( Ident "String" ) ) )
                , TyApp (
                    TyCon ( UnQual ( Ident "Maybe" ) ) ) (
                    TyCon ( UnQual ( Ident "Int" ) ) )
                , TyApp (
                    TyCon ( UnQual ( Ident "Maybe" ) ) ) (
                    TyCon ( UnQual ( Ident "Int" ) ) )
                , TyApp (
                    TyCon ( UnQual ( Ident "Maybe" ) ) ) (
                    TyCon ( UnQual ( Ident "Int" ) ) )
                ] ) ) ) ) )
