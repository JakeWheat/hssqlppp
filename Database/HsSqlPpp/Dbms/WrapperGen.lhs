Copyright 2010 Jake Wheat

> {-# LANGUAGE FlexibleContexts #-}

> module Database.HsSqlPpp.Dbms.WrapperGen
>     (wrapperGen) where

> import Text.Show.Pretty
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
>                          return $ ppShow ast ++ "\n\n" ++ prettyPrint (processTree env ast)
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

> noSrcLoc :: SrcLoc
> noSrcLoc = (SrcLoc "" 0 0)

> createWrapper :: Environment
>               -> String
>               -> String
>               -> [Decl]
> createWrapper env fnName sql =
>     let rt = getStatementType env sql
>     in case rt of
>       Left e -> error e
>       Right (StatementType _ ts) ->
>           [makeTypeSig fnName (map (sqlTypeToHaskellTypeName . snd) ts)
>           ,PatBind noSrcLoc
>             (PVar (Ident fnName))
>             Nothing
>             (UnGuardedRhs(Lit ( Exts.String (show $ getStatementType env sql))))
>             (BDecls [])]

> makeTypeSig :: String -> [String] -> Decl
> makeTypeSig fnName typeNames =
>   TypeSig noSrcLoc [Ident fnName]
>     (TyForall Nothing [ClassA (UnQual (Ident "IConnection")) [TyVar(Ident "conn")]]
>        (TyFun (TyVar (Ident "conn"))
>           (TyApp (TyCon (UnQual (Ident "IO")))
>             (TyList (TyTuple Boxed
>                 (flip map typeNames $ \t -> TyApp (
>                     TyCon(UnQual(Ident "Maybe"))) (
>                     TyCon(UnQual(Ident t)))))))))

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
