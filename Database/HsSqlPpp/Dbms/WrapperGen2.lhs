Copyright 2010 Jake Wheat

More refined example wrapper generator. The idea is to start with
$(runStmt connStr sqlStr)
and return a fn :: (IConnection conn) => conn -> Arg1 -> Arg2 -> ...
                   -> IO (hlist * fieldname, fieldtype)

at some point the type checker will be able to determine whether the
output fields are maybes or not, we output them all as maybes at the
moment. For input args, this may not be possible, but we assume all
input args can never be null for now.

> {-# LANGUAGE TemplateHaskell,QuasiQuotes,EmptyDataDecls,ScopedTypeVariables,RankNTypes,FlexibleContexts #-}

> module Database.HsSqlPpp.Dbms.WrapperGen2 where

> import Language.Haskell.TH.Quote
> import Language.Haskell.TH.Syntax
> import Language.Haskell.TH.Lib
> import Language.Haskell.TH

> import Data.Maybe

> import Text.Show.Pretty (ppShow)

> import qualified GHC.Base
> import qualified GHC.Types

 > import Data.Generics.PlateData
 > import Data.Generics hiding (Prefix,Infix)

> import Control.Monad.Error

> import qualified Control.Monad as M

> import Database.HsSqlPpp.Dbms.WrapLib2
> import Database.HDBC

> import qualified Database.HsSqlPpp.Ast.SqlTypes as Sql
> import Database.HsSqlPpp.Ast.Catalog
> import Database.HsSqlPpp.Ast.TypeChecker
> import Database.HsSqlPpp.Parsing.Parser
> import Database.HsSqlPpp.Ast.Annotation


> {-
> data Ptype;   ptype :: Proxy Ptype ; ptype    = proxy::Proxy Ptype
> data Allegiance; allegiance :: Proxy Allegiance ; allegiance = proxy::Proxy Allegiance
> data Tag; tag :: Proxy Tag ; tag = proxy::Proxy Tag
> data X; x :: Proxy X ; x = proxy::Proxy X
> data Y; y :: Proxy Y ; y = proxy::Proxy Y


> sqlStmt :: String -> String -> Q Exp
> sqlStmt connStr sqlStr = do
>   runQ [| \conn ->
>           selectRelation conn sqlStr [] >>=
>           return . map (\ [a0, a1, a2, a3, a4] ->
>               ptype .=. fromSql a0 .*.
>               allegiance .=. fromSql a1 .*.
>               tag .=. fromSql a2 .*.
>               x .=. fromSql a3 .*.
>               y .=. fromSql a4 .*.
>               emptyRecord)
>         |]

> sqlStmt connStr sqlStr = return $ LitE $ StringL $ connStr ++ ":" ++ sqlStr
>. -}

> sqlStmt :: String -> String -> Q Exp
> sqlStmt dbName sqlStr = do
>   catU <- runIO $ readCatalogFromDatabase dbName
>   case updateCatalog defaultCatalog catU of
>     Left er -> error $ show er
>     Right cat -> do
>       let rt = getStatementType cat sqlStr
>       case rt of
>         Right (StatementType inA outA) ->
>             do
>                 argNames <- getNNewNames "a" $ length inA
>                 cnName <- newName "cn"
>                 ia <- inArgs argNames
>                 bd <- [| selectRelation $(return $ VarE cnName) sqlStr $(return ia) >>=
>                              return . map $mapper|]
>                 return (LamE (map VarP (cnName : argNames)) bd)
>            where
>              getNNewNames :: String -> Int -> Q [Name]
>              getNNewNames i n = forM [1..n] $ const $ newName i
>              inArgs argNames = do
>                tys <- mapM sqlTypeToHaskell inA
>                let ps :: [(Name, Type)]
>                    ps = zip argNames tys
>                a <- forM ps $ \(n,t) ->
>                    [| toSql $(return (SigE (VarE n) t))|]
>                return $ ListE a
>              mapper = do
>                       retNames <- getNNewNames "r" $ length outA
>                       let largs = ListP (map VarP retNames)
>                       ntp <- forM retNames $ \r -> [| fromSql $(return (VarE r)) |]
>                       tys <- mapM (sqlTypeToHaskell . snd) outA
>                       let ntpt :: [(Exp,Type)]
>                           ntpt = zip ntp tys
>                       ntp1 <- forM ntpt $ \(e,t) -> return $ SigE e t
>                       let r = LamE [largs] $ TupE ntp1
>                       return r
>         e -> error $ show e

StatementType [Type] [(String,Type)]

> test = do
>    e <- ex
>    putStrLn "-----------------------------"
>    putStrLn "results"
>    putStrLn $ ppShow e
>    putStrLn $ pprint e
>        where
>          ex :: IO Exp
>          ex = runQ [| $(do
>                 argNames <- getNNewNames "a" $ length inA
>                 cnName <- newName "cn"
>                 ia <- inArgs argNames
>                 {-runIO $ putStrLn "-----------------------------"
>                 runIO $ putStrLn "test:"
>                 runIO $ putStrLn $ ppShow ia
>                 runIO $ putStrLn $ pprint ia-}
>                 bd <- [| selectRelation $(return $ VarE cnName) sql $(return ia) >>=
>                              return . map $mapper|]
>                 return (LamE (map VarP (cnName : argNames)) bd)
>                 --return undefined
>                 {-runQ [|
>                      \cn r1 ->
>                          selectRelation cn sql [toSql (r1::Maybe Int)] >>=
>                          return . map $mapper
>                   |]-} ) |]
>          getNNewNames :: String -> Int -> Q [Name]
>          getNNewNames i n = forM [1..n] $ const $ newName i

>          inArgs argNames = do
>            tys <- mapM sqlTypeToHaskell inA
>            let ps :: [(Name, Type)]
>                ps = zip argNames tys
>            a <- forM ps $ \(n,t) ->
>                [| toSql $(return (SigE (VarE n) t))|]
>            return $ ListE a

(ListE [AppE (VarE Database.HDBC.SqlValue.toSql) (SigE (VarE r1_1) (AppT (ConT Data.Maybe.Maybe) (ConT GHC.Types.Int)))])

 >            
 >                     [| [toSql (r1::Maybe Int)] |]

>          mapper = do
>                   retNames <- getNNewNames "r" $ length outA
>                   let largs = ListP (map VarP retNames)
>                   ntp <- forM retNames $ \r -> [| fromSql $(return (VarE r)) |]
>                   tys <- mapM (sqlTypeToHaskell . snd) outA
>                   let ntpt :: [(Exp,Type)]
>                       ntpt = zip ntp tys
>                   ntp1 <- forM ntpt $ \(e,t) -> return $ SigE e t

>                   --runIO $ print ntp
>                   let r = LamE [largs] $ TupE ntp1
>                   --runIO $ putStrLn "-----------------------------"
>                   --runIO $ putStrLn "test:"
>                   --runIO $ putStrLn $ ppShow r
>                   --runIO $ putStrLn $ pprint r
>                   return r

  TupE
    [ AppE (
        VarE Database.HDBC.SqlValue.fromSql ) (
        VarE r_2 )
    , AppE (
        VarE Database.HDBC.SqlValue.fromSql ) (
        VarE r_3 )
    , AppE (
        VarE Database.HDBC.SqlValue.fromSql ) (
        VarE r_4 )
    , AppE (
        VarE Database.HDBC.SqlValue.fromSql ) (
        VarE r_5 )
    , AppE (
        VarE Database.HDBC.SqlValue.fromSql ) (
e        VarE r_6 )
    ] )

>                   --report True $  ntp
> -- = flip map outA 
>                   --let ltup = flip map retNames $ \r -> [| fromSql $(VarE r) |]
>                   --return $ LamE [largs] (TupE [SigE (AppE (VarE Database.HDBC.SqlValue.fromSql) (VarE a0_7)) (AppT (ConT Data.Maybe.Maybe) (ConT GHC.Base.String)),SigE (AppE (VarE Database.HDBC.SqlValue.fromSql) (VarE a1_8)) (AppT (ConT Data.Maybe.Maybe) (ConT GHC.Base.String)),SigE (AppE (VarE Database.HDBC.SqlValue.fromSql) (VarE a2_9)) (AppT (ConT Data.Maybe.Maybe) (ConT GHC.Types.Int)),SigE (AppE (VarE Database.HDBC.SqlValue.fromSql) (VarE a3_10)) (AppT (ConT Data.Maybe.Maybe) (ConT GHC.Types.Int)),SigE (AppE (VarE Database.HDBC.SqlValue.fromSql) (VarE a4_11)) (AppT (ConT Data.Maybe.Maybe) (ConT GHC.Types.Int))])


>                   {-runQ [| (\ [a0, a1, a2, a3, a4] ->
>                                            (fromSql a0::Maybe String
>                                            ,fromSql a1::Maybe String
>                                            ,fromSql a2::Maybe Int
>                                            ,fromSql a3::Maybe Int
>                                            ,fromSql a4::Maybe Int))|]-}
>          st = StatementType [Sql.typeInt,Sql.typeInt]
>                      [("a", Sql.ScalarType "text")
>                      ,("b", Sql.ScalarType "text")
>                      ,("c", Sql.typeInt)
>                      ,("d", Sql.typeInt)
>                      ,("e", Sql.typeInt)]
>          (StatementType inA outA) = st
>          sql = "select * from pieces"


LamE [VarP cn_0,VarP r1_1] (InfixE (Just (AppE (AppE (AppE (VarE Database.HsSqlPpp.Dbms.WrapLib.selectRelation) (VarE cn_0)) (ListE [LitE (CharL 's'),LitE (CharL 'e'),LitE (CharL 'l'),LitE (CharL 'e'),LitE (CharL 'c'),LitE (CharL 't'),LitE (CharL ' '),LitE (CharL '*'),LitE (CharL ' '),LitE (CharL 'f'),LitE (CharL 'r'),LitE (CharL 'o'),LitE (CharL 'm'),LitE (CharL ' '),LitE (CharL 'p'),LitE (CharL 'i'),LitE (CharL 'e'),LitE (CharL 'c'),LitE (CharL 'e'),LitE (CharL 's')])) (ListE [AppE (VarE Database.HDBC.SqlValue.toSql) (SigE (VarE r1_1) (AppT (ConT Data.Maybe.Maybe) (ConT GHC.Types.Int)))]))) (VarE GHC.Base.>>=) (Just (InfixE (Just (VarE GHC.Base.return)) (VarE GHC.Base..) (Just (AppE (VarE GHC.Base.map) (LamE [ListP [VarP r_2,VarP r_3,VarP r_4,VarP r_5,VarP r_6]] (TupE [SigE (AppE (VarE Database.HDBC.SqlValue.fromSql) (VarE r_2)) (AppT (ConT Data.Maybe.Maybe) (ConT GHC.Base.String)),SigE (AppE (VarE Database.HDBC.SqlValue.fromSql) (VarE r_3)) (AppT (ConT Data.Maybe.Maybe) (ConT GHC.Base.String)),SigE (AppE (VarE Database.HDBC.SqlValue.fromSql) (VarE r_4)) (AppT (ConT Data.Maybe.Maybe) (ConT GHC.Types.Int)),SigE (AppE (VarE Database.HDBC.SqlValue.fromSql) (VarE r_5)) (AppT (ConT Data.Maybe.Maybe) (ConT GHC.Types.Int)),SigE (AppE (VarE Database.HDBC.SqlValue.fromSql) (VarE r_6)) (AppT (ConT Data.Maybe.Maybe) (ConT GHC.Types.Int))])))))))

>{-   runQ [| \cn ->
>           selectRelation cn sqlStr [] >>=
>           return . map (\ [a0, a1, a2, a3, a4] ->
>             (fromSql a0::Maybe String
>             ,fromSql a1::Maybe String
>             ,fromSql a2::Maybe Int
>             ,fromSql a3::Maybe Int
>             ,fromSql a4::Maybe Int))
>         |]-}

  = do r <- selectRelation conn "select * from pieces;" []
       return $
         flip map r $
           \ [a0, a1, a2, a3, a4] ->
             (fromSql a0::Maybe String
             ,fromSql a1::Maybe String
             ,fromSql a2::Maybe Int
             ,fromSql a3::Maybe Int
             ,fromSql a4::Maybe Int)

pieces ::
         (IConnection conn) =>
         conn ->
           IO [(Maybe String, Maybe String, Maybe Int, Maybe Int, Maybe Int)]
pieces conn
  = do r <- selectRelation conn "select * from pieces;" []
       return $
         flip map r $
           \ [a0, a1, a2, a3, a4] ->
             (fromSql a0, fromSql a1, fromSql a2, fromSql a3, fromSql a4)
 


================================================================================

parsing and typechecking

get the input and output types for a parameterized sql statement:

> getStatementType :: Catalog -> String -> Either String StatementType
> getStatementType cat sql = do
>     ast <- tsl $ parseSql "" sql
>     let (_,aast) = typeCheck cat ast
>     let a = getTopLevelInfos aast
>     return $ fromJust $ head a

return the equivalent haskell type for a sql type as a string

 > sqlTypeToHaskellTypeName :: Sql.Type -> String

> sqlTypeToHaskell :: Sql.Type -> TypeQ
> sqlTypeToHaskell t =
>     case t of
>        Sql.ScalarType "text" -> [t| Maybe String |]
>        Sql.ScalarType "int4" -> [t| Maybe Int |]
>        Sql.ScalarType "int8" -> [t| Maybe Int |]
>        Sql.ScalarType "bool" -> [t| Maybe Bool |]
>        Sql.DomainType _ -> [t| Maybe String |]
>        x -> error $ show x

================================================================================

error utility - convert either to ErrorT String

> tsl :: (MonadError String m, Show t) => Either t a -> m a
> tsl x = case x of
>                Left s -> throwError $ show s
>                Right b -> return b
