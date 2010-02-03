Copyright 2010 Jake Wheat

demo code for using the hssqlppp typechecker to generate typesafe
database access code

could probably use some quasi quotation

> {-# LANGUAGE FlexibleContexts #-}

> module Database.HsSqlPpp.Dbms.WrapperGen
>     (wrapperGen) where

> import Language.Haskell.Exts hiding (String)
> import qualified Language.Haskell.Exts as Exts
> import Data.Generics.PlateData
> import Data.Generics hiding (Prefix,Infix)
> import Control.Monad.Error
> import Data.Maybe

> import Database.HsSqlPpp.Ast.SqlTypes as Sql
> import Database.HsSqlPpp.Ast.Catalog
> import Database.HsSqlPpp.Ast.TypeChecker
> import Database.HsSqlPpp.Parsing.Parser
> import Database.HsSqlPpp.Ast.Annotation


> -- | takes a haskell source file and produces a haskell source file
> -- with typesafe wrappers
> --
> -- Example:
> --
> -- > module Testhesql1 where
> -- > pieces = "select * from pieces;"
> -- > turn_number = "select get_turn_number();"
> -- > pieces_at_pos = "select * from pieces where x = ? and y = ?;"
> --
> -- is transformed to
> --
> -- > module Testhesql1 where
> -- > import Database.HDBC
> -- > import Database.HsSqlPpp.Dbms.WrapLib
> -- >
> -- > pieces ::
> -- >          (IConnection conn) =>
> -- >          conn ->
> -- >            IO [(Maybe String, Maybe String, Maybe Int, Maybe Int, Maybe Int)]
> -- > pieces conn
> -- >   = do r <- selectRelation conn "select * from pieces;" []
> -- >        return $
> -- >          flip map r $
> -- >            \ [a0, a1, a2, a3, a4] ->
> -- >              (fromSql a0, fromSql a1, fromSql a2, fromSql a3, fromSql a4)
> -- >
> -- > turn_number :: (IConnection conn) => conn -> IO [(Maybe Int)]
> -- > turn_number conn
> -- >   = do r <- selectRelation conn "select get_turn_number();" []
> -- >        return $ flip map r $ \ [a0] -> (fromSql a0)
> -- >
> -- > pieces_at_pos ::
> -- >                 (IConnection conn) =>
> -- >                 conn ->
> -- >                   Maybe Int ->
> -- >                     Maybe Int ->
> -- >                       IO [(Maybe String, Maybe String, Maybe Int, Maybe Int, Maybe Int)]
> -- > pieces_at_pos conn b0 b1
> -- >   = do r <- selectRelation conn
> -- >               "select * from pieces where x = ? and y = ?;"
> -- >               [toSql b0, toSql b1]
> -- >        return $
> -- >          flip map r $
> -- >            \ [a0, a1, a2, a3, a4] ->
> -- >              (fromSql a0, fromSql a1, fromSql a2, fromSql a3, fromSql a4)
> --
> -- This code is just an example of how to get the type information
> -- out for each sql statement, please see the source code for how it
> -- works.  It's not nearly good enough for production use.

> wrapperGen :: String -- ^ name of database to typecheck against
>            -> FilePath -- ^ haskell source filename to process
>            -> IO String -- ^ generated wrapper source code
> wrapperGen db fn = do
>   p <- parseFile fn
>   case p of
>     ParseOk ast -> do
>                    catU <- readCatalogFromDatabase db
>                    case updateCatalog defaultCatalog catU of
>                      Left er -> return $ show er
>                      Right cat ->
>                          return $ {-ppShow ast ++  "\n\n" ++ -} prettyPrint (processTree cat (addImports ast))
>     x -> return $ show x

This is the function which finds the statements which look like
ident = "string"
and converts them into hdbc wrappers with the correct types

> processTree :: Data a => Catalog -> a -> a
> processTree cat =
>     transformBi $ \x ->
>       case x of
>         (PatBind _
>              (PVar (Ident fnName))
>              Nothing
>              (UnGuardedRhs(Lit (Exts.String sqlSrc)))
>              (BDecls [])) : tl
>           -> createWrapper cat fnName sqlSrc ++ tl
>         x1 -> x1

for each bind to convert, lookup the haskell types needed, then
create a type sig and a function to use hdbc to run the sql

> createWrapper :: Catalog
>               -> String
>               -> String
>               -> [Decl]
> createWrapper cat fnName sql =
>     let rt = getStatementType cat sql
>     in case rt of
>       Left e -> error e
>       Right (StatementType pt ts) ->
>           let pts = map sqlTypeToHaskellTypeName pt
>               tns = map (sqlTypeToHaskellTypeName . snd) ts
>           in [makeTypeSig fnName pts tns
>              ,makeFn fnName sql pts tns]

================================================================================

create the type signature for a wrapper, produces something like
(IConnection conn) => conn -> inarg1 -> inarg2 -> ... ->
             IO [(outarg1, outarg2, ...)]

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

================================================================================

create the function which calls hdbc

takes something like:
pieces_at_pos = "select * from pieces where x = ? and y = ?;"
and produces:

pieces_at_pos conn b0 b1
  = do r <- selectRelation conn
              "select * from pieces where x = ? and y = ?;"
              [toSql b0, toSql b1]
       return $
         flip map r $
           \ [a0, a1, a2, a3, a4] ->
             (fromSql a0, fromSql a1, fromSql a2, fromSql a3, fromSql a4)

doesn't really need to know the types, just the number of inargs and outargs,
since the work is done by hdbc's toSql and fromSql, and by the type signature
that is generated in the function above

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

================================================================================

> addImports :: Data a => a -> a
> addImports =
>     transformBi $ \x ->
>       case x of
>         Module sl mn o wt es im d -> Module sl mn o wt es (imports ++ im) d

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
> sqlTypeToHaskellTypeName t =
>     case t of
>        ScalarType "text" -> "String"
>        ScalarType "int4" -> "Int"
>        ScalarType "int8" -> "Int"
>        ScalarType "bool" -> "Bool"
>        DomainType _ -> "String"
>        x -> show x

================================================================================

simple ast shortcuts

> noSrcLoc :: SrcLoc
> noSrcLoc = (SrcLoc "" 0 0)

================================================================================

error utility - convert either to ErrorT String

> tsl :: (MonadError String m, Show t) => Either t a -> m a
> tsl x = case x of
>                Left s -> throwError $ show s
>                Right b -> return b
