

-- UUAGC 0.9.10 (Ast.ag)
module Ast(
    -- exports
    MySourcePos

    --ast nodes
   ,Statement (..)
   ,SelectExpression (..)
   ,FnBody (..)
   ,SetClause (..)
   ,TableRef (..)
   ,JoinExpression (..)
   ,JoinType (..)
   ,SelectList (..)
   ,SelectItem (..)
   ,CopySource (..)
   ,AttributeDef (..)
   ,RowConstraint (..)
   ,Constraint (..)
   ,TypeAttributeDef (..)
   ,ParamDef (..)
   ,VarDef (..)
   ,RaiseType (..)
   ,CombineType (..)
   ,Volatility (..)
   ,Language (..)
   ,TypeName (..)
   ,DropType (..)
   ,Cascade (..)
   ,Direction (..)
   ,Distinct (..)
   ,Natural (..)
   ,IfExists (..)
   ,RestartIdentity (..)
   ,Expression (..)
   ,FunName (..)
   ,KeywordOperator(..)
   ,OperatorType (..)
   ,getOperatorType
   ,InList (..)
   ,StatementList
   --checking stuff
   ,Message (..)
   ,MessageStuff (..)
   --types
   ,Type (..)
   ,PseudoType (..)
   ,TypeErrorInfo (..)
   --fns
   ,checkAst
   ,getExpressionType
   ,resetSps
   ,resetSp
   ,resetSp'
   ,resetSps'
   ,nsp
   ,checkFunctionTypes
   ,typeSmallInt
   ,typeBigInt
   ,typeInt
   ,typeNumeric
   ,typeFloat4
   ,typeFloat8
   ,typeVarChar
   ,typeChar
   ,typeBool

) where

import Data.Maybe



-- for now, assume that all the overloaded operators that have the
-- same name are all either binary, prefix or postfix, otherwise the
-- getoperatortype would need the types of the arguments to determine
-- the operator type, and the parser would have to be a lot cleverer

getOperatorType :: String -> OperatorType
getOperatorType s = case () of
                      _ | any (\(x,_,_) -> x == s) binaryOperatorTypes ->
                            BinaryOp
                        | any (\(x,_,_) -> x == s ||
                                           (x=="-" && s=="u-"))
                              prefixOperatorTypes ->
                            PrefixOp
                        | any (\(x,_,_) -> x == s) postfixOperatorTypes ->
                            PostfixOp
                        --special case the keyword operators
                        | otherwise ->
                            error $ "don't know flavour of operator " ++ s



instance Show Message where
   show m = showMessage m

showMessage :: Message -> [Char]
showMessage m = case m of
                  Error sp s -> showit "Error" sp s
                  Warning sp s -> showit "Warning" sp s
                  Notice sp s -> showit "Notice" sp s
                where
                  showit lev (fn,l,c) s = lev ++ "\n" ++ fn ++ ":"
                                          ++ show l ++ ":" ++ show c ++ ":\n"
                                          ++ show s ++ "\n"


setUnknown :: Type -> Type -> Type
setUnknown _ _ = UnknownType

appendTypeList :: Type -> Type -> Type
appendTypeList t1 (TypeList ts) = TypeList (t1:ts)
appendTypeList t1 t2 = TypeList (t1:t2:[])

--if the first argument is unknown or type error, pass it on
--otherwise use the second argument
propagateUnknownError :: Type -> Type -> Type
propagateUnknownError t t1 =
    case t of
      a@(TypeError _ _) -> a
      UnknownType -> UnknownType
      TypeList l -> doTypeList l
      _ -> t1
    where
      -- run through the type list, if there are any eorors, collect
      -- them all into a list
      -- otherwise, if there are any unknowns, then the type is
      -- unknown
      -- otherwise, keep the list the same
      doTypeList ts =
          let unks = filter (\u -> case u of
                                     UnknownType -> True
                                     _ -> False) ts
              errs = filter (\u -> case u of
                                     TypeError _ _ -> True
                                     _ -> False) ts
          in case () of
               _ | length errs > 0 -> case () of
                                        _ | length errs == 1 -> head errs
                                          | otherwise -> TypeList errs
                 | length unks > 0 -> UnknownType
                 | otherwise -> t1

typesFromTypeList :: Type -> [Type]
typesFromTypeList (TypeList ts) = ts
typesFromTypeList x = error $ "can't get types from list " ++ show x

typeFromArray :: Type -> Type
typeFromArray (ArrayType t) = t
typeFromArray x = error $ "can't get types from non array " ++ show x

isArrayType :: Type -> Bool
isArrayType (ArrayType _) = True
isArrayType (Pseudo AnyArray) = True
isArrayType _ = False



type TypeList = [Type]
type ArgCheckList = [ArgCheck]


type TypePred = (Type -> Bool)
type TypePredError = (Type -> TypeErrorInfo)

exactType :: Type -> ArgCheck
exactType t = ArgCheck (t==) (WrongType t)

checkPredList :: MySourcePos -> [ArgCheck] -> [Type] -> [Type]
checkPredList sp achks ats =
    if length achks /= length ats
      then [TypeError sp
            (WrongNumArgs
             (length achks)
             (length ats))]
      else checkArg 0 [] achks ats
    where
      checkArg :: Int -> [Type] -> [ArgCheck] -> [Type] -> [Type]
      checkArg n acc ((ArgCheck chk err):chks) (t:ts) =
          if chk t
            then checkArg (n+1) acc chks ts
            else checkArg (n+1) (acc ++ [TypeError sp $ err t]) chks ts
      checkArg n acc [] [] = acc


type RetTypeFunner = ([Type] -> Type)

checkTypes :: MySourcePos -> Type -> ArgsCheck -> RetType -> Type
checkTypes sp tl@(TypeList l) argC retT =
    --1: check tl for errors or unknowns
    --2: check the args against the constraints,
    --  filter this for unknown or errors
    --  (it returns Just error, or Nothing if ok)
    --3: get the return type, and check that for unknowns or errors
    --4: success, return the result type
    let c = case checkArgs of
              Just t -> t
              Nothing -> getRetType
    in pe tl $ pe c c
    where
      getRetType =
          case retT of
            ConstRetType t -> t
            RetTypeAsArgN n -> l !! n
            RetTypeFun f -> f l
      checkArgs =
          case argC of
            AllSameType t -> checkArgListMatches t l
            AllSameType1 t | length l == 0 ->
                                Just $ te NeedOneOrMoreArgs
                           | otherwise -> checkArgListMatches t l
            AllSameTypeNum t n | length l /= n ->
                                    Just $ te $ WrongNumArgs n (length l)
                               | otherwise -> checkArgListMatches t l
            AllSameTypeNumAny n | length l /= n ->
                                    Just $ te $ WrongNumArgs n (length l)
                                | otherwise -> checkArgListMatches (head l) l
            AllSameTypeAny -> checkArgListMatches (head l) l
            AllSameType1Any | length l == 0 ->
                                Just $ te NeedOneOrMoreArgs
                            | otherwise -> checkArgListMatches (head l) l
            ExactList ts | ts /= l ->
                              Just $ te $ WrongTypeList ts l
                         | otherwise -> Nothing
            ExactPredList chks -> case checkPredList sp chks l of
                                    x | length x == 0 -> Nothing
                                      | otherwise -> Just $ TypeList x
            AllSameTypePredNum p n -> case checkPredList sp
                                             (replicate n p)
                                             l of
                                        x | length x == 0 -> Nothing
                                          | otherwise -> Just $ TypeList x
      checkArgListMatches tc tcs = if all (==tc) tcs
                                   then Nothing
                                   else Just $ te (WrongTypes tc tcs)
      te = TypeError sp
      pe = propagateUnknownError


checkTypes _ x _ _ = error $ "can't check types of non type list: " ++ show x




typeSmallInt = ScalarType "int2"
typeBigInt = ScalarType "int8"
typeInt = ScalarType "int4"
typeNumeric = ScalarType "numeric"
typeFloat4 = ScalarType "float4"
typeFloat8 = ScalarType "float8"
typeVarChar = ScalarType "varchar"
typeChar = ScalarType "char"
typeBool = ScalarType "bool"

canonicalizeType t =
    case t of
      ScalarType s -> cName s
      ArrayType a -> ArrayType $ canonicalizeType a
      SetOfType a -> SetOfType $ canonicalizeType a
      t1@_ -> t1
    where
      cName s = case () of
                  _ | s `elem` smallIntNames -> typeSmallInt
                    | s `elem` intNames -> typeInt
                    | s `elem` bigIntNames -> typeBigInt
                    | s `elem` numericNames -> typeNumeric
                    | s `elem` float4Names -> typeFloat4
                    | s `elem` float8Names -> typeFloat8
                    | s `elem` varcharNames -> typeVarChar
                    | s `elem` charNames -> typeChar
                    | s `elem` boolNames -> typeBool
                    | otherwise -> ScalarType s
      smallIntNames = ["int2", "smallint"]
      intNames = ["int4", "integer", "int"]
      bigIntNames = ["int8", "bigint"]
      numericNames = ["numeric", "decimal"]
      float4Names = ["real", "float4"]
      float8Names = ["double precision", "float"]
      varcharNames = ["character varying", "varchar"]
      charNames = ["character", "char"]
      boolNames = ["boolean", "bool"]





allOpsAndFns :: [(String, [Type], Type)]
allOpsAndFns = binaryOperatorTypes
               ++ prefixOperatorTypes
               ++ postfixOperatorTypes
               ++ functionTypes

--add the keyword operators here

keywordBinaryOperatorTypes :: [(KeywordOperator,[Type],Type)]
keywordBinaryOperatorTypes = [
 (And, [typeBool, typeBool], typeBool),
 (Or, [typeBool, typeBool], typeBool),
 (Like, [ScalarType "text", ScalarType "text"], typeBool)]
keywordUnaryOperatorTypes :: [(KeywordOperator,[Type],Type)]
keywordUnaryOperatorTypes = [
 (Not, [typeBool], typeBool),
 (IsNull, [ScalarType "any"], typeBool),
 (IsNotNull, [ScalarType "any"], typeBool)]

allKeywordOps :: [(KeywordOperator, [Type], Type)]
allKeywordOps = keywordBinaryOperatorTypes ++ keywordUnaryOperatorTypes

allTypes :: [Type]
allTypes = defaultTypeNames

checkFunctionTypes :: [(String, [Type])]
checkFunctionTypes =
    catMaybes $ map (\(f,a,r) ->
                     let ts = (r:a)
                         badts = filter (not . knownType) ts
                     in if length badts == 0
                          then Nothing
                          else Just (f, badts)
                    ) allOpsAndFns
    where
      knownType :: Type -> Bool
      knownType l = case l of
                      Pseudo _ -> True
                      t@(ScalarType _) -> t `elem` defaultTypeNames
                      ArrayType t -> knownType t
                      SetOfType t -> knownType t



checkAst :: StatementList -> [Message]
checkAst sts = let t = sem_Root (Root sts)
               in (messages_Syn_Root (wrap_Root t Inh_Root))
{-
================================================================================

= Types

== getExpressionType

Gets the type of an expression

-}

getExpressionType :: Expression -> Type
getExpressionType ex = let t = sem_ExpressionRoot (ExpressionRoot ex)
                       in (nodeType_Syn_ExpressionRoot
                           (wrap_ExpressionRoot t Inh_ExpressionRoot))


--hack job, often not interested in the source positions when testing
--the asts produced, so this function will reset all the source
--positions to empty ("", 0, 0)

resetSps :: [Statement] -> [Statement]
resetSps sts = map resetSp sts

resetSp :: Statement -> Statement
resetSp (CreateFunction l n p r bq b v) = (CreateFunction l n p r bq
                                              (case b of
                                                SqlFnBody stss -> SqlFnBody (map resetSp' stss)
                                                PlpgsqlFnBody vd stss -> PlpgsqlFnBody vd (map resetSp' stss))
                                            v)
resetSp (ForSelectStatement v s stss) = ForSelectStatement v s (map resetSp' stss)
resetSp (ForIntegerStatement v f t stss) = ForIntegerStatement v f t (map resetSp' stss)
resetSp (CaseStatement v cs els) = CaseStatement v (map (\(el,st) -> (el,map resetSp' st)) cs) (map resetSp' els)
resetSp (If cs els) = If (map (\(el,st) -> (el,map resetSp' st)) cs) (map resetSp' els)
resetSp a = a

resetSp' :: SourcePosStatement -> SourcePosStatement
resetSp' (_,st) = (nsp,resetSp st)

resetSps' :: StatementList -> StatementList
resetSps' sts = map resetSp' sts

nsp :: MySourcePos
nsp = ("", 0,0)


defaultTypeNames = [
    ArrayType (Pseudo Cstring),
    ArrayType (Pseudo Record),
    ArrayType (ScalarType "abstime"),
    ArrayType (ScalarType "aclitem"),
    ArrayType (ScalarType "bit"),
    ArrayType (ScalarType "bool"),
    ArrayType (ScalarType "box"),
    ArrayType (ScalarType "bpchar"),
    ArrayType (ScalarType "bytea"),
    ArrayType (ScalarType "char"),
    ArrayType (ScalarType "cid"),
    ArrayType (ScalarType "cidr"),
    ArrayType (ScalarType "circle"),
    ArrayType (ScalarType "date"),
    ArrayType (ScalarType "float4"),
    ArrayType (ScalarType "float8"),
    ArrayType (ScalarType "gtsvector"),
    ArrayType (ScalarType "inet"),
    ArrayType (ScalarType "int2"),
    ArrayType (ScalarType "int2vector"),
    ArrayType (ScalarType "int4"),
    ArrayType (ScalarType "int8"),
    ArrayType (ScalarType "interval"),
    ArrayType (ScalarType "line"),
    ArrayType (ScalarType "lseg"),
    ArrayType (ScalarType "macaddr"),
    ArrayType (ScalarType "money"),
    ArrayType (ScalarType "name"),
    ArrayType (ScalarType "numeric"),
    ArrayType (ScalarType "oid"),
    ArrayType (ScalarType "oidvector"),
    ArrayType (ScalarType "path"),
    ArrayType (ScalarType "point"),
    ArrayType (ScalarType "polygon"),
    ArrayType (ScalarType "refcursor"),
    ArrayType (ScalarType "regclass"),
    ArrayType (ScalarType "regconfig"),
    ArrayType (ScalarType "regdictionary"),
    ArrayType (ScalarType "regoper"),
    ArrayType (ScalarType "regoperator"),
    ArrayType (ScalarType "regproc"),
    ArrayType (ScalarType "regprocedure"),
    ArrayType (ScalarType "regtype"),
    ArrayType (ScalarType "reltime"),
    ArrayType (ScalarType "text"),
    ArrayType (ScalarType "tid"),
    ArrayType (ScalarType "time"),
    ArrayType (ScalarType "timestamp"),
    ArrayType (ScalarType "timestamptz"),
    ArrayType (ScalarType "timetz"),
    ArrayType (ScalarType "tinterval"),
    ArrayType (ScalarType "tsquery"),
    ArrayType (ScalarType "tsvector"),
    ArrayType (ScalarType "txid_snapshot"),
    ArrayType (ScalarType "uuid"),
    ArrayType (ScalarType "varbit"),
    ArrayType (ScalarType "varchar"),
    ArrayType (ScalarType "xid"),
    ArrayType (ScalarType "xml"),
    CompositeType "pg_aggregate",
    CompositeType "pg_am",
    CompositeType "pg_amop",
    CompositeType "pg_amproc",
    CompositeType "pg_attrdef",
    CompositeType "pg_attribute",
    CompositeType "pg_authid",
    CompositeType "pg_auth_members",
    CompositeType "pg_cast",
    CompositeType "pg_class",
    CompositeType "pg_constraint",
    CompositeType "pg_conversion",
    CompositeType "pg_cursors",
    CompositeType "pg_database",
    CompositeType "pg_depend",
    CompositeType "pg_description",
    CompositeType "pg_enum",
    CompositeType "pg_foreign_data_wrapper",
    CompositeType "pg_foreign_server",
    CompositeType "pg_group",
    CompositeType "pg_index",
    CompositeType "pg_indexes",
    CompositeType "pg_inherits",
    CompositeType "pg_language",
    CompositeType "pg_largeobject",
    CompositeType "pg_listener",
    CompositeType "pg_locks",
    CompositeType "pg_namespace",
    CompositeType "pg_opclass",
    CompositeType "pg_operator",
    CompositeType "pg_opfamily",
    CompositeType "pg_pltemplate",
    CompositeType "pg_prepared_statements",
    CompositeType "pg_prepared_xacts",
    CompositeType "pg_proc",
    CompositeType "pg_rewrite",
    CompositeType "pg_roles",
    CompositeType "pg_rules",
    CompositeType "pg_settings",
    CompositeType "pg_shadow",
    CompositeType "pg_shdepend",
    CompositeType "pg_shdescription",
    CompositeType "pg_stat_activity",
    CompositeType "pg_stat_all_indexes",
    CompositeType "pg_stat_all_tables",
    CompositeType "pg_stat_bgwriter",
    CompositeType "pg_stat_database",
    CompositeType "pg_statio_all_indexes",
    CompositeType "pg_statio_all_sequences",
    CompositeType "pg_statio_all_tables",
    CompositeType "pg_statio_sys_indexes",
    CompositeType "pg_statio_sys_sequences",
    CompositeType "pg_statio_sys_tables",
    CompositeType "pg_statio_user_indexes",
    CompositeType "pg_statio_user_sequences",
    CompositeType "pg_statio_user_tables",
    CompositeType "pg_statistic",
    CompositeType "pg_stats",
    CompositeType "pg_stat_sys_indexes",
    CompositeType "pg_stat_sys_tables",
    CompositeType "pg_stat_user_functions",
    CompositeType "pg_stat_user_indexes",
    CompositeType "pg_stat_user_tables",
    CompositeType "pg_tables",
    CompositeType "pg_tablespace",
    CompositeType "pg_timezone_abbrevs",
    CompositeType "pg_timezone_names",
    CompositeType "pg_trigger",
    CompositeType "pg_ts_config",
    CompositeType "pg_ts_config_map",
    CompositeType "pg_ts_dict",
    CompositeType "pg_ts_parser",
    CompositeType "pg_ts_template",
    CompositeType "pg_type",
    CompositeType "pg_user",
    CompositeType "pg_user_mapping",
    CompositeType "pg_user_mappings",
    CompositeType "pg_views",
    Pseudo Any,
    Pseudo AnyArray,
    Pseudo AnyElement,
    Pseudo AnyEnum,
    Pseudo AnyNonArray,
    Pseudo Cstring,
    Pseudo Internal,
    Pseudo LanguageHandler,
    Pseudo Opaque,
    Pseudo Record,
    Pseudo Trigger,
    Pseudo Void,
    ScalarType "abstime",
    ScalarType "aclitem",
    ScalarType "bit",
    ScalarType "bool",
    ScalarType "box",
    ScalarType "bpchar",
    ScalarType "bytea",
    ScalarType "char",
    ScalarType "cid",
    ScalarType "cidr",
    ScalarType "circle",
    ScalarType "date",
    ScalarType "float4",
    ScalarType "float8",
    ScalarType "gtsvector",
    ScalarType "inet",
    ScalarType "int2",
    ScalarType "int2vector",
    ScalarType "int4",
    ScalarType "int8",
    ScalarType "interval",
    ScalarType "line",
    ScalarType "lseg",
    ScalarType "macaddr",
    ScalarType "money",
    ScalarType "name",
    ScalarType "numeric",
    ScalarType "oid",
    ScalarType "oidvector",
    ScalarType "path",
    ScalarType "point",
    ScalarType "polygon",
    ScalarType "refcursor",
    ScalarType "regclass",
    ScalarType "regconfig",
    ScalarType "regdictionary",
    ScalarType "regoper",
    ScalarType "regoperator",
    ScalarType "regproc",
    ScalarType "regprocedure",
    ScalarType "regtype",
    ScalarType "reltime",
    ScalarType "smgr",
    ScalarType "text",
    ScalarType "tid",
    ScalarType "time",
    ScalarType "timestamp",
    ScalarType "timestamptz",
    ScalarType "timetz",
    ScalarType "tinterval",
    ScalarType "tsquery",
    ScalarType "tsvector",
    ScalarType "txid_snapshot",
    ScalarType "unknown",
    ScalarType "uuid",
    ScalarType "varbit",
    ScalarType "varchar",
    ScalarType "xid",
    ScalarType "xml"]


binaryOperatorTypes = [
    ("!~", [ScalarType "bpchar",ScalarType "text"], ScalarType "bool"),
    ("!~", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("!~", [ScalarType "name",ScalarType "text"], ScalarType "bool"),
    ("!~*", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("!~*", [ScalarType "name",ScalarType "text"], ScalarType "bool"),
    ("!~*", [ScalarType "bpchar",ScalarType "text"], ScalarType "bool"),
    ("!~~", [ScalarType "bpchar",ScalarType "text"], ScalarType "bool"),
    ("!~~", [ScalarType "bytea",ScalarType "bytea"], ScalarType "bool"),
    ("!~~", [ScalarType "name",ScalarType "text"], ScalarType "bool"),
    ("!~~", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("!~~*", [ScalarType "name",ScalarType "text"], ScalarType "bool"),
    ("!~~*", [ScalarType "bpchar",ScalarType "text"], ScalarType "bool"),
    ("!~~*", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("#", [ScalarType "int8",ScalarType "int8"], ScalarType "int8"),
    ("#", [ScalarType "bit",ScalarType "bit"], ScalarType "bit"),
    ("#", [ScalarType "line",ScalarType "line"], ScalarType "point"),
    ("#", [ScalarType "box",ScalarType "box"], ScalarType "box"),
    ("#", [ScalarType "int2",ScalarType "int2"], ScalarType "int2"),
    ("#", [ScalarType "lseg",ScalarType "lseg"], ScalarType "point"),
    ("#", [ScalarType "int4",ScalarType "int4"], ScalarType "int4"),
    ("##", [ScalarType "lseg",ScalarType "box"], ScalarType "point"),
    ("##", [ScalarType "point",ScalarType "lseg"], ScalarType "point"),
    ("##", [ScalarType "point",ScalarType "box"], ScalarType "point"),
    ("##", [ScalarType "lseg",ScalarType "lseg"], ScalarType "point"),
    ("##", [ScalarType "point",ScalarType "line"], ScalarType "point"),
    ("##", [ScalarType "lseg",ScalarType "line"], ScalarType "point"),
    ("##", [ScalarType "line",ScalarType "box"], ScalarType "point"),
    ("##", [ScalarType "line",ScalarType "lseg"], ScalarType "point"),
    ("#<", [ScalarType "tinterval",ScalarType "reltime"], ScalarType "bool"),
    ("#<=", [ScalarType "tinterval",ScalarType "reltime"], ScalarType "bool"),
    ("#<>", [ScalarType "tinterval",ScalarType "reltime"], ScalarType "bool"),
    ("#=", [ScalarType "tinterval",ScalarType "reltime"], ScalarType "bool"),
    ("#>", [ScalarType "tinterval",ScalarType "reltime"], ScalarType "bool"),
    ("#>=", [ScalarType "tinterval",ScalarType "reltime"], ScalarType "bool"),
    ("%", [ScalarType "int8",ScalarType "int8"], ScalarType "int8"),
    ("%", [ScalarType "int4",ScalarType "int4"], ScalarType "int4"),
    ("%", [ScalarType "int2",ScalarType "int2"], ScalarType "int2"),
    ("%", [ScalarType "numeric",ScalarType "numeric"], ScalarType "numeric"),
    ("&", [ScalarType "int2",ScalarType "int2"], ScalarType "int2"),
    ("&", [ScalarType "int4",ScalarType "int4"], ScalarType "int4"),
    ("&", [ScalarType "int8",ScalarType "int8"], ScalarType "int8"),
    ("&", [ScalarType "bit",ScalarType "bit"], ScalarType "bit"),
    ("&", [ScalarType "inet",ScalarType "inet"], ScalarType "inet"),
    ("&&", [ScalarType "circle",ScalarType "circle"], ScalarType "bool"),
    ("&&", [ScalarType "tinterval",ScalarType "tinterval"], ScalarType "bool"),
    ("&&", [ScalarType "polygon",ScalarType "polygon"], ScalarType "bool"),
    ("&&", [ScalarType "tsquery",ScalarType "tsquery"], ScalarType "tsquery"),
    ("&&", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    ("&&", [Pseudo AnyArray,Pseudo AnyArray], ScalarType "bool"),
    ("&<", [ScalarType "circle",ScalarType "circle"], ScalarType "bool"),
    ("&<", [ScalarType "polygon",ScalarType "polygon"], ScalarType "bool"),
    ("&<", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    ("&<|", [ScalarType "circle",ScalarType "circle"], ScalarType "bool"),
    ("&<|", [ScalarType "polygon",ScalarType "polygon"], ScalarType "bool"),
    ("&<|", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    ("&>", [ScalarType "circle",ScalarType "circle"], ScalarType "bool"),
    ("&>", [ScalarType "polygon",ScalarType "polygon"], ScalarType "bool"),
    ("&>", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    ("*", [ScalarType "int4",ScalarType "int2"], ScalarType "int4"),
    ("*", [ScalarType "int8",ScalarType "int8"], ScalarType "int8"),
    ("*", [ScalarType "money",ScalarType "float4"], ScalarType "money"),
    ("*", [ScalarType "int2",ScalarType "int8"], ScalarType "int8"),
    ("*", [ScalarType "int4",ScalarType "money"], ScalarType "money"),
    ("*", [ScalarType "float8",ScalarType "float4"], ScalarType "float8"),
    ("*", [ScalarType "path",ScalarType "point"], ScalarType "path"),
    ("*", [ScalarType "money",ScalarType "int4"], ScalarType "money"),
    ("*", [ScalarType "float8",ScalarType "money"], ScalarType "money"),
    ("*", [ScalarType "float8",ScalarType "interval"], ScalarType "interval"),
    ("*", [ScalarType "int4",ScalarType "int4"], ScalarType "int4"),
    ("*", [ScalarType "float4",ScalarType "float8"], ScalarType "float8"),
    ("*", [ScalarType "float4",ScalarType "float4"], ScalarType "float4"),
    ("*", [ScalarType "int8",ScalarType "int4"], ScalarType "int8"),
    ("*", [ScalarType "int2",ScalarType "money"], ScalarType "money"),
    ("*", [ScalarType "money",ScalarType "int2"], ScalarType "money"),
    ("*", [ScalarType "point",ScalarType "point"], ScalarType "point"),
    ("*", [ScalarType "circle",ScalarType "point"], ScalarType "circle"),
    ("*", [ScalarType "int4",ScalarType "int8"], ScalarType "int8"),
    ("*", [ScalarType "int2",ScalarType "int2"], ScalarType "int2"),
    ("*", [ScalarType "interval",ScalarType "float8"], ScalarType "interval"),
    ("*", [ScalarType "float4",ScalarType "money"], ScalarType "money"),
    ("*", [ScalarType "box",ScalarType "point"], ScalarType "box"),
    ("*", [ScalarType "int2",ScalarType "int4"], ScalarType "int4"),
    ("*", [ScalarType "float8",ScalarType "float8"], ScalarType "float8"),
    ("*", [ScalarType "numeric",ScalarType "numeric"], ScalarType "numeric"),
    ("*", [ScalarType "int8",ScalarType "int2"], ScalarType "int8"),
    ("*", [ScalarType "money",ScalarType "float8"], ScalarType "money"),
    ("+", [ScalarType "float4",ScalarType "float4"], ScalarType "float4"),
    ("+", [ScalarType "path",ScalarType "point"], ScalarType "path"),
    ("+", [ScalarType "path",ScalarType "path"], ScalarType "path"),
    ("+", [ScalarType "point",ScalarType "point"], ScalarType "point"),
    ("+", [ScalarType "int8",ScalarType "int8"], ScalarType "int8"),
    ("+", [ScalarType "int8",ScalarType "int4"], ScalarType "int8"),
    ("+", [ScalarType "int4",ScalarType "int8"], ScalarType "int8"),
    ("+", [ScalarType "int8",ScalarType "int2"], ScalarType "int8"),
    ("+", [ScalarType "int2",ScalarType "int8"], ScalarType "int8"),
    ("+", [ScalarType "date",ScalarType "timetz"], ScalarType "timestamptz"),
    ("+", [ScalarType "date",ScalarType "time"], ScalarType "timestamp"),
    ("+", [ScalarType "inet",ScalarType "int8"], ScalarType "inet"),
    ("+", [ScalarType "interval",ScalarType "interval"], ScalarType "interval"),
    ("+", [ScalarType "timestamptz",ScalarType "interval"], ScalarType "timestamptz"),
    ("+", [ScalarType "int8",ScalarType "inet"], ScalarType "inet"),
    ("+", [ScalarType "numeric",ScalarType "numeric"], ScalarType "numeric"),
    ("+", [ScalarType "float8",ScalarType "float4"], ScalarType "float8"),
    ("+", [ScalarType "int4",ScalarType "date"], ScalarType "date"),
    ("+", [ScalarType "interval",ScalarType "timestamptz"], ScalarType "timestamptz"),
    ("+", [ScalarType "float4",ScalarType "float8"], ScalarType "float8"),
    ("+", [ScalarType "interval",ScalarType "timestamp"], ScalarType "timestamp"),
    ("+", [ScalarType "interval",ScalarType "timetz"], ScalarType "timetz"),
    ("+", [ScalarType "time",ScalarType "interval"], ScalarType "time"),
    ("+", [ScalarType "interval",ScalarType "date"], ScalarType "timestamp"),
    ("+", [ScalarType "timetz",ScalarType "interval"], ScalarType "timetz"),
    ("+", [ScalarType "date",ScalarType "int4"], ScalarType "date"),
    ("+", [ScalarType "int2",ScalarType "int2"], ScalarType "int2"),
    ("+", [ScalarType "int4",ScalarType "int4"], ScalarType "int4"),
    ("+", [ScalarType "int2",ScalarType "int4"], ScalarType "int4"),
    ("+", [ScalarType "int4",ScalarType "int2"], ScalarType "int4"),
    ("+", [ScalarType "date",ScalarType "interval"], ScalarType "timestamp"),
    ("+", [ScalarType "circle",ScalarType "point"], ScalarType "circle"),
    ("+", [ScalarType "interval",ScalarType "time"], ScalarType "time"),
    ("+", [ScalarType "timetz",ScalarType "date"], ScalarType "timestamptz"),
    ("+", [ArrayType (ScalarType "aclitem"),ScalarType "aclitem"], ArrayType (ScalarType "aclitem")),
    ("+", [ScalarType "abstime",ScalarType "reltime"], ScalarType "abstime"),
    ("+", [ScalarType "timestamp",ScalarType "interval"], ScalarType "timestamp"),
    ("+", [ScalarType "float8",ScalarType "float8"], ScalarType "float8"),
    ("+", [ScalarType "time",ScalarType "date"], ScalarType "timestamp"),
    ("+", [ScalarType "money",ScalarType "money"], ScalarType "money"),
    ("+", [ScalarType "box",ScalarType "point"], ScalarType "box"),
    ("-", [ScalarType "int4",ScalarType "int4"], ScalarType "int4"),
    ("-", [ScalarType "float8",ScalarType "float8"], ScalarType "float8"),
    ("-", [ScalarType "float4",ScalarType "float8"], ScalarType "float8"),
    ("-", [ScalarType "float8",ScalarType "float4"], ScalarType "float8"),
    ("-", [ScalarType "money",ScalarType "money"], ScalarType "money"),
    ("-", [ScalarType "inet",ScalarType "int8"], ScalarType "inet"),
    ("-", [ScalarType "path",ScalarType "point"], ScalarType "path"),
    ("-", [ScalarType "numeric",ScalarType "numeric"], ScalarType "numeric"),
    ("-", [ScalarType "timestamptz",ScalarType "timestamptz"], ScalarType "interval"),
    ("-", [ScalarType "timestamptz",ScalarType "interval"], ScalarType "timestamptz"),
    ("-", [ScalarType "interval",ScalarType "interval"], ScalarType "interval"),
    ("-", [ScalarType "int2",ScalarType "int8"], ScalarType "int8"),
    ("-", [ScalarType "box",ScalarType "point"], ScalarType "box"),
    ("-", [ScalarType "timestamp",ScalarType "timestamp"], ScalarType "interval"),
    ("-", [ScalarType "int8",ScalarType "int2"], ScalarType "int8"),
    ("-", [ScalarType "int4",ScalarType "int8"], ScalarType "int8"),
    ("-", [ScalarType "int8",ScalarType "int4"], ScalarType "int8"),
    ("-", [ScalarType "date",ScalarType "interval"], ScalarType "timestamp"),
    ("-", [ScalarType "int4",ScalarType "int2"], ScalarType "int4"),
    ("-", [ScalarType "time",ScalarType "time"], ScalarType "interval"),
    ("-", [ArrayType (ScalarType "aclitem"),ScalarType "aclitem"], ArrayType (ScalarType "aclitem")),
    ("-", [ScalarType "int8",ScalarType "int8"], ScalarType "int8"),
    ("-", [ScalarType "int2",ScalarType "int4"], ScalarType "int4"),
    ("-", [ScalarType "int2",ScalarType "int2"], ScalarType "int2"),
    ("-", [ScalarType "timetz",ScalarType "interval"], ScalarType "timetz"),
    ("-", [ScalarType "date",ScalarType "date"], ScalarType "int4"),
    ("-", [ScalarType "date",ScalarType "int4"], ScalarType "date"),
    ("-", [ScalarType "time",ScalarType "interval"], ScalarType "time"),
    ("-", [ScalarType "point",ScalarType "point"], ScalarType "point"),
    ("-", [ScalarType "abstime",ScalarType "reltime"], ScalarType "abstime"),
    ("-", [ScalarType "circle",ScalarType "point"], ScalarType "circle"),
    ("-", [ScalarType "float4",ScalarType "float4"], ScalarType "float4"),
    ("-", [ScalarType "inet",ScalarType "inet"], ScalarType "int8"),
    ("-", [ScalarType "timestamp",ScalarType "interval"], ScalarType "timestamp"),
    ("/", [ScalarType "int4",ScalarType "int2"], ScalarType "int4"),
    ("/", [ScalarType "int2",ScalarType "int4"], ScalarType "int4"),
    ("/", [ScalarType "circle",ScalarType "point"], ScalarType "circle"),
    ("/", [ScalarType "money",ScalarType "float8"], ScalarType "money"),
    ("/", [ScalarType "float8",ScalarType "float4"], ScalarType "float8"),
    ("/", [ScalarType "numeric",ScalarType "numeric"], ScalarType "numeric"),
    ("/", [ScalarType "box",ScalarType "point"], ScalarType "box"),
    ("/", [ScalarType "int2",ScalarType "int8"], ScalarType "int8"),
    ("/", [ScalarType "int4",ScalarType "int8"], ScalarType "int8"),
    ("/", [ScalarType "interval",ScalarType "float8"], ScalarType "interval"),
    ("/", [ScalarType "path",ScalarType "point"], ScalarType "path"),
    ("/", [ScalarType "float4",ScalarType "float4"], ScalarType "float4"),
    ("/", [ScalarType "money",ScalarType "float4"], ScalarType "money"),
    ("/", [ScalarType "int8",ScalarType "int4"], ScalarType "int8"),
    ("/", [ScalarType "int8",ScalarType "int2"], ScalarType "int8"),
    ("/", [ScalarType "float8",ScalarType "float8"], ScalarType "float8"),
    ("/", [ScalarType "int4",ScalarType "int4"], ScalarType "int4"),
    ("/", [ScalarType "money",ScalarType "int2"], ScalarType "money"),
    ("/", [ScalarType "int2",ScalarType "int2"], ScalarType "int2"),
    ("/", [ScalarType "int8",ScalarType "int8"], ScalarType "int8"),
    ("/", [ScalarType "point",ScalarType "point"], ScalarType "point"),
    ("/", [ScalarType "float4",ScalarType "float8"], ScalarType "float8"),
    ("/", [ScalarType "money",ScalarType "int4"], ScalarType "money"),
    ("<", [Pseudo AnyArray,Pseudo AnyArray], ScalarType "bool"),
    ("<", [ScalarType "tsvector",ScalarType "tsvector"], ScalarType "bool"),
    ("<", [ScalarType "inet",ScalarType "inet"], ScalarType "bool"),
    ("<", [ScalarType "lseg",ScalarType "lseg"], ScalarType "bool"),
    ("<", [ScalarType "macaddr",ScalarType "macaddr"], ScalarType "bool"),
    ("<", [ScalarType "int8",ScalarType "int4"], ScalarType "bool"),
    ("<", [Pseudo AnyEnum,Pseudo AnyEnum], ScalarType "bool"),
    ("<", [ScalarType "uuid",ScalarType "uuid"], ScalarType "bool"),
    ("<", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    ("<", [ScalarType "int2",ScalarType "int2"], ScalarType "bool"),
    ("<", [Pseudo Record,Pseudo Record], ScalarType "bool"),
    ("<", [ScalarType "int2",ScalarType "int4"], ScalarType "bool"),
    ("<", [ScalarType "int4",ScalarType "int2"], ScalarType "bool"),
    ("<", [ScalarType "timestamptz",ScalarType "timestamp"], ScalarType "bool"),
    ("<", [ScalarType "int4",ScalarType "int4"], ScalarType "bool"),
    ("<", [ScalarType "abstime",ScalarType "abstime"], ScalarType "bool"),
    ("<", [ScalarType "bool",ScalarType "bool"], ScalarType "bool"),
    ("<", [ScalarType "reltime",ScalarType "reltime"], ScalarType "bool"),
    ("<", [ScalarType "timestamp",ScalarType "timestamptz"], ScalarType "bool"),
    ("<", [ScalarType "oid",ScalarType "oid"], ScalarType "bool"),
    ("<", [ScalarType "timestamptz",ScalarType "date"], ScalarType "bool"),
    ("<", [ScalarType "oidvector",ScalarType "oidvector"], ScalarType "bool"),
    ("<", [ScalarType "timestamp",ScalarType "date"], ScalarType "bool"),
    ("<", [ScalarType "float4",ScalarType "float4"], ScalarType "bool"),
    ("<", [ScalarType "char",ScalarType "char"], ScalarType "bool"),
    ("<", [ScalarType "date",ScalarType "timestamptz"], ScalarType "bool"),
    ("<", [ScalarType "date",ScalarType "timestamp"], ScalarType "bool"),
    ("<", [ScalarType "name",ScalarType "name"], ScalarType "bool"),
    ("<", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("<", [ScalarType "timestamp",ScalarType "timestamp"], ScalarType "bool"),
    ("<", [ScalarType "float8",ScalarType "float8"], ScalarType "bool"),
    ("<", [ScalarType "bytea",ScalarType "bytea"], ScalarType "bool"),
    ("<", [ScalarType "path",ScalarType "path"], ScalarType "bool"),
    ("<", [ScalarType "tinterval",ScalarType "tinterval"], ScalarType "bool"),
    ("<", [ScalarType "money",ScalarType "money"], ScalarType "bool"),
    ("<", [ScalarType "int8",ScalarType "int2"], ScalarType "bool"),
    ("<", [ScalarType "int2",ScalarType "int8"], ScalarType "bool"),
    ("<", [ScalarType "bpchar",ScalarType "bpchar"], ScalarType "bool"),
    ("<", [ScalarType "tid",ScalarType "tid"], ScalarType "bool"),
    ("<", [ScalarType "varbit",ScalarType "varbit"], ScalarType "bool"),
    ("<", [ScalarType "tsquery",ScalarType "tsquery"], ScalarType "bool"),
    ("<", [ScalarType "date",ScalarType "date"], ScalarType "bool"),
    ("<", [ScalarType "time",ScalarType "time"], ScalarType "bool"),
    ("<", [ScalarType "timetz",ScalarType "timetz"], ScalarType "bool"),
    ("<", [ScalarType "int4",ScalarType "int8"], ScalarType "bool"),
    ("<", [ScalarType "float4",ScalarType "float8"], ScalarType "bool"),
    ("<", [ScalarType "float8",ScalarType "float4"], ScalarType "bool"),
    ("<", [ScalarType "bit",ScalarType "bit"], ScalarType "bool"),
    ("<", [ScalarType "int8",ScalarType "int8"], ScalarType "bool"),
    ("<", [ScalarType "timestamptz",ScalarType "timestamptz"], ScalarType "bool"),
    ("<", [ScalarType "numeric",ScalarType "numeric"], ScalarType "bool"),
    ("<", [ScalarType "interval",ScalarType "interval"], ScalarType "bool"),
    ("<", [ScalarType "circle",ScalarType "circle"], ScalarType "bool"),
    ("<#>", [ScalarType "abstime",ScalarType "abstime"], ScalarType "tinterval"),
    ("<->", [ScalarType "point",ScalarType "box"], ScalarType "float8"),
    ("<->", [ScalarType "lseg",ScalarType "line"], ScalarType "float8"),
    ("<->", [ScalarType "line",ScalarType "line"], ScalarType "float8"),
    ("<->", [ScalarType "polygon",ScalarType "polygon"], ScalarType "float8"),
    ("<->", [ScalarType "path",ScalarType "path"], ScalarType "float8"),
    ("<->", [ScalarType "box",ScalarType "box"], ScalarType "float8"),
    ("<->", [ScalarType "circle",ScalarType "circle"], ScalarType "float8"),
    ("<->", [ScalarType "point",ScalarType "circle"], ScalarType "float8"),
    ("<->", [ScalarType "circle",ScalarType "polygon"], ScalarType "float8"),
    ("<->", [ScalarType "line",ScalarType "box"], ScalarType "float8"),
    ("<->", [ScalarType "point",ScalarType "point"], ScalarType "float8"),
    ("<->", [ScalarType "point",ScalarType "path"], ScalarType "float8"),
    ("<->", [ScalarType "lseg",ScalarType "lseg"], ScalarType "float8"),
    ("<->", [ScalarType "point",ScalarType "line"], ScalarType "float8"),
    ("<->", [ScalarType "lseg",ScalarType "box"], ScalarType "float8"),
    ("<->", [ScalarType "point",ScalarType "lseg"], ScalarType "float8"),
    ("<<", [ScalarType "tinterval",ScalarType "tinterval"], ScalarType "bool"),
    ("<<", [ScalarType "int4",ScalarType "int4"], ScalarType "int4"),
    ("<<", [ScalarType "int2",ScalarType "int4"], ScalarType "int2"),
    ("<<", [ScalarType "int8",ScalarType "int4"], ScalarType "int8"),
    ("<<", [ScalarType "circle",ScalarType "circle"], ScalarType "bool"),
    ("<<", [ScalarType "bit",ScalarType "int4"], ScalarType "bit"),
    ("<<", [ScalarType "inet",ScalarType "inet"], ScalarType "bool"),
    ("<<", [ScalarType "point",ScalarType "point"], ScalarType "bool"),
    ("<<", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    ("<<", [ScalarType "polygon",ScalarType "polygon"], ScalarType "bool"),
    ("<<=", [ScalarType "inet",ScalarType "inet"], ScalarType "bool"),
    ("<<|", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    ("<<|", [ScalarType "circle",ScalarType "circle"], ScalarType "bool"),
    ("<<|", [ScalarType "polygon",ScalarType "polygon"], ScalarType "bool"),
    ("<=", [Pseudo AnyEnum,Pseudo AnyEnum], ScalarType "bool"),
    ("<=", [ScalarType "oidvector",ScalarType "oidvector"], ScalarType "bool"),
    ("<=", [ScalarType "timestamp",ScalarType "date"], ScalarType "bool"),
    ("<=", [ScalarType "oid",ScalarType "oid"], ScalarType "bool"),
    ("<=", [ScalarType "bytea",ScalarType "bytea"], ScalarType "bool"),
    ("<=", [ScalarType "timestamptz",ScalarType "date"], ScalarType "bool"),
    ("<=", [ScalarType "reltime",ScalarType "reltime"], ScalarType "bool"),
    ("<=", [ScalarType "int2",ScalarType "int8"], ScalarType "bool"),
    ("<=", [ScalarType "int8",ScalarType "int8"], ScalarType "bool"),
    ("<=", [ScalarType "bpchar",ScalarType "bpchar"], ScalarType "bool"),
    ("<=", [ScalarType "timestamp",ScalarType "timestamptz"], ScalarType "bool"),
    ("<=", [ScalarType "abstime",ScalarType "abstime"], ScalarType "bool"),
    ("<=", [ScalarType "varbit",ScalarType "varbit"], ScalarType "bool"),
    ("<=", [Pseudo AnyArray,Pseudo AnyArray], ScalarType "bool"),
    ("<=", [ScalarType "circle",ScalarType "circle"], ScalarType "bool"),
    ("<=", [ScalarType "date",ScalarType "date"], ScalarType "bool"),
    ("<=", [ScalarType "timestamptz",ScalarType "timestamp"], ScalarType "bool"),
    ("<=", [ScalarType "int4",ScalarType "int2"], ScalarType "bool"),
    ("<=", [ScalarType "int2",ScalarType "int4"], ScalarType "bool"),
    ("<=", [ScalarType "tid",ScalarType "tid"], ScalarType "bool"),
    ("<=", [ScalarType "time",ScalarType "time"], ScalarType "bool"),
    ("<=", [ScalarType "timetz",ScalarType "timetz"], ScalarType "bool"),
    ("<=", [ScalarType "macaddr",ScalarType "macaddr"], ScalarType "bool"),
    ("<=", [ScalarType "int4",ScalarType "int4"], ScalarType "bool"),
    ("<=", [ScalarType "int2",ScalarType "int2"], ScalarType "bool"),
    ("<=", [ScalarType "inet",ScalarType "inet"], ScalarType "bool"),
    ("<=", [ScalarType "tsquery",ScalarType "tsquery"], ScalarType "bool"),
    ("<=", [ScalarType "float4",ScalarType "float8"], ScalarType "bool"),
    ("<=", [ScalarType "bit",ScalarType "bit"], ScalarType "bool"),
    ("<=", [ScalarType "float8",ScalarType "float4"], ScalarType "bool"),
    ("<=", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    ("<=", [ScalarType "int4",ScalarType "int8"], ScalarType "bool"),
    ("<=", [ScalarType "uuid",ScalarType "uuid"], ScalarType "bool"),
    ("<=", [ScalarType "lseg",ScalarType "lseg"], ScalarType "bool"),
    ("<=", [ScalarType "timestamptz",ScalarType "timestamptz"], ScalarType "bool"),
    ("<=", [ScalarType "numeric",ScalarType "numeric"], ScalarType "bool"),
    ("<=", [ScalarType "float4",ScalarType "float4"], ScalarType "bool"),
    ("<=", [ScalarType "int8",ScalarType "int4"], ScalarType "bool"),
    ("<=", [ScalarType "money",ScalarType "money"], ScalarType "bool"),
    ("<=", [ScalarType "tsvector",ScalarType "tsvector"], ScalarType "bool"),
    ("<=", [ScalarType "date",ScalarType "timestamptz"], ScalarType "bool"),
    ("<=", [ScalarType "interval",ScalarType "interval"], ScalarType "bool"),
    ("<=", [Pseudo Record,Pseudo Record], ScalarType "bool"),
    ("<=", [ScalarType "char",ScalarType "char"], ScalarType "bool"),
    ("<=", [ScalarType "path",ScalarType "path"], ScalarType "bool"),
    ("<=", [ScalarType "float8",ScalarType "float8"], ScalarType "bool"),
    ("<=", [ScalarType "timestamp",ScalarType "timestamp"], ScalarType "bool"),
    ("<=", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("<=", [ScalarType "name",ScalarType "name"], ScalarType "bool"),
    ("<=", [ScalarType "date",ScalarType "timestamp"], ScalarType "bool"),
    ("<=", [ScalarType "tinterval",ScalarType "tinterval"], ScalarType "bool"),
    ("<=", [ScalarType "bool",ScalarType "bool"], ScalarType "bool"),
    ("<=", [ScalarType "int8",ScalarType "int2"], ScalarType "bool"),
    ("<>", [ScalarType "float8",ScalarType "float8"], ScalarType "bool"),
    ("<>", [ScalarType "int4",ScalarType "int4"], ScalarType "bool"),
    ("<>", [ScalarType "interval",ScalarType "interval"], ScalarType "bool"),
    ("<>", [ScalarType "float4",ScalarType "float8"], ScalarType "bool"),
    ("<>", [ScalarType "timestamptz",ScalarType "timestamptz"], ScalarType "bool"),
    ("<>", [ScalarType "date",ScalarType "timestamptz"], ScalarType "bool"),
    ("<>", [ScalarType "date",ScalarType "timestamp"], ScalarType "bool"),
    ("<>", [ScalarType "int4",ScalarType "int2"], ScalarType "bool"),
    ("<>", [ScalarType "name",ScalarType "name"], ScalarType "bool"),
    ("<>", [ScalarType "time",ScalarType "time"], ScalarType "bool"),
    ("<>", [ScalarType "inet",ScalarType "inet"], ScalarType "bool"),
    ("<>", [ScalarType "point",ScalarType "point"], ScalarType "bool"),
    ("<>", [ScalarType "int2",ScalarType "int4"], ScalarType "bool"),
    ("<>", [ScalarType "timestamptz",ScalarType "timestamp"], ScalarType "bool"),
    ("<>", [ScalarType "tsvector",ScalarType "tsvector"], ScalarType "bool"),
    ("<>", [ScalarType "timetz",ScalarType "timetz"], ScalarType "bool"),
    ("<>", [ScalarType "timestamp",ScalarType "timestamp"], ScalarType "bool"),
    ("<>", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("<>", [ScalarType "int8",ScalarType "int4"], ScalarType "bool"),
    ("<>", [ScalarType "int2",ScalarType "int2"], ScalarType "bool"),
    ("<>", [Pseudo Record,Pseudo Record], ScalarType "bool"),
    ("<>", [ScalarType "money",ScalarType "money"], ScalarType "bool"),
    ("<>", [ScalarType "oidvector",ScalarType "oidvector"], ScalarType "bool"),
    ("<>", [ScalarType "bytea",ScalarType "bytea"], ScalarType "bool"),
    ("<>", [ScalarType "int8",ScalarType "int8"], ScalarType "bool"),
    ("<>", [ScalarType "float8",ScalarType "float4"], ScalarType "bool"),
    ("<>", [ScalarType "tsquery",ScalarType "tsquery"], ScalarType "bool"),
    ("<>", [ScalarType "numeric",ScalarType "numeric"], ScalarType "bool"),
    ("<>", [ScalarType "lseg",ScalarType "lseg"], ScalarType "bool"),
    ("<>", [ScalarType "bit",ScalarType "bit"], ScalarType "bool"),
    ("<>", [ScalarType "oid",ScalarType "oid"], ScalarType "bool"),
    ("<>", [ScalarType "int4",ScalarType "int8"], ScalarType "bool"),
    ("<>", [ScalarType "macaddr",ScalarType "macaddr"], ScalarType "bool"),
    ("<>", [ScalarType "int8",ScalarType "int2"], ScalarType "bool"),
    ("<>", [ScalarType "timestamptz",ScalarType "date"], ScalarType "bool"),
    ("<>", [ScalarType "char",ScalarType "char"], ScalarType "bool"),
    ("<>", [ScalarType "tid",ScalarType "tid"], ScalarType "bool"),
    ("<>", [ScalarType "reltime",ScalarType "reltime"], ScalarType "bool"),
    ("<>", [ScalarType "bpchar",ScalarType "bpchar"], ScalarType "bool"),
    ("<>", [ScalarType "uuid",ScalarType "uuid"], ScalarType "bool"),
    ("<>", [ScalarType "int2",ScalarType "int8"], ScalarType "bool"),
    ("<>", [ScalarType "bool",ScalarType "bool"], ScalarType "bool"),
    ("<>", [ScalarType "circle",ScalarType "circle"], ScalarType "bool"),
    ("<>", [Pseudo AnyArray,Pseudo AnyArray], ScalarType "bool"),
    ("<>", [Pseudo AnyEnum,Pseudo AnyEnum], ScalarType "bool"),
    ("<>", [ScalarType "timestamp",ScalarType "date"], ScalarType "bool"),
    ("<>", [ScalarType "timestamp",ScalarType "timestamptz"], ScalarType "bool"),
    ("<>", [ScalarType "abstime",ScalarType "abstime"], ScalarType "bool"),
    ("<>", [ScalarType "date",ScalarType "date"], ScalarType "bool"),
    ("<>", [ScalarType "tinterval",ScalarType "tinterval"], ScalarType "bool"),
    ("<>", [ScalarType "float4",ScalarType "float4"], ScalarType "bool"),
    ("<>", [ScalarType "varbit",ScalarType "varbit"], ScalarType "bool"),
    ("<?>", [ScalarType "abstime",ScalarType "tinterval"], ScalarType "bool"),
    ("<@", [ScalarType "point",ScalarType "line"], ScalarType "bool"),
    ("<@", [ScalarType "point",ScalarType "path"], ScalarType "bool"),
    ("<@", [ScalarType "point",ScalarType "box"], ScalarType "bool"),
    ("<@", [ScalarType "lseg",ScalarType "line"], ScalarType "bool"),
    ("<@", [ScalarType "point",ScalarType "lseg"], ScalarType "bool"),
    ("<@", [ScalarType "lseg",ScalarType "box"], ScalarType "bool"),
    ("<@", [ScalarType "point",ScalarType "circle"], ScalarType "bool"),
    ("<@", [ScalarType "point",ScalarType "polygon"], ScalarType "bool"),
    ("<@", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    ("<@", [ScalarType "polygon",ScalarType "polygon"], ScalarType "bool"),
    ("<@", [Pseudo AnyArray,Pseudo AnyArray], ScalarType "bool"),
    ("<@", [ScalarType "tsquery",ScalarType "tsquery"], ScalarType "bool"),
    ("<@", [ScalarType "circle",ScalarType "circle"], ScalarType "bool"),
    ("<^", [ScalarType "point",ScalarType "point"], ScalarType "bool"),
    ("<^", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    ("=", [ScalarType "int4",ScalarType "int8"], ScalarType "bool"),
    ("=", [ScalarType "bool",ScalarType "bool"], ScalarType "bool"),
    ("=", [ScalarType "char",ScalarType "char"], ScalarType "bool"),
    ("=", [ScalarType "name",ScalarType "name"], ScalarType "bool"),
    ("=", [ScalarType "int2",ScalarType "int2"], ScalarType "bool"),
    ("=", [ScalarType "int4",ScalarType "int4"], ScalarType "bool"),
    ("=", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("=", [ScalarType "xid",ScalarType "xid"], ScalarType "bool"),
    ("=", [ScalarType "xid",ScalarType "int4"], ScalarType "bool"),
    ("=", [ScalarType "cid",ScalarType "cid"], ScalarType "bool"),
    ("=", [ScalarType "int2vector",ScalarType "int2vector"], ScalarType "bool"),
    ("=", [ScalarType "tid",ScalarType "tid"], ScalarType "bool"),
    ("=", [ScalarType "int8",ScalarType "int8"], ScalarType "bool"),
    ("=", [ScalarType "int8",ScalarType "int4"], ScalarType "bool"),
    ("=", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    ("=", [ScalarType "int2",ScalarType "int4"], ScalarType "bool"),
    ("=", [ScalarType "int4",ScalarType "int2"], ScalarType "bool"),
    ("=", [ScalarType "abstime",ScalarType "abstime"], ScalarType "bool"),
    ("=", [ScalarType "reltime",ScalarType "reltime"], ScalarType "bool"),
    ("=", [ScalarType "oid",ScalarType "oid"], ScalarType "bool"),
    ("=", [ScalarType "oidvector",ScalarType "oidvector"], ScalarType "bool"),
    ("=", [ScalarType "float4",ScalarType "float4"], ScalarType "bool"),
    ("=", [ScalarType "float8",ScalarType "float8"], ScalarType "bool"),
    ("=", [ScalarType "path",ScalarType "path"], ScalarType "bool"),
    ("=", [ScalarType "tinterval",ScalarType "tinterval"], ScalarType "bool"),
    ("=", [ScalarType "money",ScalarType "money"], ScalarType "bool"),
    ("=", [ScalarType "aclitem",ScalarType "aclitem"], ScalarType "bool"),
    ("=", [ScalarType "bpchar",ScalarType "bpchar"], ScalarType "bool"),
    ("=", [Pseudo AnyArray,Pseudo AnyArray], ScalarType "bool"),
    ("=", [ScalarType "date",ScalarType "date"], ScalarType "bool"),
    ("=", [ScalarType "time",ScalarType "time"], ScalarType "bool"),
    ("=", [ScalarType "timetz",ScalarType "timetz"], ScalarType "bool"),
    ("=", [ScalarType "float4",ScalarType "float8"], ScalarType "bool"),
    ("=", [ScalarType "float8",ScalarType "float4"], ScalarType "bool"),
    ("=", [ScalarType "timestamptz",ScalarType "timestamptz"], ScalarType "bool"),
    ("=", [ScalarType "interval",ScalarType "interval"], ScalarType "bool"),
    ("=", [ScalarType "circle",ScalarType "circle"], ScalarType "bool"),
    ("=", [ScalarType "lseg",ScalarType "lseg"], ScalarType "bool"),
    ("=", [ScalarType "line",ScalarType "line"], ScalarType "bool"),
    ("=", [ScalarType "macaddr",ScalarType "macaddr"], ScalarType "bool"),
    ("=", [ScalarType "inet",ScalarType "inet"], ScalarType "bool"),
    ("=", [ScalarType "numeric",ScalarType "numeric"], ScalarType "bool"),
    ("=", [ScalarType "bit",ScalarType "bit"], ScalarType "bool"),
    ("=", [ScalarType "varbit",ScalarType "varbit"], ScalarType "bool"),
    ("=", [ScalarType "int2",ScalarType "int8"], ScalarType "bool"),
    ("=", [ScalarType "int8",ScalarType "int2"], ScalarType "bool"),
    ("=", [ScalarType "bytea",ScalarType "bytea"], ScalarType "bool"),
    ("=", [ScalarType "timestamp",ScalarType "timestamp"], ScalarType "bool"),
    ("=", [ScalarType "date",ScalarType "timestamp"], ScalarType "bool"),
    ("=", [ScalarType "date",ScalarType "timestamptz"], ScalarType "bool"),
    ("=", [ScalarType "timestamp",ScalarType "date"], ScalarType "bool"),
    ("=", [ScalarType "timestamptz",ScalarType "date"], ScalarType "bool"),
    ("=", [ScalarType "timestamp",ScalarType "timestamptz"], ScalarType "bool"),
    ("=", [ScalarType "timestamptz",ScalarType "timestamp"], ScalarType "bool"),
    ("=", [ScalarType "uuid",ScalarType "uuid"], ScalarType "bool"),
    ("=", [Pseudo AnyEnum,Pseudo AnyEnum], ScalarType "bool"),
    ("=", [ScalarType "tsvector",ScalarType "tsvector"], ScalarType "bool"),
    ("=", [ScalarType "tsquery",ScalarType "tsquery"], ScalarType "bool"),
    ("=", [Pseudo Record,Pseudo Record], ScalarType "bool"),
    (">", [ScalarType "tsvector",ScalarType "tsvector"], ScalarType "bool"),
    (">", [ScalarType "int8",ScalarType "int2"], ScalarType "bool"),
    (">", [ScalarType "money",ScalarType "money"], ScalarType "bool"),
    (">", [ScalarType "int8",ScalarType "int4"], ScalarType "bool"),
    (">", [Pseudo AnyEnum,Pseudo AnyEnum], ScalarType "bool"),
    (">", [ScalarType "oidvector",ScalarType "oidvector"], ScalarType "bool"),
    (">", [ScalarType "tinterval",ScalarType "tinterval"], ScalarType "bool"),
    (">", [ScalarType "float4",ScalarType "float4"], ScalarType "bool"),
    (">", [ScalarType "date",ScalarType "timestamptz"], ScalarType "bool"),
    (">", [ScalarType "char",ScalarType "char"], ScalarType "bool"),
    (">", [ScalarType "date",ScalarType "timestamp"], ScalarType "bool"),
    (">", [ScalarType "name",ScalarType "name"], ScalarType "bool"),
    (">", [ScalarType "timestamp",ScalarType "timestamp"], ScalarType "bool"),
    (">", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    (">", [ScalarType "float8",ScalarType "float8"], ScalarType "bool"),
    (">", [ScalarType "bytea",ScalarType "bytea"], ScalarType "bool"),
    (">", [ScalarType "uuid",ScalarType "uuid"], ScalarType "bool"),
    (">", [ScalarType "path",ScalarType "path"], ScalarType "bool"),
    (">", [ScalarType "tsquery",ScalarType "tsquery"], ScalarType "bool"),
    (">", [ScalarType "bool",ScalarType "bool"], ScalarType "bool"),
    (">", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    (">", [ScalarType "int2",ScalarType "int2"], ScalarType "bool"),
    (">", [ScalarType "int4",ScalarType "int4"], ScalarType "bool"),
    (">", [ScalarType "int2",ScalarType "int4"], ScalarType "bool"),
    (">", [ScalarType "inet",ScalarType "inet"], ScalarType "bool"),
    (">", [ScalarType "bit",ScalarType "bit"], ScalarType "bool"),
    (">", [ScalarType "float8",ScalarType "float4"], ScalarType "bool"),
    (">", [Pseudo Record,Pseudo Record], ScalarType "bool"),
    (">", [ScalarType "circle",ScalarType "circle"], ScalarType "bool"),
    (">", [ScalarType "int4",ScalarType "int2"], ScalarType "bool"),
    (">", [ScalarType "float4",ScalarType "float8"], ScalarType "bool"),
    (">", [ScalarType "int8",ScalarType "int8"], ScalarType "bool"),
    (">", [ScalarType "timestamptz",ScalarType "timestamp"], ScalarType "bool"),
    (">", [ScalarType "abstime",ScalarType "abstime"], ScalarType "bool"),
    (">", [ScalarType "timetz",ScalarType "timetz"], ScalarType "bool"),
    (">", [ScalarType "macaddr",ScalarType "macaddr"], ScalarType "bool"),
    (">", [ScalarType "time",ScalarType "time"], ScalarType "bool"),
    (">", [ScalarType "tid",ScalarType "tid"], ScalarType "bool"),
    (">", [ScalarType "int4",ScalarType "int8"], ScalarType "bool"),
    (">", [ScalarType "date",ScalarType "date"], ScalarType "bool"),
    (">", [ScalarType "interval",ScalarType "interval"], ScalarType "bool"),
    (">", [ScalarType "varbit",ScalarType "varbit"], ScalarType "bool"),
    (">", [Pseudo AnyArray,Pseudo AnyArray], ScalarType "bool"),
    (">", [ScalarType "lseg",ScalarType "lseg"], ScalarType "bool"),
    (">", [ScalarType "timestamptz",ScalarType "timestamptz"], ScalarType "bool"),
    (">", [ScalarType "timestamp",ScalarType "timestamptz"], ScalarType "bool"),
    (">", [ScalarType "bpchar",ScalarType "bpchar"], ScalarType "bool"),
    (">", [ScalarType "reltime",ScalarType "reltime"], ScalarType "bool"),
    (">", [ScalarType "int2",ScalarType "int8"], ScalarType "bool"),
    (">", [ScalarType "timestamptz",ScalarType "date"], ScalarType "bool"),
    (">", [ScalarType "oid",ScalarType "oid"], ScalarType "bool"),
    (">", [ScalarType "numeric",ScalarType "numeric"], ScalarType "bool"),
    (">", [ScalarType "timestamp",ScalarType "date"], ScalarType "bool"),
    (">=", [ScalarType "int2",ScalarType "int8"], ScalarType "bool"),
    (">=", [ScalarType "circle",ScalarType "circle"], ScalarType "bool"),
    (">=", [ScalarType "interval",ScalarType "interval"], ScalarType "bool"),
    (">=", [ScalarType "timestamptz",ScalarType "timestamptz"], ScalarType "bool"),
    (">=", [ScalarType "numeric",ScalarType "numeric"], ScalarType "bool"),
    (">=", [ScalarType "float8",ScalarType "float4"], ScalarType "bool"),
    (">=", [ScalarType "bit",ScalarType "bit"], ScalarType "bool"),
    (">=", [ScalarType "float4",ScalarType "float8"], ScalarType "bool"),
    (">=", [ScalarType "timetz",ScalarType "timetz"], ScalarType "bool"),
    (">=", [ScalarType "time",ScalarType "time"], ScalarType "bool"),
    (">=", [ScalarType "date",ScalarType "date"], ScalarType "bool"),
    (">=", [Pseudo AnyArray,Pseudo AnyArray], ScalarType "bool"),
    (">=", [ScalarType "varbit",ScalarType "varbit"], ScalarType "bool"),
    (">=", [ScalarType "bpchar",ScalarType "bpchar"], ScalarType "bool"),
    (">=", [ScalarType "tsquery",ScalarType "tsquery"], ScalarType "bool"),
    (">=", [ScalarType "money",ScalarType "money"], ScalarType "bool"),
    (">=", [ScalarType "int8",ScalarType "int2"], ScalarType "bool"),
    (">=", [ScalarType "tinterval",ScalarType "tinterval"], ScalarType "bool"),
    (">=", [ScalarType "path",ScalarType "path"], ScalarType "bool"),
    (">=", [ScalarType "bytea",ScalarType "bytea"], ScalarType "bool"),
    (">=", [ScalarType "float8",ScalarType "float8"], ScalarType "bool"),
    (">=", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    (">=", [ScalarType "timestamp",ScalarType "timestamp"], ScalarType "bool"),
    (">=", [ScalarType "name",ScalarType "name"], ScalarType "bool"),
    (">=", [ScalarType "date",ScalarType "timestamp"], ScalarType "bool"),
    (">=", [ScalarType "char",ScalarType "char"], ScalarType "bool"),
    (">=", [ScalarType "float4",ScalarType "float4"], ScalarType "bool"),
    (">=", [ScalarType "date",ScalarType "timestamptz"], ScalarType "bool"),
    (">=", [ScalarType "oidvector",ScalarType "oidvector"], ScalarType "bool"),
    (">=", [ScalarType "bool",ScalarType "bool"], ScalarType "bool"),
    (">=", [ScalarType "timestamp",ScalarType "date"], ScalarType "bool"),
    (">=", [ScalarType "oid",ScalarType "oid"], ScalarType "bool"),
    (">=", [ScalarType "timestamptz",ScalarType "date"], ScalarType "bool"),
    (">=", [ScalarType "reltime",ScalarType "reltime"], ScalarType "bool"),
    (">=", [ScalarType "abstime",ScalarType "abstime"], ScalarType "bool"),
    (">=", [ScalarType "timestamp",ScalarType "timestamptz"], ScalarType "bool"),
    (">=", [ScalarType "int4",ScalarType "int2"], ScalarType "bool"),
    (">=", [ScalarType "int2",ScalarType "int4"], ScalarType "bool"),
    (">=", [Pseudo Record,Pseudo Record], ScalarType "bool"),
    (">=", [ScalarType "timestamptz",ScalarType "timestamp"], ScalarType "bool"),
    (">=", [ScalarType "int4",ScalarType "int4"], ScalarType "bool"),
    (">=", [ScalarType "int2",ScalarType "int2"], ScalarType "bool"),
    (">=", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    (">=", [ScalarType "uuid",ScalarType "uuid"], ScalarType "bool"),
    (">=", [ScalarType "int4",ScalarType "int8"], ScalarType "bool"),
    (">=", [ScalarType "int8",ScalarType "int4"], ScalarType "bool"),
    (">=", [Pseudo AnyEnum,Pseudo AnyEnum], ScalarType "bool"),
    (">=", [ScalarType "int8",ScalarType "int8"], ScalarType "bool"),
    (">=", [ScalarType "tid",ScalarType "tid"], ScalarType "bool"),
    (">=", [ScalarType "tsvector",ScalarType "tsvector"], ScalarType "bool"),
    (">=", [ScalarType "lseg",ScalarType "lseg"], ScalarType "bool"),
    (">=", [ScalarType "macaddr",ScalarType "macaddr"], ScalarType "bool"),
    (">=", [ScalarType "inet",ScalarType "inet"], ScalarType "bool"),
    (">>", [ScalarType "point",ScalarType "point"], ScalarType "bool"),
    (">>", [ScalarType "int4",ScalarType "int4"], ScalarType "int4"),
    (">>", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    (">>", [ScalarType "int8",ScalarType "int4"], ScalarType "int8"),
    (">>", [ScalarType "inet",ScalarType "inet"], ScalarType "bool"),
    (">>", [ScalarType "circle",ScalarType "circle"], ScalarType "bool"),
    (">>", [ScalarType "bit",ScalarType "int4"], ScalarType "bit"),
    (">>", [ScalarType "int2",ScalarType "int4"], ScalarType "int2"),
    (">>", [ScalarType "polygon",ScalarType "polygon"], ScalarType "bool"),
    (">>=", [ScalarType "inet",ScalarType "inet"], ScalarType "bool"),
    (">^", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    (">^", [ScalarType "point",ScalarType "point"], ScalarType "bool"),
    ("?#", [ScalarType "lseg",ScalarType "lseg"], ScalarType "bool"),
    ("?#", [ScalarType "path",ScalarType "path"], ScalarType "bool"),
    ("?#", [ScalarType "lseg",ScalarType "box"], ScalarType "bool"),
    ("?#", [ScalarType "line",ScalarType "box"], ScalarType "bool"),
    ("?#", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    ("?#", [ScalarType "lseg",ScalarType "line"], ScalarType "bool"),
    ("?#", [ScalarType "line",ScalarType "line"], ScalarType "bool"),
    ("?-", [ScalarType "point",ScalarType "point"], ScalarType "bool"),
    ("?-|", [ScalarType "line",ScalarType "line"], ScalarType "bool"),
    ("?-|", [ScalarType "lseg",ScalarType "lseg"], ScalarType "bool"),
    ("?|", [ScalarType "point",ScalarType "point"], ScalarType "bool"),
    ("?||", [ScalarType "line",ScalarType "line"], ScalarType "bool"),
    ("?||", [ScalarType "lseg",ScalarType "lseg"], ScalarType "bool"),
    ("@>", [ScalarType "circle",ScalarType "circle"], ScalarType "bool"),
    ("@>", [ArrayType (ScalarType "aclitem"),ScalarType "aclitem"], ScalarType "bool"),
    ("@>", [ScalarType "circle",ScalarType "point"], ScalarType "bool"),
    ("@>", [ScalarType "path",ScalarType "point"], ScalarType "bool"),
    ("@>", [Pseudo AnyArray,Pseudo AnyArray], ScalarType "bool"),
    ("@>", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    ("@>", [ScalarType "polygon",ScalarType "point"], ScalarType "bool"),
    ("@>", [ScalarType "polygon",ScalarType "polygon"], ScalarType "bool"),
    ("@>", [ScalarType "tsquery",ScalarType "tsquery"], ScalarType "bool"),
    ("@@", [ScalarType "tsvector",ScalarType "tsquery"], ScalarType "bool"),
    ("@@", [ScalarType "tsquery",ScalarType "tsvector"], ScalarType "bool"),
    ("@@", [ScalarType "text",ScalarType "tsquery"], ScalarType "bool"),
    ("@@", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("@@@", [ScalarType "tsvector",ScalarType "tsquery"], ScalarType "bool"),
    ("@@@", [ScalarType "tsquery",ScalarType "tsvector"], ScalarType "bool"),
    ("^", [ScalarType "numeric",ScalarType "numeric"], ScalarType "numeric"),
    ("^", [ScalarType "float8",ScalarType "float8"], ScalarType "float8"),
    ("|", [ScalarType "inet",ScalarType "inet"], ScalarType "inet"),
    ("|", [ScalarType "bit",ScalarType "bit"], ScalarType "bit"),
    ("|", [ScalarType "int8",ScalarType "int8"], ScalarType "int8"),
    ("|", [ScalarType "int4",ScalarType "int4"], ScalarType "int4"),
    ("|", [ScalarType "int2",ScalarType "int2"], ScalarType "int2"),
    ("|&>", [ScalarType "circle",ScalarType "circle"], ScalarType "bool"),
    ("|&>", [ScalarType "polygon",ScalarType "polygon"], ScalarType "bool"),
    ("|&>", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    ("|>>", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    ("|>>", [ScalarType "circle",ScalarType "circle"], ScalarType "bool"),
    ("|>>", [ScalarType "polygon",ScalarType "polygon"], ScalarType "bool"),
    ("||", [ScalarType "varbit",ScalarType "varbit"], ScalarType "varbit"),
    ("||", [ScalarType "tsvector",ScalarType "tsvector"], ScalarType "tsvector"),
    ("||", [ScalarType "tsquery",ScalarType "tsquery"], ScalarType "tsquery"),
    ("||", [Pseudo AnyNonArray,ScalarType "text"], ScalarType "text"),
    ("||", [ScalarType "text",Pseudo AnyNonArray], ScalarType "text"),
    ("||", [ScalarType "text",ScalarType "text"], ScalarType "text"),
    ("||", [ScalarType "bytea",ScalarType "bytea"], ScalarType "bytea"),
    ("||", [Pseudo AnyArray,Pseudo AnyElement], Pseudo AnyArray),
    ("||", [Pseudo AnyArray,Pseudo AnyArray], Pseudo AnyArray),
    ("||", [Pseudo AnyElement,Pseudo AnyArray], Pseudo AnyArray),
    ("~", [ScalarType "polygon",ScalarType "polygon"], ScalarType "bool"),
    ("~", [ScalarType "circle",ScalarType "circle"], ScalarType "bool"),
    ("~", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    ("~", [ScalarType "bpchar",ScalarType "text"], ScalarType "bool"),
    ("~", [ScalarType "name",ScalarType "text"], ScalarType "bool"),
    ("~", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("~", [ArrayType (ScalarType "aclitem"),ScalarType "aclitem"], ScalarType "bool"),
    ("~", [ScalarType "circle",ScalarType "point"], ScalarType "bool"),
    ("~", [ScalarType "polygon",ScalarType "point"], ScalarType "bool"),
    ("~", [ScalarType "path",ScalarType "point"], ScalarType "bool"),
    ("~*", [ScalarType "bpchar",ScalarType "text"], ScalarType "bool"),
    ("~*", [ScalarType "name",ScalarType "text"], ScalarType "bool"),
    ("~*", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("~<=~", [ScalarType "bpchar",ScalarType "bpchar"], ScalarType "bool"),
    ("~<=~", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("~<~", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("~<~", [ScalarType "bpchar",ScalarType "bpchar"], ScalarType "bool"),
    ("~=", [ScalarType "point",ScalarType "point"], ScalarType "bool"),
    ("~=", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    ("~=", [ScalarType "tinterval",ScalarType "tinterval"], ScalarType "bool"),
    ("~=", [ScalarType "polygon",ScalarType "polygon"], ScalarType "bool"),
    ("~=", [ScalarType "circle",ScalarType "circle"], ScalarType "bool"),
    ("~>=~", [ScalarType "bpchar",ScalarType "bpchar"], ScalarType "bool"),
    ("~>=~", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("~>~", [ScalarType "bpchar",ScalarType "bpchar"], ScalarType "bool"),
    ("~>~", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("~~", [ScalarType "bpchar",ScalarType "text"], ScalarType "bool"),
    ("~~", [ScalarType "bytea",ScalarType "bytea"], ScalarType "bool"),
    ("~~", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("~~", [ScalarType "name",ScalarType "text"], ScalarType "bool"),
    ("~~*", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("~~*", [ScalarType "bpchar",ScalarType "text"], ScalarType "bool"),
    ("~~*", [ScalarType "name",ScalarType "text"], ScalarType "bool")
    ]
prefixOperatorTypes = [
    ("!!", [ScalarType "tsquery"], ScalarType "tsquery"),
    ("!!", [ScalarType "int8"], ScalarType "numeric"),
    ("#", [ScalarType "polygon"], ScalarType "int4"),
    ("#", [ScalarType "path"], ScalarType "int4"),
    ("+", [ScalarType "float8"], ScalarType "float8"),
    ("+", [ScalarType "int4"], ScalarType "int4"),
    ("+", [ScalarType "float4"], ScalarType "float4"),
    ("+", [ScalarType "int2"], ScalarType "int2"),
    ("+", [ScalarType "int8"], ScalarType "int8"),
    ("+", [ScalarType "numeric"], ScalarType "numeric"),
    ("-", [ScalarType "int2"], ScalarType "int2"),
    ("-", [ScalarType "int8"], ScalarType "int8"),
    ("-", [ScalarType "int4"], ScalarType "int4"),
    ("-", [ScalarType "float4"], ScalarType "float4"),
    ("-", [ScalarType "float8"], ScalarType "float8"),
    ("-", [ScalarType "interval"], ScalarType "interval"),
    ("-", [ScalarType "numeric"], ScalarType "numeric"),
    ("?-", [ScalarType "lseg"], ScalarType "bool"),
    ("?-", [ScalarType "line"], ScalarType "bool"),
    ("?|", [ScalarType "line"], ScalarType "bool"),
    ("?|", [ScalarType "lseg"], ScalarType "bool"),
    ("@", [ScalarType "numeric"], ScalarType "numeric"),
    ("@", [ScalarType "int2"], ScalarType "int2"),
    ("@", [ScalarType "float4"], ScalarType "float4"),
    ("@", [ScalarType "int8"], ScalarType "int8"),
    ("@", [ScalarType "int4"], ScalarType "int4"),
    ("@", [ScalarType "float8"], ScalarType "float8"),
    ("@-@", [ScalarType "path"], ScalarType "float8"),
    ("@-@", [ScalarType "lseg"], ScalarType "float8"),
    ("@@", [ScalarType "box"], ScalarType "point"),
    ("@@", [ScalarType "circle"], ScalarType "point"),
    ("@@", [ScalarType "lseg"], ScalarType "point"),
    ("@@", [ScalarType "path"], ScalarType "point"),
    ("@@", [ScalarType "polygon"], ScalarType "point"),
    ("|", [ScalarType "tinterval"], ScalarType "abstime"),
    ("|/", [ScalarType "float8"], ScalarType "float8"),
    ("||/", [ScalarType "float8"], ScalarType "float8"),
    ("~", [ScalarType "int4"], ScalarType "int4"),
    ("~", [ScalarType "int2"], ScalarType "int2"),
    ("~", [ScalarType "bit"], ScalarType "bit"),
    ("~", [ScalarType "int8"], ScalarType "int8"),
    ("~", [ScalarType "inet"], ScalarType "inet")
    ]
postfixOperatorTypes = [
    ("!", [ScalarType "int8"], ScalarType "numeric")
    ]
functionTypes = [
    ("RI_FKey_cascade_del", [], Pseudo Trigger),
    ("RI_FKey_cascade_upd", [], Pseudo Trigger),
    ("RI_FKey_check_ins", [], Pseudo Trigger),
    ("RI_FKey_check_upd", [], Pseudo Trigger),
    ("RI_FKey_noaction_del", [], Pseudo Trigger),
    ("RI_FKey_noaction_upd", [], Pseudo Trigger),
    ("RI_FKey_restrict_del", [], Pseudo Trigger),
    ("RI_FKey_restrict_upd", [], Pseudo Trigger),
    ("RI_FKey_setdefault_del", [], Pseudo Trigger),
    ("RI_FKey_setdefault_upd", [], Pseudo Trigger),
    ("RI_FKey_setnull_del", [], Pseudo Trigger),
    ("RI_FKey_setnull_upd", [], Pseudo Trigger),
    ("abbrev", [ScalarType "cidr"], ScalarType "text"),
    ("abbrev", [ScalarType "inet"], ScalarType "text"),
    ("abs", [ScalarType "float4"], ScalarType "float4"),
    ("abs", [ScalarType "float8"], ScalarType "float8"),
    ("abs", [ScalarType "int2"], ScalarType "int2"),
    ("abs", [ScalarType "int4"], ScalarType "int4"),
    ("abs", [ScalarType "int8"], ScalarType "int8"),
    ("abs", [ScalarType "numeric"], ScalarType "numeric"),
    ("abstime", [ScalarType "timestamp"], ScalarType "abstime"),
    ("abstime", [ScalarType "timestamptz"], ScalarType "abstime"),
    ("abstimeeq", [ScalarType "abstime",ScalarType "abstime"], ScalarType "bool"),
    ("abstimege", [ScalarType "abstime",ScalarType "abstime"], ScalarType "bool"),
    ("abstimegt", [ScalarType "abstime",ScalarType "abstime"], ScalarType "bool"),
    ("abstimein", [Pseudo Cstring], ScalarType "abstime"),
    ("abstimele", [ScalarType "abstime",ScalarType "abstime"], ScalarType "bool"),
    ("abstimelt", [ScalarType "abstime",ScalarType "abstime"], ScalarType "bool"),
    ("abstimene", [ScalarType "abstime",ScalarType "abstime"], ScalarType "bool"),
    ("abstimeout", [ScalarType "abstime"], Pseudo Cstring),
    ("abstimesend", [ScalarType "abstime"], ScalarType "bytea"),
    ("aclcontains", [ArrayType (ScalarType "aclitem"),ScalarType "aclitem"], ScalarType "bool"),
    ("aclinsert", [ArrayType (ScalarType "aclitem"),ScalarType "aclitem"], ArrayType (ScalarType "aclitem")),
    ("aclitemeq", [ScalarType "aclitem",ScalarType "aclitem"], ScalarType "bool"),
    ("aclitemin", [Pseudo Cstring], ScalarType "aclitem"),
    ("aclitemout", [ScalarType "aclitem"], Pseudo Cstring),
    ("aclremove", [ArrayType (ScalarType "aclitem"),ScalarType "aclitem"], ArrayType (ScalarType "aclitem")),
    ("acos", [ScalarType "float8"], ScalarType "float8"),
    ("age", [ScalarType "timestamp"], ScalarType "interval"),
    ("age", [ScalarType "timestamp",ScalarType "timestamp"], ScalarType "interval"),
    ("age", [ScalarType "timestamptz"], ScalarType "interval"),
    ("age", [ScalarType "timestamptz",ScalarType "timestamptz"], ScalarType "interval"),
    ("age", [ScalarType "xid"], ScalarType "int4"),
    ("any_in", [Pseudo Cstring], Pseudo Any),
    ("any_out", [Pseudo Any], Pseudo Cstring),
    ("anyarray_in", [Pseudo Cstring], Pseudo AnyArray),
    ("anyarray_out", [Pseudo AnyArray], Pseudo Cstring),
    ("anyarray_send", [Pseudo AnyArray], ScalarType "bytea"),
    ("anyelement_in", [Pseudo Cstring], Pseudo AnyElement),
    ("anyelement_out", [Pseudo AnyElement], Pseudo Cstring),
    ("anyenum_in", [Pseudo Cstring], Pseudo AnyEnum),
    ("anyenum_out", [Pseudo AnyEnum], Pseudo Cstring),
    ("anynonarray_in", [Pseudo Cstring], Pseudo AnyNonArray),
    ("anynonarray_out", [Pseudo AnyNonArray], Pseudo Cstring),
    ("anytextcat", [Pseudo AnyNonArray,ScalarType "text"], ScalarType "text"),
    ("area", [ScalarType "box"], ScalarType "float8"),
    ("area", [ScalarType "circle"], ScalarType "float8"),
    ("area", [ScalarType "path"], ScalarType "float8"),
    ("array_append", [Pseudo AnyArray,Pseudo AnyElement], Pseudo AnyArray),
    ("array_cat", [Pseudo AnyArray,Pseudo AnyArray], Pseudo AnyArray),
    ("array_dims", [Pseudo AnyArray], ScalarType "text"),
    ("array_eq", [Pseudo AnyArray,Pseudo AnyArray], ScalarType "bool"),
    ("array_fill", [Pseudo AnyElement,ArrayType (ScalarType "int4")], Pseudo AnyArray),
    ("array_fill", [Pseudo AnyElement,ArrayType (ScalarType "int4"),ArrayType (ScalarType "int4")], Pseudo AnyArray),
    ("array_ge", [Pseudo AnyArray,Pseudo AnyArray], ScalarType "bool"),
    ("array_gt", [Pseudo AnyArray,Pseudo AnyArray], ScalarType "bool"),
    ("array_in", [Pseudo Cstring,ScalarType "oid",ScalarType "int4"], Pseudo AnyArray),
    ("array_larger", [Pseudo AnyArray,Pseudo AnyArray], Pseudo AnyArray),
    ("array_le", [Pseudo AnyArray,Pseudo AnyArray], ScalarType "bool"),
    ("array_length", [Pseudo AnyArray,ScalarType "int4"], ScalarType "int4"),
    ("array_lower", [Pseudo AnyArray,ScalarType "int4"], ScalarType "int4"),
    ("array_lt", [Pseudo AnyArray,Pseudo AnyArray], ScalarType "bool"),
    ("array_ndims", [Pseudo AnyArray], ScalarType "int4"),
    ("array_ne", [Pseudo AnyArray,Pseudo AnyArray], ScalarType "bool"),
    ("array_out", [Pseudo AnyArray], Pseudo Cstring),
    ("array_prepend", [Pseudo AnyElement,Pseudo AnyArray], Pseudo AnyArray),
    ("array_send", [Pseudo AnyArray], ScalarType "bytea"),
    ("array_smaller", [Pseudo AnyArray,Pseudo AnyArray], Pseudo AnyArray),
    ("array_to_string", [Pseudo AnyArray,ScalarType "text"], ScalarType "text"),
    ("array_upper", [Pseudo AnyArray,ScalarType "int4"], ScalarType "int4"),
    ("arraycontained", [Pseudo AnyArray,Pseudo AnyArray], ScalarType "bool"),
    ("arraycontains", [Pseudo AnyArray,Pseudo AnyArray], ScalarType "bool"),
    ("arrayoverlap", [Pseudo AnyArray,Pseudo AnyArray], ScalarType "bool"),
    ("ascii", [ScalarType "text"], ScalarType "int4"),
    ("asin", [ScalarType "float8"], ScalarType "float8"),
    ("atan", [ScalarType "float8"], ScalarType "float8"),
    ("atan2", [ScalarType "float8",ScalarType "float8"], ScalarType "float8"),
    ("bit", [ScalarType "bit",ScalarType "int4",ScalarType "bool"], ScalarType "bit"),
    ("bit", [ScalarType "int4",ScalarType "int4"], ScalarType "bit"),
    ("bit", [ScalarType "int8",ScalarType "int4"], ScalarType "bit"),
    ("bit_in", [Pseudo Cstring,ScalarType "oid",ScalarType "int4"], ScalarType "bit"),
    ("bit_length", [ScalarType "bit"], ScalarType "int4"),
    ("bit_length", [ScalarType "bytea"], ScalarType "int4"),
    ("bit_length", [ScalarType "text"], ScalarType "int4"),
    ("bit_out", [ScalarType "bit"], Pseudo Cstring),
    ("bit_send", [ScalarType "bit"], ScalarType "bytea"),
    ("bitand", [ScalarType "bit",ScalarType "bit"], ScalarType "bit"),
    ("bitcat", [ScalarType "varbit",ScalarType "varbit"], ScalarType "varbit"),
    ("bitcmp", [ScalarType "bit",ScalarType "bit"], ScalarType "int4"),
    ("biteq", [ScalarType "bit",ScalarType "bit"], ScalarType "bool"),
    ("bitge", [ScalarType "bit",ScalarType "bit"], ScalarType "bool"),
    ("bitgt", [ScalarType "bit",ScalarType "bit"], ScalarType "bool"),
    ("bitle", [ScalarType "bit",ScalarType "bit"], ScalarType "bool"),
    ("bitlt", [ScalarType "bit",ScalarType "bit"], ScalarType "bool"),
    ("bitne", [ScalarType "bit",ScalarType "bit"], ScalarType "bool"),
    ("bitnot", [ScalarType "bit"], ScalarType "bit"),
    ("bitor", [ScalarType "bit",ScalarType "bit"], ScalarType "bit"),
    ("bitshiftleft", [ScalarType "bit",ScalarType "int4"], ScalarType "bit"),
    ("bitshiftright", [ScalarType "bit",ScalarType "int4"], ScalarType "bit"),
    ("bittypmodin", [ArrayType (Pseudo Cstring)], ScalarType "int4"),
    ("bittypmodout", [ScalarType "int4"], Pseudo Cstring),
    ("bitxor", [ScalarType "bit",ScalarType "bit"], ScalarType "bit"),
    ("bool", [ScalarType "int4"], ScalarType "bool"),
    ("booland_statefunc", [ScalarType "bool",ScalarType "bool"], ScalarType "bool"),
    ("booleq", [ScalarType "bool",ScalarType "bool"], ScalarType "bool"),
    ("boolge", [ScalarType "bool",ScalarType "bool"], ScalarType "bool"),
    ("boolgt", [ScalarType "bool",ScalarType "bool"], ScalarType "bool"),
    ("boolin", [Pseudo Cstring], ScalarType "bool"),
    ("boolle", [ScalarType "bool",ScalarType "bool"], ScalarType "bool"),
    ("boollt", [ScalarType "bool",ScalarType "bool"], ScalarType "bool"),
    ("boolne", [ScalarType "bool",ScalarType "bool"], ScalarType "bool"),
    ("boolor_statefunc", [ScalarType "bool",ScalarType "bool"], ScalarType "bool"),
    ("boolout", [ScalarType "bool"], Pseudo Cstring),
    ("boolsend", [ScalarType "bool"], ScalarType "bytea"),
    ("box", [ScalarType "circle"], ScalarType "box"),
    ("box", [ScalarType "point",ScalarType "point"], ScalarType "box"),
    ("box", [ScalarType "polygon"], ScalarType "box"),
    ("box_above", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    ("box_above_eq", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    ("box_add", [ScalarType "box",ScalarType "point"], ScalarType "box"),
    ("box_below", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    ("box_below_eq", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    ("box_center", [ScalarType "box"], ScalarType "point"),
    ("box_contain", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    ("box_contained", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    ("box_distance", [ScalarType "box",ScalarType "box"], ScalarType "float8"),
    ("box_div", [ScalarType "box",ScalarType "point"], ScalarType "box"),
    ("box_eq", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    ("box_ge", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    ("box_gt", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    ("box_in", [Pseudo Cstring], ScalarType "box"),
    ("box_intersect", [ScalarType "box",ScalarType "box"], ScalarType "box"),
    ("box_le", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    ("box_left", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    ("box_lt", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    ("box_mul", [ScalarType "box",ScalarType "point"], ScalarType "box"),
    ("box_out", [ScalarType "box"], Pseudo Cstring),
    ("box_overabove", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    ("box_overbelow", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    ("box_overlap", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    ("box_overleft", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    ("box_overright", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    ("box_right", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    ("box_same", [ScalarType "box",ScalarType "box"], ScalarType "bool"),
    ("box_send", [ScalarType "box"], ScalarType "bytea"),
    ("box_sub", [ScalarType "box",ScalarType "point"], ScalarType "box"),
    ("bpchar", [ScalarType "bpchar",ScalarType "int4",ScalarType "bool"], ScalarType "bpchar"),
    ("bpchar", [ScalarType "char"], ScalarType "bpchar"),
    ("bpchar", [ScalarType "name"], ScalarType "bpchar"),
    ("bpchar_larger", [ScalarType "bpchar",ScalarType "bpchar"], ScalarType "bpchar"),
    ("bpchar_pattern_ge", [ScalarType "bpchar",ScalarType "bpchar"], ScalarType "bool"),
    ("bpchar_pattern_gt", [ScalarType "bpchar",ScalarType "bpchar"], ScalarType "bool"),
    ("bpchar_pattern_le", [ScalarType "bpchar",ScalarType "bpchar"], ScalarType "bool"),
    ("bpchar_pattern_lt", [ScalarType "bpchar",ScalarType "bpchar"], ScalarType "bool"),
    ("bpchar_smaller", [ScalarType "bpchar",ScalarType "bpchar"], ScalarType "bpchar"),
    ("bpcharcmp", [ScalarType "bpchar",ScalarType "bpchar"], ScalarType "int4"),
    ("bpchareq", [ScalarType "bpchar",ScalarType "bpchar"], ScalarType "bool"),
    ("bpcharge", [ScalarType "bpchar",ScalarType "bpchar"], ScalarType "bool"),
    ("bpchargt", [ScalarType "bpchar",ScalarType "bpchar"], ScalarType "bool"),
    ("bpchariclike", [ScalarType "bpchar",ScalarType "text"], ScalarType "bool"),
    ("bpcharicnlike", [ScalarType "bpchar",ScalarType "text"], ScalarType "bool"),
    ("bpcharicregexeq", [ScalarType "bpchar",ScalarType "text"], ScalarType "bool"),
    ("bpcharicregexne", [ScalarType "bpchar",ScalarType "text"], ScalarType "bool"),
    ("bpcharin", [Pseudo Cstring,ScalarType "oid",ScalarType "int4"], ScalarType "bpchar"),
    ("bpcharle", [ScalarType "bpchar",ScalarType "bpchar"], ScalarType "bool"),
    ("bpcharlike", [ScalarType "bpchar",ScalarType "text"], ScalarType "bool"),
    ("bpcharlt", [ScalarType "bpchar",ScalarType "bpchar"], ScalarType "bool"),
    ("bpcharne", [ScalarType "bpchar",ScalarType "bpchar"], ScalarType "bool"),
    ("bpcharnlike", [ScalarType "bpchar",ScalarType "text"], ScalarType "bool"),
    ("bpcharout", [ScalarType "bpchar"], Pseudo Cstring),
    ("bpcharregexeq", [ScalarType "bpchar",ScalarType "text"], ScalarType "bool"),
    ("bpcharregexne", [ScalarType "bpchar",ScalarType "text"], ScalarType "bool"),
    ("bpcharsend", [ScalarType "bpchar"], ScalarType "bytea"),
    ("bpchartypmodin", [ArrayType (Pseudo Cstring)], ScalarType "int4"),
    ("bpchartypmodout", [ScalarType "int4"], Pseudo Cstring),
    ("broadcast", [ScalarType "inet"], ScalarType "inet"),
    ("btabstimecmp", [ScalarType "abstime",ScalarType "abstime"], ScalarType "int4"),
    ("btarraycmp", [Pseudo AnyArray,Pseudo AnyArray], ScalarType "int4"),
    ("btboolcmp", [ScalarType "bool",ScalarType "bool"], ScalarType "int4"),
    ("btbpchar_pattern_cmp", [ScalarType "bpchar",ScalarType "bpchar"], ScalarType "int4"),
    ("btcharcmp", [ScalarType "char",ScalarType "char"], ScalarType "int4"),
    ("btfloat48cmp", [ScalarType "float4",ScalarType "float8"], ScalarType "int4"),
    ("btfloat4cmp", [ScalarType "float4",ScalarType "float4"], ScalarType "int4"),
    ("btfloat84cmp", [ScalarType "float8",ScalarType "float4"], ScalarType "int4"),
    ("btfloat8cmp", [ScalarType "float8",ScalarType "float8"], ScalarType "int4"),
    ("btint24cmp", [ScalarType "int2",ScalarType "int4"], ScalarType "int4"),
    ("btint28cmp", [ScalarType "int2",ScalarType "int8"], ScalarType "int4"),
    ("btint2cmp", [ScalarType "int2",ScalarType "int2"], ScalarType "int4"),
    ("btint42cmp", [ScalarType "int4",ScalarType "int2"], ScalarType "int4"),
    ("btint48cmp", [ScalarType "int4",ScalarType "int8"], ScalarType "int4"),
    ("btint4cmp", [ScalarType "int4",ScalarType "int4"], ScalarType "int4"),
    ("btint82cmp", [ScalarType "int8",ScalarType "int2"], ScalarType "int4"),
    ("btint84cmp", [ScalarType "int8",ScalarType "int4"], ScalarType "int4"),
    ("btint8cmp", [ScalarType "int8",ScalarType "int8"], ScalarType "int4"),
    ("btnamecmp", [ScalarType "name",ScalarType "name"], ScalarType "int4"),
    ("btoidcmp", [ScalarType "oid",ScalarType "oid"], ScalarType "int4"),
    ("btoidvectorcmp", [ScalarType "oidvector",ScalarType "oidvector"], ScalarType "int4"),
    ("btoptions", [ArrayType (ScalarType "text"),ScalarType "bool"], ScalarType "bytea"),
    ("btrecordcmp", [Pseudo Record,Pseudo Record], ScalarType "int4"),
    ("btreltimecmp", [ScalarType "reltime",ScalarType "reltime"], ScalarType "int4"),
    ("btrim", [ScalarType "bytea",ScalarType "bytea"], ScalarType "bytea"),
    ("btrim", [ScalarType "text"], ScalarType "text"),
    ("btrim", [ScalarType "text",ScalarType "text"], ScalarType "text"),
    ("bttext_pattern_cmp", [ScalarType "text",ScalarType "text"], ScalarType "int4"),
    ("bttextcmp", [ScalarType "text",ScalarType "text"], ScalarType "int4"),
    ("bttidcmp", [ScalarType "tid",ScalarType "tid"], ScalarType "int4"),
    ("bttintervalcmp", [ScalarType "tinterval",ScalarType "tinterval"], ScalarType "int4"),
    ("byteacat", [ScalarType "bytea",ScalarType "bytea"], ScalarType "bytea"),
    ("byteacmp", [ScalarType "bytea",ScalarType "bytea"], ScalarType "int4"),
    ("byteaeq", [ScalarType "bytea",ScalarType "bytea"], ScalarType "bool"),
    ("byteage", [ScalarType "bytea",ScalarType "bytea"], ScalarType "bool"),
    ("byteagt", [ScalarType "bytea",ScalarType "bytea"], ScalarType "bool"),
    ("byteain", [Pseudo Cstring], ScalarType "bytea"),
    ("byteale", [ScalarType "bytea",ScalarType "bytea"], ScalarType "bool"),
    ("bytealike", [ScalarType "bytea",ScalarType "bytea"], ScalarType "bool"),
    ("bytealt", [ScalarType "bytea",ScalarType "bytea"], ScalarType "bool"),
    ("byteane", [ScalarType "bytea",ScalarType "bytea"], ScalarType "bool"),
    ("byteanlike", [ScalarType "bytea",ScalarType "bytea"], ScalarType "bool"),
    ("byteaout", [ScalarType "bytea"], Pseudo Cstring),
    ("byteasend", [ScalarType "bytea"], ScalarType "bytea"),
    ("cash_cmp", [ScalarType "money",ScalarType "money"], ScalarType "int4"),
    ("cash_div_flt4", [ScalarType "money",ScalarType "float4"], ScalarType "money"),
    ("cash_div_flt8", [ScalarType "money",ScalarType "float8"], ScalarType "money"),
    ("cash_div_int2", [ScalarType "money",ScalarType "int2"], ScalarType "money"),
    ("cash_div_int4", [ScalarType "money",ScalarType "int4"], ScalarType "money"),
    ("cash_eq", [ScalarType "money",ScalarType "money"], ScalarType "bool"),
    ("cash_ge", [ScalarType "money",ScalarType "money"], ScalarType "bool"),
    ("cash_gt", [ScalarType "money",ScalarType "money"], ScalarType "bool"),
    ("cash_in", [Pseudo Cstring], ScalarType "money"),
    ("cash_le", [ScalarType "money",ScalarType "money"], ScalarType "bool"),
    ("cash_lt", [ScalarType "money",ScalarType "money"], ScalarType "bool"),
    ("cash_mi", [ScalarType "money",ScalarType "money"], ScalarType "money"),
    ("cash_mul_flt4", [ScalarType "money",ScalarType "float4"], ScalarType "money"),
    ("cash_mul_flt8", [ScalarType "money",ScalarType "float8"], ScalarType "money"),
    ("cash_mul_int2", [ScalarType "money",ScalarType "int2"], ScalarType "money"),
    ("cash_mul_int4", [ScalarType "money",ScalarType "int4"], ScalarType "money"),
    ("cash_ne", [ScalarType "money",ScalarType "money"], ScalarType "bool"),
    ("cash_out", [ScalarType "money"], Pseudo Cstring),
    ("cash_pl", [ScalarType "money",ScalarType "money"], ScalarType "money"),
    ("cash_send", [ScalarType "money"], ScalarType "bytea"),
    ("cash_words", [ScalarType "money"], ScalarType "text"),
    ("cashlarger", [ScalarType "money",ScalarType "money"], ScalarType "money"),
    ("cashsmaller", [ScalarType "money",ScalarType "money"], ScalarType "money"),
    ("cbrt", [ScalarType "float8"], ScalarType "float8"),
    ("ceil", [ScalarType "float8"], ScalarType "float8"),
    ("ceil", [ScalarType "numeric"], ScalarType "numeric"),
    ("ceiling", [ScalarType "float8"], ScalarType "float8"),
    ("ceiling", [ScalarType "numeric"], ScalarType "numeric"),
    ("center", [ScalarType "box"], ScalarType "point"),
    ("center", [ScalarType "circle"], ScalarType "point"),
    ("char", [ScalarType "int4"], ScalarType "char"),
    ("char", [ScalarType "text"], ScalarType "char"),
    ("char_length", [ScalarType "bpchar"], ScalarType "int4"),
    ("char_length", [ScalarType "text"], ScalarType "int4"),
    ("character_length", [ScalarType "bpchar"], ScalarType "int4"),
    ("character_length", [ScalarType "text"], ScalarType "int4"),
    ("chareq", [ScalarType "char",ScalarType "char"], ScalarType "bool"),
    ("charge", [ScalarType "char",ScalarType "char"], ScalarType "bool"),
    ("chargt", [ScalarType "char",ScalarType "char"], ScalarType "bool"),
    ("charin", [Pseudo Cstring], ScalarType "char"),
    ("charle", [ScalarType "char",ScalarType "char"], ScalarType "bool"),
    ("charlt", [ScalarType "char",ScalarType "char"], ScalarType "bool"),
    ("charne", [ScalarType "char",ScalarType "char"], ScalarType "bool"),
    ("charout", [ScalarType "char"], Pseudo Cstring),
    ("charsend", [ScalarType "char"], ScalarType "bytea"),
    ("chr", [ScalarType "int4"], ScalarType "text"),
    ("cideq", [ScalarType "cid",ScalarType "cid"], ScalarType "bool"),
    ("cidin", [Pseudo Cstring], ScalarType "cid"),
    ("cidout", [ScalarType "cid"], Pseudo Cstring),
    ("cidr", [ScalarType "inet"], ScalarType "cidr"),
    ("cidr_in", [Pseudo Cstring], ScalarType "cidr"),
    ("cidr_out", [ScalarType "cidr"], Pseudo Cstring),
    ("cidr_send", [ScalarType "cidr"], ScalarType "bytea"),
    ("cidsend", [ScalarType "cid"], ScalarType "bytea"),
    ("circle", [ScalarType "box"], ScalarType "circle"),
    ("circle", [ScalarType "point",ScalarType "float8"], ScalarType "circle"),
    ("circle", [ScalarType "polygon"], ScalarType "circle"),
    ("circle_above", [ScalarType "circle",ScalarType "circle"], ScalarType "bool"),
    ("circle_add_pt", [ScalarType "circle",ScalarType "point"], ScalarType "circle"),
    ("circle_below", [ScalarType "circle",ScalarType "circle"], ScalarType "bool"),
    ("circle_center", [ScalarType "circle"], ScalarType "point"),
    ("circle_contain", [ScalarType "circle",ScalarType "circle"], ScalarType "bool"),
    ("circle_contain_pt", [ScalarType "circle",ScalarType "point"], ScalarType "bool"),
    ("circle_contained", [ScalarType "circle",ScalarType "circle"], ScalarType "bool"),
    ("circle_distance", [ScalarType "circle",ScalarType "circle"], ScalarType "float8"),
    ("circle_div_pt", [ScalarType "circle",ScalarType "point"], ScalarType "circle"),
    ("circle_eq", [ScalarType "circle",ScalarType "circle"], ScalarType "bool"),
    ("circle_ge", [ScalarType "circle",ScalarType "circle"], ScalarType "bool"),
    ("circle_gt", [ScalarType "circle",ScalarType "circle"], ScalarType "bool"),
    ("circle_in", [Pseudo Cstring], ScalarType "circle"),
    ("circle_le", [ScalarType "circle",ScalarType "circle"], ScalarType "bool"),
    ("circle_left", [ScalarType "circle",ScalarType "circle"], ScalarType "bool"),
    ("circle_lt", [ScalarType "circle",ScalarType "circle"], ScalarType "bool"),
    ("circle_mul_pt", [ScalarType "circle",ScalarType "point"], ScalarType "circle"),
    ("circle_ne", [ScalarType "circle",ScalarType "circle"], ScalarType "bool"),
    ("circle_out", [ScalarType "circle"], Pseudo Cstring),
    ("circle_overabove", [ScalarType "circle",ScalarType "circle"], ScalarType "bool"),
    ("circle_overbelow", [ScalarType "circle",ScalarType "circle"], ScalarType "bool"),
    ("circle_overlap", [ScalarType "circle",ScalarType "circle"], ScalarType "bool"),
    ("circle_overleft", [ScalarType "circle",ScalarType "circle"], ScalarType "bool"),
    ("circle_overright", [ScalarType "circle",ScalarType "circle"], ScalarType "bool"),
    ("circle_right", [ScalarType "circle",ScalarType "circle"], ScalarType "bool"),
    ("circle_same", [ScalarType "circle",ScalarType "circle"], ScalarType "bool"),
    ("circle_send", [ScalarType "circle"], ScalarType "bytea"),
    ("circle_sub_pt", [ScalarType "circle",ScalarType "point"], ScalarType "circle"),
    ("clock_timestamp", [], ScalarType "timestamptz"),
    ("close_lb", [ScalarType "line",ScalarType "box"], ScalarType "point"),
    ("close_ls", [ScalarType "line",ScalarType "lseg"], ScalarType "point"),
    ("close_lseg", [ScalarType "lseg",ScalarType "lseg"], ScalarType "point"),
    ("close_pb", [ScalarType "point",ScalarType "box"], ScalarType "point"),
    ("close_pl", [ScalarType "point",ScalarType "line"], ScalarType "point"),
    ("close_ps", [ScalarType "point",ScalarType "lseg"], ScalarType "point"),
    ("close_sb", [ScalarType "lseg",ScalarType "box"], ScalarType "point"),
    ("close_sl", [ScalarType "lseg",ScalarType "line"], ScalarType "point"),
    ("col_description", [ScalarType "oid",ScalarType "int4"], ScalarType "text"),
    ("convert", [ScalarType "bytea",ScalarType "name",ScalarType "name"], ScalarType "bytea"),
    ("convert_from", [ScalarType "bytea",ScalarType "name"], ScalarType "text"),
    ("convert_to", [ScalarType "text",ScalarType "name"], ScalarType "bytea"),
    ("cos", [ScalarType "float8"], ScalarType "float8"),
    ("cot", [ScalarType "float8"], ScalarType "float8"),
    ("cstring_in", [Pseudo Cstring], Pseudo Cstring),
    ("cstring_out", [Pseudo Cstring], Pseudo Cstring),
    ("cstring_send", [Pseudo Cstring], ScalarType "bytea"),
    ("current_database", [], ScalarType "name"),
    ("current_query", [], ScalarType "text"),
    ("current_schema", [], ScalarType "name"),
    ("current_schemas", [ScalarType "bool"], ArrayType (ScalarType "name")),
    ("current_setting", [ScalarType "text"], ScalarType "text"),
    ("current_user", [], ScalarType "name"),
    ("currtid", [ScalarType "oid",ScalarType "tid"], ScalarType "tid"),
    ("currtid2", [ScalarType "text",ScalarType "tid"], ScalarType "tid"),
    ("currval", [ScalarType "regclass"], ScalarType "int8"),
    ("cursor_to_xml", [ScalarType "refcursor",ScalarType "int4",ScalarType "bool",ScalarType "bool",ScalarType "text"], ScalarType "xml"),
    ("cursor_to_xmlschema", [ScalarType "refcursor",ScalarType "bool",ScalarType "bool",ScalarType "text"], ScalarType "xml"),
    ("database_to_xml", [ScalarType "bool",ScalarType "bool",ScalarType "text"], ScalarType "xml"),
    ("database_to_xml_and_xmlschema", [ScalarType "bool",ScalarType "bool",ScalarType "text"], ScalarType "xml"),
    ("database_to_xmlschema", [ScalarType "bool",ScalarType "bool",ScalarType "text"], ScalarType "xml"),
    ("date", [ScalarType "abstime"], ScalarType "date"),
    ("date", [ScalarType "timestamp"], ScalarType "date"),
    ("date", [ScalarType "timestamptz"], ScalarType "date"),
    ("date_cmp", [ScalarType "date",ScalarType "date"], ScalarType "int4"),
    ("date_cmp_timestamp", [ScalarType "date",ScalarType "timestamp"], ScalarType "int4"),
    ("date_cmp_timestamptz", [ScalarType "date",ScalarType "timestamptz"], ScalarType "int4"),
    ("date_eq", [ScalarType "date",ScalarType "date"], ScalarType "bool"),
    ("date_eq_timestamp", [ScalarType "date",ScalarType "timestamp"], ScalarType "bool"),
    ("date_eq_timestamptz", [ScalarType "date",ScalarType "timestamptz"], ScalarType "bool"),
    ("date_ge", [ScalarType "date",ScalarType "date"], ScalarType "bool"),
    ("date_ge_timestamp", [ScalarType "date",ScalarType "timestamp"], ScalarType "bool"),
    ("date_ge_timestamptz", [ScalarType "date",ScalarType "timestamptz"], ScalarType "bool"),
    ("date_gt", [ScalarType "date",ScalarType "date"], ScalarType "bool"),
    ("date_gt_timestamp", [ScalarType "date",ScalarType "timestamp"], ScalarType "bool"),
    ("date_gt_timestamptz", [ScalarType "date",ScalarType "timestamptz"], ScalarType "bool"),
    ("date_in", [Pseudo Cstring], ScalarType "date"),
    ("date_larger", [ScalarType "date",ScalarType "date"], ScalarType "date"),
    ("date_le", [ScalarType "date",ScalarType "date"], ScalarType "bool"),
    ("date_le_timestamp", [ScalarType "date",ScalarType "timestamp"], ScalarType "bool"),
    ("date_le_timestamptz", [ScalarType "date",ScalarType "timestamptz"], ScalarType "bool"),
    ("date_lt", [ScalarType "date",ScalarType "date"], ScalarType "bool"),
    ("date_lt_timestamp", [ScalarType "date",ScalarType "timestamp"], ScalarType "bool"),
    ("date_lt_timestamptz", [ScalarType "date",ScalarType "timestamptz"], ScalarType "bool"),
    ("date_mi", [ScalarType "date",ScalarType "date"], ScalarType "int4"),
    ("date_mi_interval", [ScalarType "date",ScalarType "interval"], ScalarType "timestamp"),
    ("date_mii", [ScalarType "date",ScalarType "int4"], ScalarType "date"),
    ("date_ne", [ScalarType "date",ScalarType "date"], ScalarType "bool"),
    ("date_ne_timestamp", [ScalarType "date",ScalarType "timestamp"], ScalarType "bool"),
    ("date_ne_timestamptz", [ScalarType "date",ScalarType "timestamptz"], ScalarType "bool"),
    ("date_out", [ScalarType "date"], Pseudo Cstring),
    ("date_part", [ScalarType "text",ScalarType "abstime"], ScalarType "float8"),
    ("date_part", [ScalarType "text",ScalarType "date"], ScalarType "float8"),
    ("date_part", [ScalarType "text",ScalarType "interval"], ScalarType "float8"),
    ("date_part", [ScalarType "text",ScalarType "reltime"], ScalarType "float8"),
    ("date_part", [ScalarType "text",ScalarType "time"], ScalarType "float8"),
    ("date_part", [ScalarType "text",ScalarType "timestamp"], ScalarType "float8"),
    ("date_part", [ScalarType "text",ScalarType "timestamptz"], ScalarType "float8"),
    ("date_part", [ScalarType "text",ScalarType "timetz"], ScalarType "float8"),
    ("date_pl_interval", [ScalarType "date",ScalarType "interval"], ScalarType "timestamp"),
    ("date_pli", [ScalarType "date",ScalarType "int4"], ScalarType "date"),
    ("date_send", [ScalarType "date"], ScalarType "bytea"),
    ("date_smaller", [ScalarType "date",ScalarType "date"], ScalarType "date"),
    ("date_trunc", [ScalarType "text",ScalarType "interval"], ScalarType "interval"),
    ("date_trunc", [ScalarType "text",ScalarType "timestamp"], ScalarType "timestamp"),
    ("date_trunc", [ScalarType "text",ScalarType "timestamptz"], ScalarType "timestamptz"),
    ("datetime_pl", [ScalarType "date",ScalarType "time"], ScalarType "timestamp"),
    ("datetimetz_pl", [ScalarType "date",ScalarType "timetz"], ScalarType "timestamptz"),
    ("dcbrt", [ScalarType "float8"], ScalarType "float8"),
    ("decode", [ScalarType "text",ScalarType "text"], ScalarType "bytea"),
    ("degrees", [ScalarType "float8"], ScalarType "float8"),
    ("dexp", [ScalarType "float8"], ScalarType "float8"),
    ("diagonal", [ScalarType "box"], ScalarType "lseg"),
    ("diameter", [ScalarType "circle"], ScalarType "float8"),
    ("dist_cpoly", [ScalarType "circle",ScalarType "polygon"], ScalarType "float8"),
    ("dist_lb", [ScalarType "line",ScalarType "box"], ScalarType "float8"),
    ("dist_pb", [ScalarType "point",ScalarType "box"], ScalarType "float8"),
    ("dist_pc", [ScalarType "point",ScalarType "circle"], ScalarType "float8"),
    ("dist_pl", [ScalarType "point",ScalarType "line"], ScalarType "float8"),
    ("dist_ppath", [ScalarType "point",ScalarType "path"], ScalarType "float8"),
    ("dist_ps", [ScalarType "point",ScalarType "lseg"], ScalarType "float8"),
    ("dist_sb", [ScalarType "lseg",ScalarType "box"], ScalarType "float8"),
    ("dist_sl", [ScalarType "lseg",ScalarType "line"], ScalarType "float8"),
    ("div", [ScalarType "numeric",ScalarType "numeric"], ScalarType "numeric"),
    ("dlog1", [ScalarType "float8"], ScalarType "float8"),
    ("dlog10", [ScalarType "float8"], ScalarType "float8"),
    ("domain_in", [Pseudo Cstring,ScalarType "oid",ScalarType "int4"], Pseudo Any),
    ("dpow", [ScalarType "float8",ScalarType "float8"], ScalarType "float8"),
    ("dround", [ScalarType "float8"], ScalarType "float8"),
    ("dsqrt", [ScalarType "float8"], ScalarType "float8"),
    ("dtrunc", [ScalarType "float8"], ScalarType "float8"),
    ("encode", [ScalarType "bytea",ScalarType "text"], ScalarType "text"),
    ("enum_cmp", [Pseudo AnyEnum,Pseudo AnyEnum], ScalarType "int4"),
    ("enum_eq", [Pseudo AnyEnum,Pseudo AnyEnum], ScalarType "bool"),
    ("enum_first", [Pseudo AnyEnum], Pseudo AnyEnum),
    ("enum_ge", [Pseudo AnyEnum,Pseudo AnyEnum], ScalarType "bool"),
    ("enum_gt", [Pseudo AnyEnum,Pseudo AnyEnum], ScalarType "bool"),
    ("enum_in", [Pseudo Cstring,ScalarType "oid"], Pseudo AnyEnum),
    ("enum_larger", [Pseudo AnyEnum,Pseudo AnyEnum], Pseudo AnyEnum),
    ("enum_last", [Pseudo AnyEnum], Pseudo AnyEnum),
    ("enum_le", [Pseudo AnyEnum,Pseudo AnyEnum], ScalarType "bool"),
    ("enum_lt", [Pseudo AnyEnum,Pseudo AnyEnum], ScalarType "bool"),
    ("enum_ne", [Pseudo AnyEnum,Pseudo AnyEnum], ScalarType "bool"),
    ("enum_out", [Pseudo AnyEnum], Pseudo Cstring),
    ("enum_range", [Pseudo AnyEnum], Pseudo AnyArray),
    ("enum_range", [Pseudo AnyEnum,Pseudo AnyEnum], Pseudo AnyArray),
    ("enum_recv", [Pseudo Cstring,ScalarType "oid"], Pseudo AnyEnum),
    ("enum_send", [Pseudo AnyEnum], ScalarType "bytea"),
    ("enum_smaller", [Pseudo AnyEnum,Pseudo AnyEnum], Pseudo AnyEnum),
    ("exp", [ScalarType "float8"], ScalarType "float8"),
    ("exp", [ScalarType "numeric"], ScalarType "numeric"),
    ("factorial", [ScalarType "int8"], ScalarType "numeric"),
    ("family", [ScalarType "inet"], ScalarType "int4"),
    ("flatfile_update_trigger", [], Pseudo Trigger),
    ("float4", [ScalarType "float8"], ScalarType "float4"),
    ("float4", [ScalarType "int2"], ScalarType "float4"),
    ("float4", [ScalarType "int4"], ScalarType "float4"),
    ("float4", [ScalarType "int8"], ScalarType "float4"),
    ("float4", [ScalarType "numeric"], ScalarType "float4"),
    ("float48div", [ScalarType "float4",ScalarType "float8"], ScalarType "float8"),
    ("float48eq", [ScalarType "float4",ScalarType "float8"], ScalarType "bool"),
    ("float48ge", [ScalarType "float4",ScalarType "float8"], ScalarType "bool"),
    ("float48gt", [ScalarType "float4",ScalarType "float8"], ScalarType "bool"),
    ("float48le", [ScalarType "float4",ScalarType "float8"], ScalarType "bool"),
    ("float48lt", [ScalarType "float4",ScalarType "float8"], ScalarType "bool"),
    ("float48mi", [ScalarType "float4",ScalarType "float8"], ScalarType "float8"),
    ("float48mul", [ScalarType "float4",ScalarType "float8"], ScalarType "float8"),
    ("float48ne", [ScalarType "float4",ScalarType "float8"], ScalarType "bool"),
    ("float48pl", [ScalarType "float4",ScalarType "float8"], ScalarType "float8"),
    ("float4_accum", [ArrayType (ScalarType "float8"),ScalarType "float4"], ArrayType (ScalarType "float8")),
    ("float4abs", [ScalarType "float4"], ScalarType "float4"),
    ("float4div", [ScalarType "float4",ScalarType "float4"], ScalarType "float4"),
    ("float4eq", [ScalarType "float4",ScalarType "float4"], ScalarType "bool"),
    ("float4ge", [ScalarType "float4",ScalarType "float4"], ScalarType "bool"),
    ("float4gt", [ScalarType "float4",ScalarType "float4"], ScalarType "bool"),
    ("float4in", [Pseudo Cstring], ScalarType "float4"),
    ("float4larger", [ScalarType "float4",ScalarType "float4"], ScalarType "float4"),
    ("float4le", [ScalarType "float4",ScalarType "float4"], ScalarType "bool"),
    ("float4lt", [ScalarType "float4",ScalarType "float4"], ScalarType "bool"),
    ("float4mi", [ScalarType "float4",ScalarType "float4"], ScalarType "float4"),
    ("float4mul", [ScalarType "float4",ScalarType "float4"], ScalarType "float4"),
    ("float4ne", [ScalarType "float4",ScalarType "float4"], ScalarType "bool"),
    ("float4out", [ScalarType "float4"], Pseudo Cstring),
    ("float4pl", [ScalarType "float4",ScalarType "float4"], ScalarType "float4"),
    ("float4send", [ScalarType "float4"], ScalarType "bytea"),
    ("float4smaller", [ScalarType "float4",ScalarType "float4"], ScalarType "float4"),
    ("float4um", [ScalarType "float4"], ScalarType "float4"),
    ("float4up", [ScalarType "float4"], ScalarType "float4"),
    ("float8", [ScalarType "float4"], ScalarType "float8"),
    ("float8", [ScalarType "int2"], ScalarType "float8"),
    ("float8", [ScalarType "int4"], ScalarType "float8"),
    ("float8", [ScalarType "int8"], ScalarType "float8"),
    ("float8", [ScalarType "numeric"], ScalarType "float8"),
    ("float84div", [ScalarType "float8",ScalarType "float4"], ScalarType "float8"),
    ("float84eq", [ScalarType "float8",ScalarType "float4"], ScalarType "bool"),
    ("float84ge", [ScalarType "float8",ScalarType "float4"], ScalarType "bool"),
    ("float84gt", [ScalarType "float8",ScalarType "float4"], ScalarType "bool"),
    ("float84le", [ScalarType "float8",ScalarType "float4"], ScalarType "bool"),
    ("float84lt", [ScalarType "float8",ScalarType "float4"], ScalarType "bool"),
    ("float84mi", [ScalarType "float8",ScalarType "float4"], ScalarType "float8"),
    ("float84mul", [ScalarType "float8",ScalarType "float4"], ScalarType "float8"),
    ("float84ne", [ScalarType "float8",ScalarType "float4"], ScalarType "bool"),
    ("float84pl", [ScalarType "float8",ScalarType "float4"], ScalarType "float8"),
    ("float8_accum", [ArrayType (ScalarType "float8"),ScalarType "float8"], ArrayType (ScalarType "float8")),
    ("float8_avg", [ArrayType (ScalarType "float8")], ScalarType "float8"),
    ("float8_corr", [ArrayType (ScalarType "float8")], ScalarType "float8"),
    ("float8_covar_pop", [ArrayType (ScalarType "float8")], ScalarType "float8"),
    ("float8_covar_samp", [ArrayType (ScalarType "float8")], ScalarType "float8"),
    ("float8_regr_accum", [ArrayType (ScalarType "float8"),ScalarType "float8",ScalarType "float8"], ArrayType (ScalarType "float8")),
    ("float8_regr_avgx", [ArrayType (ScalarType "float8")], ScalarType "float8"),
    ("float8_regr_avgy", [ArrayType (ScalarType "float8")], ScalarType "float8"),
    ("float8_regr_intercept", [ArrayType (ScalarType "float8")], ScalarType "float8"),
    ("float8_regr_r2", [ArrayType (ScalarType "float8")], ScalarType "float8"),
    ("float8_regr_slope", [ArrayType (ScalarType "float8")], ScalarType "float8"),
    ("float8_regr_sxx", [ArrayType (ScalarType "float8")], ScalarType "float8"),
    ("float8_regr_sxy", [ArrayType (ScalarType "float8")], ScalarType "float8"),
    ("float8_regr_syy", [ArrayType (ScalarType "float8")], ScalarType "float8"),
    ("float8_stddev_pop", [ArrayType (ScalarType "float8")], ScalarType "float8"),
    ("float8_stddev_samp", [ArrayType (ScalarType "float8")], ScalarType "float8"),
    ("float8_var_pop", [ArrayType (ScalarType "float8")], ScalarType "float8"),
    ("float8_var_samp", [ArrayType (ScalarType "float8")], ScalarType "float8"),
    ("float8abs", [ScalarType "float8"], ScalarType "float8"),
    ("float8div", [ScalarType "float8",ScalarType "float8"], ScalarType "float8"),
    ("float8eq", [ScalarType "float8",ScalarType "float8"], ScalarType "bool"),
    ("float8ge", [ScalarType "float8",ScalarType "float8"], ScalarType "bool"),
    ("float8gt", [ScalarType "float8",ScalarType "float8"], ScalarType "bool"),
    ("float8in", [Pseudo Cstring], ScalarType "float8"),
    ("float8larger", [ScalarType "float8",ScalarType "float8"], ScalarType "float8"),
    ("float8le", [ScalarType "float8",ScalarType "float8"], ScalarType "bool"),
    ("float8lt", [ScalarType "float8",ScalarType "float8"], ScalarType "bool"),
    ("float8mi", [ScalarType "float8",ScalarType "float8"], ScalarType "float8"),
    ("float8mul", [ScalarType "float8",ScalarType "float8"], ScalarType "float8"),
    ("float8ne", [ScalarType "float8",ScalarType "float8"], ScalarType "bool"),
    ("float8out", [ScalarType "float8"], Pseudo Cstring),
    ("float8pl", [ScalarType "float8",ScalarType "float8"], ScalarType "float8"),
    ("float8send", [ScalarType "float8"], ScalarType "bytea"),
    ("float8smaller", [ScalarType "float8",ScalarType "float8"], ScalarType "float8"),
    ("float8um", [ScalarType "float8"], ScalarType "float8"),
    ("float8up", [ScalarType "float8"], ScalarType "float8"),
    ("floor", [ScalarType "float8"], ScalarType "float8"),
    ("floor", [ScalarType "numeric"], ScalarType "numeric"),
    ("flt4_mul_cash", [ScalarType "float4",ScalarType "money"], ScalarType "money"),
    ("flt8_mul_cash", [ScalarType "float8",ScalarType "money"], ScalarType "money"),
    ("fmgr_c_validator", [ScalarType "oid"], Pseudo Void),
    ("fmgr_internal_validator", [ScalarType "oid"], Pseudo Void),
    ("fmgr_sql_validator", [ScalarType "oid"], Pseudo Void),
    ("format_type", [ScalarType "oid",ScalarType "int4"], ScalarType "text"),
    ("generate_series", [ScalarType "int4",ScalarType "int4"], SetOfType (ScalarType "int4")),
    ("generate_series", [ScalarType "int4",ScalarType "int4",ScalarType "int4"], SetOfType (ScalarType "int4")),
    ("generate_series", [ScalarType "int8",ScalarType "int8"], SetOfType (ScalarType "int8")),
    ("generate_series", [ScalarType "int8",ScalarType "int8",ScalarType "int8"], SetOfType (ScalarType "int8")),
    ("generate_series", [ScalarType "timestamp",ScalarType "timestamp",ScalarType "interval"], SetOfType (ScalarType "timestamp")),
    ("generate_series", [ScalarType "timestamptz",ScalarType "timestamptz",ScalarType "interval"], SetOfType (ScalarType "timestamptz")),
    ("generate_subscripts", [Pseudo AnyArray,ScalarType "int4"], SetOfType (ScalarType "int4")),
    ("generate_subscripts", [Pseudo AnyArray,ScalarType "int4",ScalarType "bool"], SetOfType (ScalarType "int4")),
    ("get_bit", [ScalarType "bytea",ScalarType "int4"], ScalarType "int4"),
    ("get_byte", [ScalarType "bytea",ScalarType "int4"], ScalarType "int4"),
    ("get_current_ts_config", [], ScalarType "regconfig"),
    ("getdatabaseencoding", [], ScalarType "name"),
    ("getpgusername", [], ScalarType "name"),
    ("gin_cmp_tslexeme", [ScalarType "text",ScalarType "text"], ScalarType "int4"),
    ("ginoptions", [ArrayType (ScalarType "text"),ScalarType "bool"], ScalarType "bytea"),
    ("gistoptions", [ArrayType (ScalarType "text"),ScalarType "bool"], ScalarType "bytea"),
    ("gtsvectorin", [Pseudo Cstring], ScalarType "gtsvector"),
    ("gtsvectorout", [ScalarType "gtsvector"], Pseudo Cstring),
    ("has_any_column_privilege", [ScalarType "name",ScalarType "oid",ScalarType "text"], ScalarType "bool"),
    ("has_any_column_privilege", [ScalarType "name",ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("has_any_column_privilege", [ScalarType "oid",ScalarType "oid",ScalarType "text"], ScalarType "bool"),
    ("has_any_column_privilege", [ScalarType "oid",ScalarType "text"], ScalarType "bool"),
    ("has_any_column_privilege", [ScalarType "oid",ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("has_any_column_privilege", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("has_column_privilege", [ScalarType "name",ScalarType "oid",ScalarType "int2",ScalarType "text"], ScalarType "bool"),
    ("has_column_privilege", [ScalarType "name",ScalarType "oid",ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("has_column_privilege", [ScalarType "name",ScalarType "text",ScalarType "int2",ScalarType "text"], ScalarType "bool"),
    ("has_column_privilege", [ScalarType "name",ScalarType "text",ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("has_column_privilege", [ScalarType "oid",ScalarType "int2",ScalarType "text"], ScalarType "bool"),
    ("has_column_privilege", [ScalarType "oid",ScalarType "oid",ScalarType "int2",ScalarType "text"], ScalarType "bool"),
    ("has_column_privilege", [ScalarType "oid",ScalarType "oid",ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("has_column_privilege", [ScalarType "oid",ScalarType "text",ScalarType "int2",ScalarType "text"], ScalarType "bool"),
    ("has_column_privilege", [ScalarType "oid",ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("has_column_privilege", [ScalarType "oid",ScalarType "text",ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("has_column_privilege", [ScalarType "text",ScalarType "int2",ScalarType "text"], ScalarType "bool"),
    ("has_column_privilege", [ScalarType "text",ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("has_database_privilege", [ScalarType "name",ScalarType "oid",ScalarType "text"], ScalarType "bool"),
    ("has_database_privilege", [ScalarType "name",ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("has_database_privilege", [ScalarType "oid",ScalarType "oid",ScalarType "text"], ScalarType "bool"),
    ("has_database_privilege", [ScalarType "oid",ScalarType "text"], ScalarType "bool"),
    ("has_database_privilege", [ScalarType "oid",ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("has_database_privilege", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("has_foreign_data_wrapper_privilege", [ScalarType "name",ScalarType "oid",ScalarType "text"], ScalarType "bool"),
    ("has_foreign_data_wrapper_privilege", [ScalarType "name",ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("has_foreign_data_wrapper_privilege", [ScalarType "oid",ScalarType "oid",ScalarType "text"], ScalarType "bool"),
    ("has_foreign_data_wrapper_privilege", [ScalarType "oid",ScalarType "text"], ScalarType "bool"),
    ("has_foreign_data_wrapper_privilege", [ScalarType "oid",ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("has_foreign_data_wrapper_privilege", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("has_function_privilege", [ScalarType "name",ScalarType "oid",ScalarType "text"], ScalarType "bool"),
    ("has_function_privilege", [ScalarType "name",ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("has_function_privilege", [ScalarType "oid",ScalarType "oid",ScalarType "text"], ScalarType "bool"),
    ("has_function_privilege", [ScalarType "oid",ScalarType "text"], ScalarType "bool"),
    ("has_function_privilege", [ScalarType "oid",ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("has_function_privilege", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("has_language_privilege", [ScalarType "name",ScalarType "oid",ScalarType "text"], ScalarType "bool"),
    ("has_language_privilege", [ScalarType "name",ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("has_language_privilege", [ScalarType "oid",ScalarType "oid",ScalarType "text"], ScalarType "bool"),
    ("has_language_privilege", [ScalarType "oid",ScalarType "text"], ScalarType "bool"),
    ("has_language_privilege", [ScalarType "oid",ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("has_language_privilege", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("has_schema_privilege", [ScalarType "name",ScalarType "oid",ScalarType "text"], ScalarType "bool"),
    ("has_schema_privilege", [ScalarType "name",ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("has_schema_privilege", [ScalarType "oid",ScalarType "oid",ScalarType "text"], ScalarType "bool"),
    ("has_schema_privilege", [ScalarType "oid",ScalarType "text"], ScalarType "bool"),
    ("has_schema_privilege", [ScalarType "oid",ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("has_schema_privilege", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("has_server_privilege", [ScalarType "name",ScalarType "oid",ScalarType "text"], ScalarType "bool"),
    ("has_server_privilege", [ScalarType "name",ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("has_server_privilege", [ScalarType "oid",ScalarType "oid",ScalarType "text"], ScalarType "bool"),
    ("has_server_privilege", [ScalarType "oid",ScalarType "text"], ScalarType "bool"),
    ("has_server_privilege", [ScalarType "oid",ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("has_server_privilege", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("has_table_privilege", [ScalarType "name",ScalarType "oid",ScalarType "text"], ScalarType "bool"),
    ("has_table_privilege", [ScalarType "name",ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("has_table_privilege", [ScalarType "oid",ScalarType "oid",ScalarType "text"], ScalarType "bool"),
    ("has_table_privilege", [ScalarType "oid",ScalarType "text"], ScalarType "bool"),
    ("has_table_privilege", [ScalarType "oid",ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("has_table_privilege", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("has_tablespace_privilege", [ScalarType "name",ScalarType "oid",ScalarType "text"], ScalarType "bool"),
    ("has_tablespace_privilege", [ScalarType "name",ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("has_tablespace_privilege", [ScalarType "oid",ScalarType "oid",ScalarType "text"], ScalarType "bool"),
    ("has_tablespace_privilege", [ScalarType "oid",ScalarType "text"], ScalarType "bool"),
    ("has_tablespace_privilege", [ScalarType "oid",ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("has_tablespace_privilege", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("hash_aclitem", [ScalarType "aclitem"], ScalarType "int4"),
    ("hash_numeric", [ScalarType "numeric"], ScalarType "int4"),
    ("hashbpchar", [ScalarType "bpchar"], ScalarType "int4"),
    ("hashchar", [ScalarType "char"], ScalarType "int4"),
    ("hashenum", [Pseudo AnyEnum], ScalarType "int4"),
    ("hashfloat4", [ScalarType "float4"], ScalarType "int4"),
    ("hashfloat8", [ScalarType "float8"], ScalarType "int4"),
    ("hashinet", [ScalarType "inet"], ScalarType "int4"),
    ("hashint2", [ScalarType "int2"], ScalarType "int4"),
    ("hashint2vector", [ScalarType "int2vector"], ScalarType "int4"),
    ("hashint4", [ScalarType "int4"], ScalarType "int4"),
    ("hashint8", [ScalarType "int8"], ScalarType "int4"),
    ("hashmacaddr", [ScalarType "macaddr"], ScalarType "int4"),
    ("hashname", [ScalarType "name"], ScalarType "int4"),
    ("hashoid", [ScalarType "oid"], ScalarType "int4"),
    ("hashoidvector", [ScalarType "oidvector"], ScalarType "int4"),
    ("hashoptions", [ArrayType (ScalarType "text"),ScalarType "bool"], ScalarType "bytea"),
    ("hashtext", [ScalarType "text"], ScalarType "int4"),
    ("height", [ScalarType "box"], ScalarType "float8"),
    ("host", [ScalarType "inet"], ScalarType "text"),
    ("hostmask", [ScalarType "inet"], ScalarType "inet"),
    ("inet_client_addr", [], ScalarType "inet"),
    ("inet_client_port", [], ScalarType "int4"),
    ("inet_in", [Pseudo Cstring], ScalarType "inet"),
    ("inet_out", [ScalarType "inet"], Pseudo Cstring),
    ("inet_send", [ScalarType "inet"], ScalarType "bytea"),
    ("inet_server_addr", [], ScalarType "inet"),
    ("inet_server_port", [], ScalarType "int4"),
    ("inetand", [ScalarType "inet",ScalarType "inet"], ScalarType "inet"),
    ("inetmi", [ScalarType "inet",ScalarType "inet"], ScalarType "int8"),
    ("inetmi_int8", [ScalarType "inet",ScalarType "int8"], ScalarType "inet"),
    ("inetnot", [ScalarType "inet"], ScalarType "inet"),
    ("inetor", [ScalarType "inet",ScalarType "inet"], ScalarType "inet"),
    ("inetpl", [ScalarType "inet",ScalarType "int8"], ScalarType "inet"),
    ("initcap", [ScalarType "text"], ScalarType "text"),
    ("int2", [ScalarType "float4"], ScalarType "int2"),
    ("int2", [ScalarType "float8"], ScalarType "int2"),
    ("int2", [ScalarType "int4"], ScalarType "int2"),
    ("int2", [ScalarType "int8"], ScalarType "int2"),
    ("int2", [ScalarType "numeric"], ScalarType "int2"),
    ("int24div", [ScalarType "int2",ScalarType "int4"], ScalarType "int4"),
    ("int24eq", [ScalarType "int2",ScalarType "int4"], ScalarType "bool"),
    ("int24ge", [ScalarType "int2",ScalarType "int4"], ScalarType "bool"),
    ("int24gt", [ScalarType "int2",ScalarType "int4"], ScalarType "bool"),
    ("int24le", [ScalarType "int2",ScalarType "int4"], ScalarType "bool"),
    ("int24lt", [ScalarType "int2",ScalarType "int4"], ScalarType "bool"),
    ("int24mi", [ScalarType "int2",ScalarType "int4"], ScalarType "int4"),
    ("int24mul", [ScalarType "int2",ScalarType "int4"], ScalarType "int4"),
    ("int24ne", [ScalarType "int2",ScalarType "int4"], ScalarType "bool"),
    ("int24pl", [ScalarType "int2",ScalarType "int4"], ScalarType "int4"),
    ("int28div", [ScalarType "int2",ScalarType "int8"], ScalarType "int8"),
    ("int28eq", [ScalarType "int2",ScalarType "int8"], ScalarType "bool"),
    ("int28ge", [ScalarType "int2",ScalarType "int8"], ScalarType "bool"),
    ("int28gt", [ScalarType "int2",ScalarType "int8"], ScalarType "bool"),
    ("int28le", [ScalarType "int2",ScalarType "int8"], ScalarType "bool"),
    ("int28lt", [ScalarType "int2",ScalarType "int8"], ScalarType "bool"),
    ("int28mi", [ScalarType "int2",ScalarType "int8"], ScalarType "int8"),
    ("int28mul", [ScalarType "int2",ScalarType "int8"], ScalarType "int8"),
    ("int28ne", [ScalarType "int2",ScalarType "int8"], ScalarType "bool"),
    ("int28pl", [ScalarType "int2",ScalarType "int8"], ScalarType "int8"),
    ("int2_accum", [ArrayType (ScalarType "numeric"),ScalarType "int2"], ArrayType (ScalarType "numeric")),
    ("int2_avg_accum", [ArrayType (ScalarType "int8"),ScalarType "int2"], ArrayType (ScalarType "int8")),
    ("int2_mul_cash", [ScalarType "int2",ScalarType "money"], ScalarType "money"),
    ("int2_sum", [ScalarType "int8",ScalarType "int2"], ScalarType "int8"),
    ("int2abs", [ScalarType "int2"], ScalarType "int2"),
    ("int2and", [ScalarType "int2",ScalarType "int2"], ScalarType "int2"),
    ("int2div", [ScalarType "int2",ScalarType "int2"], ScalarType "int2"),
    ("int2eq", [ScalarType "int2",ScalarType "int2"], ScalarType "bool"),
    ("int2ge", [ScalarType "int2",ScalarType "int2"], ScalarType "bool"),
    ("int2gt", [ScalarType "int2",ScalarType "int2"], ScalarType "bool"),
    ("int2in", [Pseudo Cstring], ScalarType "int2"),
    ("int2larger", [ScalarType "int2",ScalarType "int2"], ScalarType "int2"),
    ("int2le", [ScalarType "int2",ScalarType "int2"], ScalarType "bool"),
    ("int2lt", [ScalarType "int2",ScalarType "int2"], ScalarType "bool"),
    ("int2mi", [ScalarType "int2",ScalarType "int2"], ScalarType "int2"),
    ("int2mod", [ScalarType "int2",ScalarType "int2"], ScalarType "int2"),
    ("int2mul", [ScalarType "int2",ScalarType "int2"], ScalarType "int2"),
    ("int2ne", [ScalarType "int2",ScalarType "int2"], ScalarType "bool"),
    ("int2not", [ScalarType "int2"], ScalarType "int2"),
    ("int2or", [ScalarType "int2",ScalarType "int2"], ScalarType "int2"),
    ("int2out", [ScalarType "int2"], Pseudo Cstring),
    ("int2pl", [ScalarType "int2",ScalarType "int2"], ScalarType "int2"),
    ("int2send", [ScalarType "int2"], ScalarType "bytea"),
    ("int2shl", [ScalarType "int2",ScalarType "int4"], ScalarType "int2"),
    ("int2shr", [ScalarType "int2",ScalarType "int4"], ScalarType "int2"),
    ("int2smaller", [ScalarType "int2",ScalarType "int2"], ScalarType "int2"),
    ("int2um", [ScalarType "int2"], ScalarType "int2"),
    ("int2up", [ScalarType "int2"], ScalarType "int2"),
    ("int2vectoreq", [ScalarType "int2vector",ScalarType "int2vector"], ScalarType "bool"),
    ("int2vectorin", [Pseudo Cstring], ScalarType "int2vector"),
    ("int2vectorout", [ScalarType "int2vector"], Pseudo Cstring),
    ("int2vectorsend", [ScalarType "int2vector"], ScalarType "bytea"),
    ("int2xor", [ScalarType "int2",ScalarType "int2"], ScalarType "int2"),
    ("int4", [ScalarType "bit"], ScalarType "int4"),
    ("int4", [ScalarType "bool"], ScalarType "int4"),
    ("int4", [ScalarType "char"], ScalarType "int4"),
    ("int4", [ScalarType "float4"], ScalarType "int4"),
    ("int4", [ScalarType "float8"], ScalarType "int4"),
    ("int4", [ScalarType "int2"], ScalarType "int4"),
    ("int4", [ScalarType "int8"], ScalarType "int4"),
    ("int4", [ScalarType "numeric"], ScalarType "int4"),
    ("int42div", [ScalarType "int4",ScalarType "int2"], ScalarType "int4"),
    ("int42eq", [ScalarType "int4",ScalarType "int2"], ScalarType "bool"),
    ("int42ge", [ScalarType "int4",ScalarType "int2"], ScalarType "bool"),
    ("int42gt", [ScalarType "int4",ScalarType "int2"], ScalarType "bool"),
    ("int42le", [ScalarType "int4",ScalarType "int2"], ScalarType "bool"),
    ("int42lt", [ScalarType "int4",ScalarType "int2"], ScalarType "bool"),
    ("int42mi", [ScalarType "int4",ScalarType "int2"], ScalarType "int4"),
    ("int42mul", [ScalarType "int4",ScalarType "int2"], ScalarType "int4"),
    ("int42ne", [ScalarType "int4",ScalarType "int2"], ScalarType "bool"),
    ("int42pl", [ScalarType "int4",ScalarType "int2"], ScalarType "int4"),
    ("int48div", [ScalarType "int4",ScalarType "int8"], ScalarType "int8"),
    ("int48eq", [ScalarType "int4",ScalarType "int8"], ScalarType "bool"),
    ("int48ge", [ScalarType "int4",ScalarType "int8"], ScalarType "bool"),
    ("int48gt", [ScalarType "int4",ScalarType "int8"], ScalarType "bool"),
    ("int48le", [ScalarType "int4",ScalarType "int8"], ScalarType "bool"),
    ("int48lt", [ScalarType "int4",ScalarType "int8"], ScalarType "bool"),
    ("int48mi", [ScalarType "int4",ScalarType "int8"], ScalarType "int8"),
    ("int48mul", [ScalarType "int4",ScalarType "int8"], ScalarType "int8"),
    ("int48ne", [ScalarType "int4",ScalarType "int8"], ScalarType "bool"),
    ("int48pl", [ScalarType "int4",ScalarType "int8"], ScalarType "int8"),
    ("int4_accum", [ArrayType (ScalarType "numeric"),ScalarType "int4"], ArrayType (ScalarType "numeric")),
    ("int4_avg_accum", [ArrayType (ScalarType "int8"),ScalarType "int4"], ArrayType (ScalarType "int8")),
    ("int4_mul_cash", [ScalarType "int4",ScalarType "money"], ScalarType "money"),
    ("int4_sum", [ScalarType "int8",ScalarType "int4"], ScalarType "int8"),
    ("int4abs", [ScalarType "int4"], ScalarType "int4"),
    ("int4and", [ScalarType "int4",ScalarType "int4"], ScalarType "int4"),
    ("int4div", [ScalarType "int4",ScalarType "int4"], ScalarType "int4"),
    ("int4eq", [ScalarType "int4",ScalarType "int4"], ScalarType "bool"),
    ("int4ge", [ScalarType "int4",ScalarType "int4"], ScalarType "bool"),
    ("int4gt", [ScalarType "int4",ScalarType "int4"], ScalarType "bool"),
    ("int4in", [Pseudo Cstring], ScalarType "int4"),
    ("int4inc", [ScalarType "int4"], ScalarType "int4"),
    ("int4larger", [ScalarType "int4",ScalarType "int4"], ScalarType "int4"),
    ("int4le", [ScalarType "int4",ScalarType "int4"], ScalarType "bool"),
    ("int4lt", [ScalarType "int4",ScalarType "int4"], ScalarType "bool"),
    ("int4mi", [ScalarType "int4",ScalarType "int4"], ScalarType "int4"),
    ("int4mod", [ScalarType "int4",ScalarType "int4"], ScalarType "int4"),
    ("int4mul", [ScalarType "int4",ScalarType "int4"], ScalarType "int4"),
    ("int4ne", [ScalarType "int4",ScalarType "int4"], ScalarType "bool"),
    ("int4not", [ScalarType "int4"], ScalarType "int4"),
    ("int4or", [ScalarType "int4",ScalarType "int4"], ScalarType "int4"),
    ("int4out", [ScalarType "int4"], Pseudo Cstring),
    ("int4pl", [ScalarType "int4",ScalarType "int4"], ScalarType "int4"),
    ("int4send", [ScalarType "int4"], ScalarType "bytea"),
    ("int4shl", [ScalarType "int4",ScalarType "int4"], ScalarType "int4"),
    ("int4shr", [ScalarType "int4",ScalarType "int4"], ScalarType "int4"),
    ("int4smaller", [ScalarType "int4",ScalarType "int4"], ScalarType "int4"),
    ("int4um", [ScalarType "int4"], ScalarType "int4"),
    ("int4up", [ScalarType "int4"], ScalarType "int4"),
    ("int4xor", [ScalarType "int4",ScalarType "int4"], ScalarType "int4"),
    ("int8", [ScalarType "bit"], ScalarType "int8"),
    ("int8", [ScalarType "float4"], ScalarType "int8"),
    ("int8", [ScalarType "float8"], ScalarType "int8"),
    ("int8", [ScalarType "int2"], ScalarType "int8"),
    ("int8", [ScalarType "int4"], ScalarType "int8"),
    ("int8", [ScalarType "numeric"], ScalarType "int8"),
    ("int8", [ScalarType "oid"], ScalarType "int8"),
    ("int82div", [ScalarType "int8",ScalarType "int2"], ScalarType "int8"),
    ("int82eq", [ScalarType "int8",ScalarType "int2"], ScalarType "bool"),
    ("int82ge", [ScalarType "int8",ScalarType "int2"], ScalarType "bool"),
    ("int82gt", [ScalarType "int8",ScalarType "int2"], ScalarType "bool"),
    ("int82le", [ScalarType "int8",ScalarType "int2"], ScalarType "bool"),
    ("int82lt", [ScalarType "int8",ScalarType "int2"], ScalarType "bool"),
    ("int82mi", [ScalarType "int8",ScalarType "int2"], ScalarType "int8"),
    ("int82mul", [ScalarType "int8",ScalarType "int2"], ScalarType "int8"),
    ("int82ne", [ScalarType "int8",ScalarType "int2"], ScalarType "bool"),
    ("int82pl", [ScalarType "int8",ScalarType "int2"], ScalarType "int8"),
    ("int84div", [ScalarType "int8",ScalarType "int4"], ScalarType "int8"),
    ("int84eq", [ScalarType "int8",ScalarType "int4"], ScalarType "bool"),
    ("int84ge", [ScalarType "int8",ScalarType "int4"], ScalarType "bool"),
    ("int84gt", [ScalarType "int8",ScalarType "int4"], ScalarType "bool"),
    ("int84le", [ScalarType "int8",ScalarType "int4"], ScalarType "bool"),
    ("int84lt", [ScalarType "int8",ScalarType "int4"], ScalarType "bool"),
    ("int84mi", [ScalarType "int8",ScalarType "int4"], ScalarType "int8"),
    ("int84mul", [ScalarType "int8",ScalarType "int4"], ScalarType "int8"),
    ("int84ne", [ScalarType "int8",ScalarType "int4"], ScalarType "bool"),
    ("int84pl", [ScalarType "int8",ScalarType "int4"], ScalarType "int8"),
    ("int8_accum", [ArrayType (ScalarType "numeric"),ScalarType "int8"], ArrayType (ScalarType "numeric")),
    ("int8_avg", [ArrayType (ScalarType "int8")], ScalarType "numeric"),
    ("int8_avg_accum", [ArrayType (ScalarType "numeric"),ScalarType "int8"], ArrayType (ScalarType "numeric")),
    ("int8_sum", [ScalarType "numeric",ScalarType "int8"], ScalarType "numeric"),
    ("int8abs", [ScalarType "int8"], ScalarType "int8"),
    ("int8and", [ScalarType "int8",ScalarType "int8"], ScalarType "int8"),
    ("int8div", [ScalarType "int8",ScalarType "int8"], ScalarType "int8"),
    ("int8eq", [ScalarType "int8",ScalarType "int8"], ScalarType "bool"),
    ("int8ge", [ScalarType "int8",ScalarType "int8"], ScalarType "bool"),
    ("int8gt", [ScalarType "int8",ScalarType "int8"], ScalarType "bool"),
    ("int8in", [Pseudo Cstring], ScalarType "int8"),
    ("int8inc", [ScalarType "int8"], ScalarType "int8"),
    ("int8inc_any", [ScalarType "int8",Pseudo Any], ScalarType "int8"),
    ("int8inc_float8_float8", [ScalarType "int8",ScalarType "float8",ScalarType "float8"], ScalarType "int8"),
    ("int8larger", [ScalarType "int8",ScalarType "int8"], ScalarType "int8"),
    ("int8le", [ScalarType "int8",ScalarType "int8"], ScalarType "bool"),
    ("int8lt", [ScalarType "int8",ScalarType "int8"], ScalarType "bool"),
    ("int8mi", [ScalarType "int8",ScalarType "int8"], ScalarType "int8"),
    ("int8mod", [ScalarType "int8",ScalarType "int8"], ScalarType "int8"),
    ("int8mul", [ScalarType "int8",ScalarType "int8"], ScalarType "int8"),
    ("int8ne", [ScalarType "int8",ScalarType "int8"], ScalarType "bool"),
    ("int8not", [ScalarType "int8"], ScalarType "int8"),
    ("int8or", [ScalarType "int8",ScalarType "int8"], ScalarType "int8"),
    ("int8out", [ScalarType "int8"], Pseudo Cstring),
    ("int8pl", [ScalarType "int8",ScalarType "int8"], ScalarType "int8"),
    ("int8pl_inet", [ScalarType "int8",ScalarType "inet"], ScalarType "inet"),
    ("int8send", [ScalarType "int8"], ScalarType "bytea"),
    ("int8shl", [ScalarType "int8",ScalarType "int4"], ScalarType "int8"),
    ("int8shr", [ScalarType "int8",ScalarType "int4"], ScalarType "int8"),
    ("int8smaller", [ScalarType "int8",ScalarType "int8"], ScalarType "int8"),
    ("int8um", [ScalarType "int8"], ScalarType "int8"),
    ("int8up", [ScalarType "int8"], ScalarType "int8"),
    ("int8xor", [ScalarType "int8",ScalarType "int8"], ScalarType "int8"),
    ("integer_pl_date", [ScalarType "int4",ScalarType "date"], ScalarType "date"),
    ("inter_lb", [ScalarType "line",ScalarType "box"], ScalarType "bool"),
    ("inter_sb", [ScalarType "lseg",ScalarType "box"], ScalarType "bool"),
    ("inter_sl", [ScalarType "lseg",ScalarType "line"], ScalarType "bool"),
    ("interval", [ScalarType "interval",ScalarType "int4"], ScalarType "interval"),
    ("interval", [ScalarType "reltime"], ScalarType "interval"),
    ("interval", [ScalarType "time"], ScalarType "interval"),
    ("interval_accum", [ArrayType (ScalarType "interval"),ScalarType "interval"], ArrayType (ScalarType "interval")),
    ("interval_avg", [ArrayType (ScalarType "interval")], ScalarType "interval"),
    ("interval_cmp", [ScalarType "interval",ScalarType "interval"], ScalarType "int4"),
    ("interval_div", [ScalarType "interval",ScalarType "float8"], ScalarType "interval"),
    ("interval_eq", [ScalarType "interval",ScalarType "interval"], ScalarType "bool"),
    ("interval_ge", [ScalarType "interval",ScalarType "interval"], ScalarType "bool"),
    ("interval_gt", [ScalarType "interval",ScalarType "interval"], ScalarType "bool"),
    ("interval_hash", [ScalarType "interval"], ScalarType "int4"),
    ("interval_in", [Pseudo Cstring,ScalarType "oid",ScalarType "int4"], ScalarType "interval"),
    ("interval_larger", [ScalarType "interval",ScalarType "interval"], ScalarType "interval"),
    ("interval_le", [ScalarType "interval",ScalarType "interval"], ScalarType "bool"),
    ("interval_lt", [ScalarType "interval",ScalarType "interval"], ScalarType "bool"),
    ("interval_mi", [ScalarType "interval",ScalarType "interval"], ScalarType "interval"),
    ("interval_mul", [ScalarType "interval",ScalarType "float8"], ScalarType "interval"),
    ("interval_ne", [ScalarType "interval",ScalarType "interval"], ScalarType "bool"),
    ("interval_out", [ScalarType "interval"], Pseudo Cstring),
    ("interval_pl", [ScalarType "interval",ScalarType "interval"], ScalarType "interval"),
    ("interval_pl_date", [ScalarType "interval",ScalarType "date"], ScalarType "timestamp"),
    ("interval_pl_time", [ScalarType "interval",ScalarType "time"], ScalarType "time"),
    ("interval_pl_timestamp", [ScalarType "interval",ScalarType "timestamp"], ScalarType "timestamp"),
    ("interval_pl_timestamptz", [ScalarType "interval",ScalarType "timestamptz"], ScalarType "timestamptz"),
    ("interval_pl_timetz", [ScalarType "interval",ScalarType "timetz"], ScalarType "timetz"),
    ("interval_send", [ScalarType "interval"], ScalarType "bytea"),
    ("interval_smaller", [ScalarType "interval",ScalarType "interval"], ScalarType "interval"),
    ("interval_um", [ScalarType "interval"], ScalarType "interval"),
    ("intervaltypmodin", [ArrayType (Pseudo Cstring)], ScalarType "int4"),
    ("intervaltypmodout", [ScalarType "int4"], Pseudo Cstring),
    ("intinterval", [ScalarType "abstime",ScalarType "tinterval"], ScalarType "bool"),
    ("isclosed", [ScalarType "path"], ScalarType "bool"),
    ("isfinite", [ScalarType "abstime"], ScalarType "bool"),
    ("isfinite", [ScalarType "date"], ScalarType "bool"),
    ("isfinite", [ScalarType "interval"], ScalarType "bool"),
    ("isfinite", [ScalarType "timestamp"], ScalarType "bool"),
    ("isfinite", [ScalarType "timestamptz"], ScalarType "bool"),
    ("ishorizontal", [ScalarType "line"], ScalarType "bool"),
    ("ishorizontal", [ScalarType "lseg"], ScalarType "bool"),
    ("ishorizontal", [ScalarType "point",ScalarType "point"], ScalarType "bool"),
    ("isopen", [ScalarType "path"], ScalarType "bool"),
    ("isparallel", [ScalarType "line",ScalarType "line"], ScalarType "bool"),
    ("isparallel", [ScalarType "lseg",ScalarType "lseg"], ScalarType "bool"),
    ("isperp", [ScalarType "line",ScalarType "line"], ScalarType "bool"),
    ("isperp", [ScalarType "lseg",ScalarType "lseg"], ScalarType "bool"),
    ("isvertical", [ScalarType "line"], ScalarType "bool"),
    ("isvertical", [ScalarType "lseg"], ScalarType "bool"),
    ("isvertical", [ScalarType "point",ScalarType "point"], ScalarType "bool"),
    ("justify_days", [ScalarType "interval"], ScalarType "interval"),
    ("justify_hours", [ScalarType "interval"], ScalarType "interval"),
    ("justify_interval", [ScalarType "interval"], ScalarType "interval"),
    ("lastval", [], ScalarType "int8"),
    ("length", [ScalarType "bit"], ScalarType "int4"),
    ("length", [ScalarType "bpchar"], ScalarType "int4"),
    ("length", [ScalarType "bytea"], ScalarType "int4"),
    ("length", [ScalarType "bytea",ScalarType "name"], ScalarType "int4"),
    ("length", [ScalarType "lseg"], ScalarType "float8"),
    ("length", [ScalarType "path"], ScalarType "float8"),
    ("length", [ScalarType "text"], ScalarType "int4"),
    ("length", [ScalarType "tsvector"], ScalarType "int4"),
    ("like", [ScalarType "bytea",ScalarType "bytea"], ScalarType "bool"),
    ("like", [ScalarType "name",ScalarType "text"], ScalarType "bool"),
    ("like", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("like_escape", [ScalarType "bytea",ScalarType "bytea"], ScalarType "bytea"),
    ("like_escape", [ScalarType "text",ScalarType "text"], ScalarType "text"),
    ("line", [ScalarType "point",ScalarType "point"], ScalarType "line"),
    ("line_distance", [ScalarType "line",ScalarType "line"], ScalarType "float8"),
    ("line_eq", [ScalarType "line",ScalarType "line"], ScalarType "bool"),
    ("line_horizontal", [ScalarType "line"], ScalarType "bool"),
    ("line_in", [Pseudo Cstring], ScalarType "line"),
    ("line_interpt", [ScalarType "line",ScalarType "line"], ScalarType "point"),
    ("line_intersect", [ScalarType "line",ScalarType "line"], ScalarType "bool"),
    ("line_out", [ScalarType "line"], Pseudo Cstring),
    ("line_parallel", [ScalarType "line",ScalarType "line"], ScalarType "bool"),
    ("line_perp", [ScalarType "line",ScalarType "line"], ScalarType "bool"),
    ("line_send", [ScalarType "line"], ScalarType "bytea"),
    ("line_vertical", [ScalarType "line"], ScalarType "bool"),
    ("ln", [ScalarType "float8"], ScalarType "float8"),
    ("ln", [ScalarType "numeric"], ScalarType "numeric"),
    ("lo_close", [ScalarType "int4"], ScalarType "int4"),
    ("lo_creat", [ScalarType "int4"], ScalarType "oid"),
    ("lo_create", [ScalarType "oid"], ScalarType "oid"),
    ("lo_export", [ScalarType "oid",ScalarType "text"], ScalarType "int4"),
    ("lo_import", [ScalarType "text"], ScalarType "oid"),
    ("lo_import", [ScalarType "text",ScalarType "oid"], ScalarType "oid"),
    ("lo_lseek", [ScalarType "int4",ScalarType "int4",ScalarType "int4"], ScalarType "int4"),
    ("lo_open", [ScalarType "oid",ScalarType "int4"], ScalarType "int4"),
    ("lo_tell", [ScalarType "int4"], ScalarType "int4"),
    ("lo_truncate", [ScalarType "int4",ScalarType "int4"], ScalarType "int4"),
    ("lo_unlink", [ScalarType "oid"], ScalarType "int4"),
    ("log", [ScalarType "float8"], ScalarType "float8"),
    ("log", [ScalarType "numeric"], ScalarType "numeric"),
    ("log", [ScalarType "numeric",ScalarType "numeric"], ScalarType "numeric"),
    ("loread", [ScalarType "int4",ScalarType "int4"], ScalarType "bytea"),
    ("lower", [ScalarType "text"], ScalarType "text"),
    ("lowrite", [ScalarType "int4",ScalarType "bytea"], ScalarType "int4"),
    ("lpad", [ScalarType "text",ScalarType "int4"], ScalarType "text"),
    ("lpad", [ScalarType "text",ScalarType "int4",ScalarType "text"], ScalarType "text"),
    ("lseg", [ScalarType "box"], ScalarType "lseg"),
    ("lseg", [ScalarType "point",ScalarType "point"], ScalarType "lseg"),
    ("lseg_center", [ScalarType "lseg"], ScalarType "point"),
    ("lseg_distance", [ScalarType "lseg",ScalarType "lseg"], ScalarType "float8"),
    ("lseg_eq", [ScalarType "lseg",ScalarType "lseg"], ScalarType "bool"),
    ("lseg_ge", [ScalarType "lseg",ScalarType "lseg"], ScalarType "bool"),
    ("lseg_gt", [ScalarType "lseg",ScalarType "lseg"], ScalarType "bool"),
    ("lseg_horizontal", [ScalarType "lseg"], ScalarType "bool"),
    ("lseg_in", [Pseudo Cstring], ScalarType "lseg"),
    ("lseg_interpt", [ScalarType "lseg",ScalarType "lseg"], ScalarType "point"),
    ("lseg_intersect", [ScalarType "lseg",ScalarType "lseg"], ScalarType "bool"),
    ("lseg_le", [ScalarType "lseg",ScalarType "lseg"], ScalarType "bool"),
    ("lseg_length", [ScalarType "lseg"], ScalarType "float8"),
    ("lseg_lt", [ScalarType "lseg",ScalarType "lseg"], ScalarType "bool"),
    ("lseg_ne", [ScalarType "lseg",ScalarType "lseg"], ScalarType "bool"),
    ("lseg_out", [ScalarType "lseg"], Pseudo Cstring),
    ("lseg_parallel", [ScalarType "lseg",ScalarType "lseg"], ScalarType "bool"),
    ("lseg_perp", [ScalarType "lseg",ScalarType "lseg"], ScalarType "bool"),
    ("lseg_send", [ScalarType "lseg"], ScalarType "bytea"),
    ("lseg_vertical", [ScalarType "lseg"], ScalarType "bool"),
    ("ltrim", [ScalarType "text"], ScalarType "text"),
    ("ltrim", [ScalarType "text",ScalarType "text"], ScalarType "text"),
    ("macaddr_cmp", [ScalarType "macaddr",ScalarType "macaddr"], ScalarType "int4"),
    ("macaddr_eq", [ScalarType "macaddr",ScalarType "macaddr"], ScalarType "bool"),
    ("macaddr_ge", [ScalarType "macaddr",ScalarType "macaddr"], ScalarType "bool"),
    ("macaddr_gt", [ScalarType "macaddr",ScalarType "macaddr"], ScalarType "bool"),
    ("macaddr_in", [Pseudo Cstring], ScalarType "macaddr"),
    ("macaddr_le", [ScalarType "macaddr",ScalarType "macaddr"], ScalarType "bool"),
    ("macaddr_lt", [ScalarType "macaddr",ScalarType "macaddr"], ScalarType "bool"),
    ("macaddr_ne", [ScalarType "macaddr",ScalarType "macaddr"], ScalarType "bool"),
    ("macaddr_out", [ScalarType "macaddr"], Pseudo Cstring),
    ("macaddr_send", [ScalarType "macaddr"], ScalarType "bytea"),
    ("makeaclitem", [ScalarType "oid",ScalarType "oid",ScalarType "text",ScalarType "bool"], ScalarType "aclitem"),
    ("masklen", [ScalarType "inet"], ScalarType "int4"),
    ("md5", [ScalarType "bytea"], ScalarType "text"),
    ("md5", [ScalarType "text"], ScalarType "text"),
    ("mktinterval", [ScalarType "abstime",ScalarType "abstime"], ScalarType "tinterval"),
    ("mod", [ScalarType "int2",ScalarType "int2"], ScalarType "int2"),
    ("mod", [ScalarType "int4",ScalarType "int4"], ScalarType "int4"),
    ("mod", [ScalarType "int8",ScalarType "int8"], ScalarType "int8"),
    ("mod", [ScalarType "numeric",ScalarType "numeric"], ScalarType "numeric"),
    ("mul_d_interval", [ScalarType "float8",ScalarType "interval"], ScalarType "interval"),
    ("name", [ScalarType "bpchar"], ScalarType "name"),
    ("name", [ScalarType "text"], ScalarType "name"),
    ("name", [ScalarType "varchar"], ScalarType "name"),
    ("nameeq", [ScalarType "name",ScalarType "name"], ScalarType "bool"),
    ("namege", [ScalarType "name",ScalarType "name"], ScalarType "bool"),
    ("namegt", [ScalarType "name",ScalarType "name"], ScalarType "bool"),
    ("nameiclike", [ScalarType "name",ScalarType "text"], ScalarType "bool"),
    ("nameicnlike", [ScalarType "name",ScalarType "text"], ScalarType "bool"),
    ("nameicregexeq", [ScalarType "name",ScalarType "text"], ScalarType "bool"),
    ("nameicregexne", [ScalarType "name",ScalarType "text"], ScalarType "bool"),
    ("namein", [Pseudo Cstring], ScalarType "name"),
    ("namele", [ScalarType "name",ScalarType "name"], ScalarType "bool"),
    ("namelike", [ScalarType "name",ScalarType "text"], ScalarType "bool"),
    ("namelt", [ScalarType "name",ScalarType "name"], ScalarType "bool"),
    ("namene", [ScalarType "name",ScalarType "name"], ScalarType "bool"),
    ("namenlike", [ScalarType "name",ScalarType "text"], ScalarType "bool"),
    ("nameout", [ScalarType "name"], Pseudo Cstring),
    ("nameregexeq", [ScalarType "name",ScalarType "text"], ScalarType "bool"),
    ("nameregexne", [ScalarType "name",ScalarType "text"], ScalarType "bool"),
    ("namesend", [ScalarType "name"], ScalarType "bytea"),
    ("netmask", [ScalarType "inet"], ScalarType "inet"),
    ("network", [ScalarType "inet"], ScalarType "cidr"),
    ("network_cmp", [ScalarType "inet",ScalarType "inet"], ScalarType "int4"),
    ("network_eq", [ScalarType "inet",ScalarType "inet"], ScalarType "bool"),
    ("network_ge", [ScalarType "inet",ScalarType "inet"], ScalarType "bool"),
    ("network_gt", [ScalarType "inet",ScalarType "inet"], ScalarType "bool"),
    ("network_le", [ScalarType "inet",ScalarType "inet"], ScalarType "bool"),
    ("network_lt", [ScalarType "inet",ScalarType "inet"], ScalarType "bool"),
    ("network_ne", [ScalarType "inet",ScalarType "inet"], ScalarType "bool"),
    ("network_sub", [ScalarType "inet",ScalarType "inet"], ScalarType "bool"),
    ("network_subeq", [ScalarType "inet",ScalarType "inet"], ScalarType "bool"),
    ("network_sup", [ScalarType "inet",ScalarType "inet"], ScalarType "bool"),
    ("network_supeq", [ScalarType "inet",ScalarType "inet"], ScalarType "bool"),
    ("nextval", [ScalarType "regclass"], ScalarType "int8"),
    ("notlike", [ScalarType "bytea",ScalarType "bytea"], ScalarType "bool"),
    ("notlike", [ScalarType "name",ScalarType "text"], ScalarType "bool"),
    ("notlike", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("now", [], ScalarType "timestamptz"),
    ("npoints", [ScalarType "path"], ScalarType "int4"),
    ("npoints", [ScalarType "polygon"], ScalarType "int4"),
    ("numeric", [ScalarType "float4"], ScalarType "numeric"),
    ("numeric", [ScalarType "float8"], ScalarType "numeric"),
    ("numeric", [ScalarType "int2"], ScalarType "numeric"),
    ("numeric", [ScalarType "int4"], ScalarType "numeric"),
    ("numeric", [ScalarType "int8"], ScalarType "numeric"),
    ("numeric", [ScalarType "numeric",ScalarType "int4"], ScalarType "numeric"),
    ("numeric_abs", [ScalarType "numeric"], ScalarType "numeric"),
    ("numeric_accum", [ArrayType (ScalarType "numeric"),ScalarType "numeric"], ArrayType (ScalarType "numeric")),
    ("numeric_add", [ScalarType "numeric",ScalarType "numeric"], ScalarType "numeric"),
    ("numeric_avg", [ArrayType (ScalarType "numeric")], ScalarType "numeric"),
    ("numeric_avg_accum", [ArrayType (ScalarType "numeric"),ScalarType "numeric"], ArrayType (ScalarType "numeric")),
    ("numeric_cmp", [ScalarType "numeric",ScalarType "numeric"], ScalarType "int4"),
    ("numeric_div", [ScalarType "numeric",ScalarType "numeric"], ScalarType "numeric"),
    ("numeric_div_trunc", [ScalarType "numeric",ScalarType "numeric"], ScalarType "numeric"),
    ("numeric_eq", [ScalarType "numeric",ScalarType "numeric"], ScalarType "bool"),
    ("numeric_exp", [ScalarType "numeric"], ScalarType "numeric"),
    ("numeric_fac", [ScalarType "int8"], ScalarType "numeric"),
    ("numeric_ge", [ScalarType "numeric",ScalarType "numeric"], ScalarType "bool"),
    ("numeric_gt", [ScalarType "numeric",ScalarType "numeric"], ScalarType "bool"),
    ("numeric_in", [Pseudo Cstring,ScalarType "oid",ScalarType "int4"], ScalarType "numeric"),
    ("numeric_inc", [ScalarType "numeric"], ScalarType "numeric"),
    ("numeric_larger", [ScalarType "numeric",ScalarType "numeric"], ScalarType "numeric"),
    ("numeric_le", [ScalarType "numeric",ScalarType "numeric"], ScalarType "bool"),
    ("numeric_ln", [ScalarType "numeric"], ScalarType "numeric"),
    ("numeric_log", [ScalarType "numeric",ScalarType "numeric"], ScalarType "numeric"),
    ("numeric_lt", [ScalarType "numeric",ScalarType "numeric"], ScalarType "bool"),
    ("numeric_mod", [ScalarType "numeric",ScalarType "numeric"], ScalarType "numeric"),
    ("numeric_mul", [ScalarType "numeric",ScalarType "numeric"], ScalarType "numeric"),
    ("numeric_ne", [ScalarType "numeric",ScalarType "numeric"], ScalarType "bool"),
    ("numeric_out", [ScalarType "numeric"], Pseudo Cstring),
    ("numeric_power", [ScalarType "numeric",ScalarType "numeric"], ScalarType "numeric"),
    ("numeric_send", [ScalarType "numeric"], ScalarType "bytea"),
    ("numeric_smaller", [ScalarType "numeric",ScalarType "numeric"], ScalarType "numeric"),
    ("numeric_sqrt", [ScalarType "numeric"], ScalarType "numeric"),
    ("numeric_stddev_pop", [ArrayType (ScalarType "numeric")], ScalarType "numeric"),
    ("numeric_stddev_samp", [ArrayType (ScalarType "numeric")], ScalarType "numeric"),
    ("numeric_sub", [ScalarType "numeric",ScalarType "numeric"], ScalarType "numeric"),
    ("numeric_uminus", [ScalarType "numeric"], ScalarType "numeric"),
    ("numeric_uplus", [ScalarType "numeric"], ScalarType "numeric"),
    ("numeric_var_pop", [ArrayType (ScalarType "numeric")], ScalarType "numeric"),
    ("numeric_var_samp", [ArrayType (ScalarType "numeric")], ScalarType "numeric"),
    ("numerictypmodin", [ArrayType (Pseudo Cstring)], ScalarType "int4"),
    ("numerictypmodout", [ScalarType "int4"], Pseudo Cstring),
    ("numnode", [ScalarType "tsquery"], ScalarType "int4"),
    ("obj_description", [ScalarType "oid"], ScalarType "text"),
    ("obj_description", [ScalarType "oid",ScalarType "name"], ScalarType "text"),
    ("octet_length", [ScalarType "bit"], ScalarType "int4"),
    ("octet_length", [ScalarType "bpchar"], ScalarType "int4"),
    ("octet_length", [ScalarType "bytea"], ScalarType "int4"),
    ("octet_length", [ScalarType "text"], ScalarType "int4"),
    ("oid", [ScalarType "int8"], ScalarType "oid"),
    ("oideq", [ScalarType "oid",ScalarType "oid"], ScalarType "bool"),
    ("oidge", [ScalarType "oid",ScalarType "oid"], ScalarType "bool"),
    ("oidgt", [ScalarType "oid",ScalarType "oid"], ScalarType "bool"),
    ("oidin", [Pseudo Cstring], ScalarType "oid"),
    ("oidlarger", [ScalarType "oid",ScalarType "oid"], ScalarType "oid"),
    ("oidle", [ScalarType "oid",ScalarType "oid"], ScalarType "bool"),
    ("oidlt", [ScalarType "oid",ScalarType "oid"], ScalarType "bool"),
    ("oidne", [ScalarType "oid",ScalarType "oid"], ScalarType "bool"),
    ("oidout", [ScalarType "oid"], Pseudo Cstring),
    ("oidsend", [ScalarType "oid"], ScalarType "bytea"),
    ("oidsmaller", [ScalarType "oid",ScalarType "oid"], ScalarType "oid"),
    ("oidvectoreq", [ScalarType "oidvector",ScalarType "oidvector"], ScalarType "bool"),
    ("oidvectorge", [ScalarType "oidvector",ScalarType "oidvector"], ScalarType "bool"),
    ("oidvectorgt", [ScalarType "oidvector",ScalarType "oidvector"], ScalarType "bool"),
    ("oidvectorin", [Pseudo Cstring], ScalarType "oidvector"),
    ("oidvectorle", [ScalarType "oidvector",ScalarType "oidvector"], ScalarType "bool"),
    ("oidvectorlt", [ScalarType "oidvector",ScalarType "oidvector"], ScalarType "bool"),
    ("oidvectorne", [ScalarType "oidvector",ScalarType "oidvector"], ScalarType "bool"),
    ("oidvectorout", [ScalarType "oidvector"], Pseudo Cstring),
    ("oidvectorsend", [ScalarType "oidvector"], ScalarType "bytea"),
    ("oidvectortypes", [ScalarType "oidvector"], ScalarType "text"),
    ("on_pb", [ScalarType "point",ScalarType "box"], ScalarType "bool"),
    ("on_pl", [ScalarType "point",ScalarType "line"], ScalarType "bool"),
    ("on_ppath", [ScalarType "point",ScalarType "path"], ScalarType "bool"),
    ("on_ps", [ScalarType "point",ScalarType "lseg"], ScalarType "bool"),
    ("on_sb", [ScalarType "lseg",ScalarType "box"], ScalarType "bool"),
    ("on_sl", [ScalarType "lseg",ScalarType "line"], ScalarType "bool"),
    ("overlaps", [ScalarType "time",ScalarType "interval",ScalarType "time",ScalarType "interval"], ScalarType "bool"),
    ("overlaps", [ScalarType "time",ScalarType "interval",ScalarType "time",ScalarType "time"], ScalarType "bool"),
    ("overlaps", [ScalarType "time",ScalarType "time",ScalarType "time",ScalarType "interval"], ScalarType "bool"),
    ("overlaps", [ScalarType "time",ScalarType "time",ScalarType "time",ScalarType "time"], ScalarType "bool"),
    ("overlaps", [ScalarType "timestamp",ScalarType "interval",ScalarType "timestamp",ScalarType "interval"], ScalarType "bool"),
    ("overlaps", [ScalarType "timestamp",ScalarType "interval",ScalarType "timestamp",ScalarType "timestamp"], ScalarType "bool"),
    ("overlaps", [ScalarType "timestamp",ScalarType "timestamp",ScalarType "timestamp",ScalarType "interval"], ScalarType "bool"),
    ("overlaps", [ScalarType "timestamp",ScalarType "timestamp",ScalarType "timestamp",ScalarType "timestamp"], ScalarType "bool"),
    ("overlaps", [ScalarType "timestamptz",ScalarType "interval",ScalarType "timestamptz",ScalarType "interval"], ScalarType "bool"),
    ("overlaps", [ScalarType "timestamptz",ScalarType "interval",ScalarType "timestamptz",ScalarType "timestamptz"], ScalarType "bool"),
    ("overlaps", [ScalarType "timestamptz",ScalarType "timestamptz",ScalarType "timestamptz",ScalarType "interval"], ScalarType "bool"),
    ("overlaps", [ScalarType "timestamptz",ScalarType "timestamptz",ScalarType "timestamptz",ScalarType "timestamptz"], ScalarType "bool"),
    ("overlaps", [ScalarType "timetz",ScalarType "timetz",ScalarType "timetz",ScalarType "timetz"], ScalarType "bool"),
    ("overlay", [ScalarType "text",ScalarType "text",ScalarType "int4"], ScalarType "text"),
    ("overlay", [ScalarType "text",ScalarType "text",ScalarType "int4",ScalarType "int4"], ScalarType "text"),
    ("path", [ScalarType "polygon"], ScalarType "path"),
    ("path_add", [ScalarType "path",ScalarType "path"], ScalarType "path"),
    ("path_add_pt", [ScalarType "path",ScalarType "point"], ScalarType "path"),
    ("path_center", [ScalarType "path"], ScalarType "point"),
    ("path_contain_pt", [ScalarType "path",ScalarType "point"], ScalarType "bool"),
    ("path_distance", [ScalarType "path",ScalarType "path"], ScalarType "float8"),
    ("path_div_pt", [ScalarType "path",ScalarType "point"], ScalarType "path"),
    ("path_in", [Pseudo Cstring], ScalarType "path"),
    ("path_inter", [ScalarType "path",ScalarType "path"], ScalarType "bool"),
    ("path_length", [ScalarType "path"], ScalarType "float8"),
    ("path_mul_pt", [ScalarType "path",ScalarType "point"], ScalarType "path"),
    ("path_n_eq", [ScalarType "path",ScalarType "path"], ScalarType "bool"),
    ("path_n_ge", [ScalarType "path",ScalarType "path"], ScalarType "bool"),
    ("path_n_gt", [ScalarType "path",ScalarType "path"], ScalarType "bool"),
    ("path_n_le", [ScalarType "path",ScalarType "path"], ScalarType "bool"),
    ("path_n_lt", [ScalarType "path",ScalarType "path"], ScalarType "bool"),
    ("path_npoints", [ScalarType "path"], ScalarType "int4"),
    ("path_out", [ScalarType "path"], Pseudo Cstring),
    ("path_send", [ScalarType "path"], ScalarType "bytea"),
    ("path_sub_pt", [ScalarType "path",ScalarType "point"], ScalarType "path"),
    ("pclose", [ScalarType "path"], ScalarType "path"),
    ("pg_advisory_lock", [ScalarType "int4",ScalarType "int4"], Pseudo Void),
    ("pg_advisory_lock", [ScalarType "int8"], Pseudo Void),
    ("pg_advisory_lock_shared", [ScalarType "int4",ScalarType "int4"], Pseudo Void),
    ("pg_advisory_lock_shared", [ScalarType "int8"], Pseudo Void),
    ("pg_advisory_unlock", [ScalarType "int4",ScalarType "int4"], ScalarType "bool"),
    ("pg_advisory_unlock", [ScalarType "int8"], ScalarType "bool"),
    ("pg_advisory_unlock_all", [], Pseudo Void),
    ("pg_advisory_unlock_shared", [ScalarType "int4",ScalarType "int4"], ScalarType "bool"),
    ("pg_advisory_unlock_shared", [ScalarType "int8"], ScalarType "bool"),
    ("pg_backend_pid", [], ScalarType "int4"),
    ("pg_cancel_backend", [ScalarType "int4"], ScalarType "bool"),
    ("pg_char_to_encoding", [ScalarType "name"], ScalarType "int4"),
    ("pg_client_encoding", [], ScalarType "name"),
    ("pg_column_size", [Pseudo Any], ScalarType "int4"),
    ("pg_conf_load_time", [], ScalarType "timestamptz"),
    ("pg_conversion_is_visible", [ScalarType "oid"], ScalarType "bool"),
    ("pg_current_xlog_insert_location", [], ScalarType "text"),
    ("pg_current_xlog_location", [], ScalarType "text"),
    ("pg_cursor", [], SetOfType (Pseudo Record)),
    ("pg_database_size", [ScalarType "name"], ScalarType "int8"),
    ("pg_database_size", [ScalarType "oid"], ScalarType "int8"),
    ("pg_encoding_to_char", [ScalarType "int4"], ScalarType "name"),
    ("pg_function_is_visible", [ScalarType "oid"], ScalarType "bool"),
    ("pg_get_constraintdef", [ScalarType "oid"], ScalarType "text"),
    ("pg_get_constraintdef", [ScalarType "oid",ScalarType "bool"], ScalarType "text"),
    ("pg_get_expr", [ScalarType "text",ScalarType "oid"], ScalarType "text"),
    ("pg_get_expr", [ScalarType "text",ScalarType "oid",ScalarType "bool"], ScalarType "text"),
    ("pg_get_function_arguments", [ScalarType "oid"], ScalarType "text"),
    ("pg_get_function_identity_arguments", [ScalarType "oid"], ScalarType "text"),
    ("pg_get_function_result", [ScalarType "oid"], ScalarType "text"),
    ("pg_get_functiondef", [ScalarType "oid"], ScalarType "text"),
    ("pg_get_indexdef", [ScalarType "oid"], ScalarType "text"),
    ("pg_get_indexdef", [ScalarType "oid",ScalarType "int4",ScalarType "bool"], ScalarType "text"),
    ("pg_get_keywords", [], SetOfType (Pseudo Record)),
    ("pg_get_ruledef", [ScalarType "oid"], ScalarType "text"),
    ("pg_get_ruledef", [ScalarType "oid",ScalarType "bool"], ScalarType "text"),
    ("pg_get_serial_sequence", [ScalarType "text",ScalarType "text"], ScalarType "text"),
    ("pg_get_triggerdef", [ScalarType "oid"], ScalarType "text"),
    ("pg_get_userbyid", [ScalarType "oid"], ScalarType "name"),
    ("pg_get_viewdef", [ScalarType "oid"], ScalarType "text"),
    ("pg_get_viewdef", [ScalarType "oid",ScalarType "bool"], ScalarType "text"),
    ("pg_get_viewdef", [ScalarType "text"], ScalarType "text"),
    ("pg_get_viewdef", [ScalarType "text",ScalarType "bool"], ScalarType "text"),
    ("pg_has_role", [ScalarType "name",ScalarType "name",ScalarType "text"], ScalarType "bool"),
    ("pg_has_role", [ScalarType "name",ScalarType "oid",ScalarType "text"], ScalarType "bool"),
    ("pg_has_role", [ScalarType "name",ScalarType "text"], ScalarType "bool"),
    ("pg_has_role", [ScalarType "oid",ScalarType "name",ScalarType "text"], ScalarType "bool"),
    ("pg_has_role", [ScalarType "oid",ScalarType "oid",ScalarType "text"], ScalarType "bool"),
    ("pg_has_role", [ScalarType "oid",ScalarType "text"], ScalarType "bool"),
    ("pg_is_other_temp_schema", [ScalarType "oid"], ScalarType "bool"),
    ("pg_lock_status", [], SetOfType (Pseudo Record)),
    ("pg_ls_dir", [ScalarType "text"], SetOfType (ScalarType "text")),
    ("pg_my_temp_schema", [], ScalarType "oid"),
    ("pg_opclass_is_visible", [ScalarType "oid"], ScalarType "bool"),
    ("pg_operator_is_visible", [ScalarType "oid"], ScalarType "bool"),
    ("pg_options_to_table", [ArrayType (ScalarType "text")], SetOfType (Pseudo Record)),
    ("pg_postmaster_start_time", [], ScalarType "timestamptz"),
    ("pg_prepared_statement", [], SetOfType (Pseudo Record)),
    ("pg_prepared_xact", [], SetOfType (Pseudo Record)),
    ("pg_read_file", [ScalarType "text",ScalarType "int8",ScalarType "int8"], ScalarType "text"),
    ("pg_relation_size", [ScalarType "regclass"], ScalarType "int8"),
    ("pg_relation_size", [ScalarType "regclass",ScalarType "text"], ScalarType "int8"),
    ("pg_reload_conf", [], ScalarType "bool"),
    ("pg_rotate_logfile", [], ScalarType "bool"),
    ("pg_show_all_settings", [], SetOfType (Pseudo Record)),
    ("pg_size_pretty", [ScalarType "int8"], ScalarType "text"),
    ("pg_sleep", [ScalarType "float8"], Pseudo Void),
    ("pg_start_backup", [ScalarType "text",ScalarType "bool"], ScalarType "text"),
    ("pg_stat_clear_snapshot", [], Pseudo Void),
    ("pg_stat_file", [ScalarType "text"], Pseudo Record),
    ("pg_stat_get_activity", [ScalarType "int4"], SetOfType (Pseudo Record)),
    ("pg_stat_get_backend_activity", [ScalarType "int4"], ScalarType "text"),
    ("pg_stat_get_backend_activity_start", [ScalarType "int4"], ScalarType "timestamptz"),
    ("pg_stat_get_backend_client_addr", [ScalarType "int4"], ScalarType "inet"),
    ("pg_stat_get_backend_client_port", [ScalarType "int4"], ScalarType "int4"),
    ("pg_stat_get_backend_dbid", [ScalarType "int4"], ScalarType "oid"),
    ("pg_stat_get_backend_idset", [], SetOfType (ScalarType "int4")),
    ("pg_stat_get_backend_pid", [ScalarType "int4"], ScalarType "int4"),
    ("pg_stat_get_backend_start", [ScalarType "int4"], ScalarType "timestamptz"),
    ("pg_stat_get_backend_userid", [ScalarType "int4"], ScalarType "oid"),
    ("pg_stat_get_backend_waiting", [ScalarType "int4"], ScalarType "bool"),
    ("pg_stat_get_backend_xact_start", [ScalarType "int4"], ScalarType "timestamptz"),
    ("pg_stat_get_bgwriter_buf_written_checkpoints", [], ScalarType "int8"),
    ("pg_stat_get_bgwriter_buf_written_clean", [], ScalarType "int8"),
    ("pg_stat_get_bgwriter_maxwritten_clean", [], ScalarType "int8"),
    ("pg_stat_get_bgwriter_requested_checkpoints", [], ScalarType "int8"),
    ("pg_stat_get_bgwriter_timed_checkpoints", [], ScalarType "int8"),
    ("pg_stat_get_blocks_fetched", [ScalarType "oid"], ScalarType "int8"),
    ("pg_stat_get_blocks_hit", [ScalarType "oid"], ScalarType "int8"),
    ("pg_stat_get_buf_alloc", [], ScalarType "int8"),
    ("pg_stat_get_buf_written_backend", [], ScalarType "int8"),
    ("pg_stat_get_db_blocks_fetched", [ScalarType "oid"], ScalarType "int8"),
    ("pg_stat_get_db_blocks_hit", [ScalarType "oid"], ScalarType "int8"),
    ("pg_stat_get_db_numbackends", [ScalarType "oid"], ScalarType "int4"),
    ("pg_stat_get_db_tuples_deleted", [ScalarType "oid"], ScalarType "int8"),
    ("pg_stat_get_db_tuples_fetched", [ScalarType "oid"], ScalarType "int8"),
    ("pg_stat_get_db_tuples_inserted", [ScalarType "oid"], ScalarType "int8"),
    ("pg_stat_get_db_tuples_returned", [ScalarType "oid"], ScalarType "int8"),
    ("pg_stat_get_db_tuples_updated", [ScalarType "oid"], ScalarType "int8"),
    ("pg_stat_get_db_xact_commit", [ScalarType "oid"], ScalarType "int8"),
    ("pg_stat_get_db_xact_rollback", [ScalarType "oid"], ScalarType "int8"),
    ("pg_stat_get_dead_tuples", [ScalarType "oid"], ScalarType "int8"),
    ("pg_stat_get_function_calls", [ScalarType "oid"], ScalarType "int8"),
    ("pg_stat_get_function_self_time", [ScalarType "oid"], ScalarType "int8"),
    ("pg_stat_get_function_time", [ScalarType "oid"], ScalarType "int8"),
    ("pg_stat_get_last_analyze_time", [ScalarType "oid"], ScalarType "timestamptz"),
    ("pg_stat_get_last_autoanalyze_time", [ScalarType "oid"], ScalarType "timestamptz"),
    ("pg_stat_get_last_autovacuum_time", [ScalarType "oid"], ScalarType "timestamptz"),
    ("pg_stat_get_last_vacuum_time", [ScalarType "oid"], ScalarType "timestamptz"),
    ("pg_stat_get_live_tuples", [ScalarType "oid"], ScalarType "int8"),
    ("pg_stat_get_numscans", [ScalarType "oid"], ScalarType "int8"),
    ("pg_stat_get_tuples_deleted", [ScalarType "oid"], ScalarType "int8"),
    ("pg_stat_get_tuples_fetched", [ScalarType "oid"], ScalarType "int8"),
    ("pg_stat_get_tuples_hot_updated", [ScalarType "oid"], ScalarType "int8"),
    ("pg_stat_get_tuples_inserted", [ScalarType "oid"], ScalarType "int8"),
    ("pg_stat_get_tuples_returned", [ScalarType "oid"], ScalarType "int8"),
    ("pg_stat_get_tuples_updated", [ScalarType "oid"], ScalarType "int8"),
    ("pg_stat_reset", [], Pseudo Void),
    ("pg_stop_backup", [], ScalarType "text"),
    ("pg_switch_xlog", [], ScalarType "text"),
    ("pg_table_is_visible", [ScalarType "oid"], ScalarType "bool"),
    ("pg_tablespace_databases", [ScalarType "oid"], SetOfType (ScalarType "oid")),
    ("pg_tablespace_size", [ScalarType "name"], ScalarType "int8"),
    ("pg_tablespace_size", [ScalarType "oid"], ScalarType "int8"),
    ("pg_terminate_backend", [ScalarType "int4"], ScalarType "bool"),
    ("pg_timezone_abbrevs", [], SetOfType (Pseudo Record)),
    ("pg_timezone_names", [], SetOfType (Pseudo Record)),
    ("pg_total_relation_size", [ScalarType "regclass"], ScalarType "int8"),
    ("pg_try_advisory_lock", [ScalarType "int4",ScalarType "int4"], ScalarType "bool"),
    ("pg_try_advisory_lock", [ScalarType "int8"], ScalarType "bool"),
    ("pg_try_advisory_lock_shared", [ScalarType "int4",ScalarType "int4"], ScalarType "bool"),
    ("pg_try_advisory_lock_shared", [ScalarType "int8"], ScalarType "bool"),
    ("pg_ts_config_is_visible", [ScalarType "oid"], ScalarType "bool"),
    ("pg_ts_dict_is_visible", [ScalarType "oid"], ScalarType "bool"),
    ("pg_ts_parser_is_visible", [ScalarType "oid"], ScalarType "bool"),
    ("pg_ts_template_is_visible", [ScalarType "oid"], ScalarType "bool"),
    ("pg_type_is_visible", [ScalarType "oid"], ScalarType "bool"),
    ("pg_typeof", [Pseudo Any], ScalarType "regtype"),
    ("pg_xlogfile_name", [ScalarType "text"], ScalarType "text"),
    ("pg_xlogfile_name_offset", [ScalarType "text"], Pseudo Record),
    ("pi", [], ScalarType "float8"),
    ("plainto_tsquery", [ScalarType "regconfig",ScalarType "text"], ScalarType "tsquery"),
    ("plainto_tsquery", [ScalarType "text"], ScalarType "tsquery"),
    ("point", [ScalarType "box"], ScalarType "point"),
    ("point", [ScalarType "circle"], ScalarType "point"),
    ("point", [ScalarType "float8",ScalarType "float8"], ScalarType "point"),
    ("point", [ScalarType "lseg"], ScalarType "point"),
    ("point", [ScalarType "path"], ScalarType "point"),
    ("point", [ScalarType "polygon"], ScalarType "point"),
    ("point_above", [ScalarType "point",ScalarType "point"], ScalarType "bool"),
    ("point_add", [ScalarType "point",ScalarType "point"], ScalarType "point"),
    ("point_below", [ScalarType "point",ScalarType "point"], ScalarType "bool"),
    ("point_distance", [ScalarType "point",ScalarType "point"], ScalarType "float8"),
    ("point_div", [ScalarType "point",ScalarType "point"], ScalarType "point"),
    ("point_eq", [ScalarType "point",ScalarType "point"], ScalarType "bool"),
    ("point_horiz", [ScalarType "point",ScalarType "point"], ScalarType "bool"),
    ("point_in", [Pseudo Cstring], ScalarType "point"),
    ("point_left", [ScalarType "point",ScalarType "point"], ScalarType "bool"),
    ("point_mul", [ScalarType "point",ScalarType "point"], ScalarType "point"),
    ("point_ne", [ScalarType "point",ScalarType "point"], ScalarType "bool"),
    ("point_out", [ScalarType "point"], Pseudo Cstring),
    ("point_right", [ScalarType "point",ScalarType "point"], ScalarType "bool"),
    ("point_send", [ScalarType "point"], ScalarType "bytea"),
    ("point_sub", [ScalarType "point",ScalarType "point"], ScalarType "point"),
    ("point_vert", [ScalarType "point",ScalarType "point"], ScalarType "bool"),
    ("poly_above", [ScalarType "polygon",ScalarType "polygon"], ScalarType "bool"),
    ("poly_below", [ScalarType "polygon",ScalarType "polygon"], ScalarType "bool"),
    ("poly_center", [ScalarType "polygon"], ScalarType "point"),
    ("poly_contain", [ScalarType "polygon",ScalarType "polygon"], ScalarType "bool"),
    ("poly_contain_pt", [ScalarType "polygon",ScalarType "point"], ScalarType "bool"),
    ("poly_contained", [ScalarType "polygon",ScalarType "polygon"], ScalarType "bool"),
    ("poly_distance", [ScalarType "polygon",ScalarType "polygon"], ScalarType "float8"),
    ("poly_in", [Pseudo Cstring], ScalarType "polygon"),
    ("poly_left", [ScalarType "polygon",ScalarType "polygon"], ScalarType "bool"),
    ("poly_npoints", [ScalarType "polygon"], ScalarType "int4"),
    ("poly_out", [ScalarType "polygon"], Pseudo Cstring),
    ("poly_overabove", [ScalarType "polygon",ScalarType "polygon"], ScalarType "bool"),
    ("poly_overbelow", [ScalarType "polygon",ScalarType "polygon"], ScalarType "bool"),
    ("poly_overlap", [ScalarType "polygon",ScalarType "polygon"], ScalarType "bool"),
    ("poly_overleft", [ScalarType "polygon",ScalarType "polygon"], ScalarType "bool"),
    ("poly_overright", [ScalarType "polygon",ScalarType "polygon"], ScalarType "bool"),
    ("poly_right", [ScalarType "polygon",ScalarType "polygon"], ScalarType "bool"),
    ("poly_same", [ScalarType "polygon",ScalarType "polygon"], ScalarType "bool"),
    ("poly_send", [ScalarType "polygon"], ScalarType "bytea"),
    ("polygon", [ScalarType "box"], ScalarType "polygon"),
    ("polygon", [ScalarType "circle"], ScalarType "polygon"),
    ("polygon", [ScalarType "int4",ScalarType "circle"], ScalarType "polygon"),
    ("polygon", [ScalarType "path"], ScalarType "polygon"),
    ("popen", [ScalarType "path"], ScalarType "path"),
    ("position", [ScalarType "bit",ScalarType "bit"], ScalarType "int4"),
    ("position", [ScalarType "bytea",ScalarType "bytea"], ScalarType "int4"),
    ("position", [ScalarType "text",ScalarType "text"], ScalarType "int4"),
    ("postgresql_fdw_validator", [ArrayType (ScalarType "text"),ScalarType "oid"], ScalarType "bool"),
    ("pow", [ScalarType "float8",ScalarType "float8"], ScalarType "float8"),
    ("pow", [ScalarType "numeric",ScalarType "numeric"], ScalarType "numeric"),
    ("power", [ScalarType "float8",ScalarType "float8"], ScalarType "float8"),
    ("power", [ScalarType "numeric",ScalarType "numeric"], ScalarType "numeric"),
    ("pt_contained_circle", [ScalarType "point",ScalarType "circle"], ScalarType "bool"),
    ("pt_contained_poly", [ScalarType "point",ScalarType "polygon"], ScalarType "bool"),
    ("query_to_xml", [ScalarType "text",ScalarType "bool",ScalarType "bool",ScalarType "text"], ScalarType "xml"),
    ("query_to_xml_and_xmlschema", [ScalarType "text",ScalarType "bool",ScalarType "bool",ScalarType "text"], ScalarType "xml"),
    ("query_to_xmlschema", [ScalarType "text",ScalarType "bool",ScalarType "bool",ScalarType "text"], ScalarType "xml"),
    ("querytree", [ScalarType "tsquery"], ScalarType "text"),
    ("quote_ident", [ScalarType "text"], ScalarType "text"),
    ("quote_literal", [Pseudo AnyElement], ScalarType "text"),
    ("quote_literal", [ScalarType "text"], ScalarType "text"),
    ("quote_nullable", [Pseudo AnyElement], ScalarType "text"),
    ("quote_nullable", [ScalarType "text"], ScalarType "text"),
    ("radians", [ScalarType "float8"], ScalarType "float8"),
    ("radius", [ScalarType "circle"], ScalarType "float8"),
    ("random", [], ScalarType "float8"),
    ("record_eq", [Pseudo Record,Pseudo Record], ScalarType "bool"),
    ("record_ge", [Pseudo Record,Pseudo Record], ScalarType "bool"),
    ("record_gt", [Pseudo Record,Pseudo Record], ScalarType "bool"),
    ("record_in", [Pseudo Cstring,ScalarType "oid",ScalarType "int4"], Pseudo Record),
    ("record_le", [Pseudo Record,Pseudo Record], ScalarType "bool"),
    ("record_lt", [Pseudo Record,Pseudo Record], ScalarType "bool"),
    ("record_ne", [Pseudo Record,Pseudo Record], ScalarType "bool"),
    ("record_out", [Pseudo Record], Pseudo Cstring),
    ("record_send", [Pseudo Record], ScalarType "bytea"),
    ("regclass", [ScalarType "text"], ScalarType "regclass"),
    ("regclassin", [Pseudo Cstring], ScalarType "regclass"),
    ("regclassout", [ScalarType "regclass"], Pseudo Cstring),
    ("regclasssend", [ScalarType "regclass"], ScalarType "bytea"),
    ("regconfigin", [Pseudo Cstring], ScalarType "regconfig"),
    ("regconfigout", [ScalarType "regconfig"], Pseudo Cstring),
    ("regconfigsend", [ScalarType "regconfig"], ScalarType "bytea"),
    ("regdictionaryin", [Pseudo Cstring], ScalarType "regdictionary"),
    ("regdictionaryout", [ScalarType "regdictionary"], Pseudo Cstring),
    ("regdictionarysend", [ScalarType "regdictionary"], ScalarType "bytea"),
    ("regexp_matches", [ScalarType "text",ScalarType "text"], SetOfType (ArrayType (ScalarType "text"))),
    ("regexp_matches", [ScalarType "text",ScalarType "text",ScalarType "text"], SetOfType (ArrayType (ScalarType "text"))),
    ("regexp_replace", [ScalarType "text",ScalarType "text",ScalarType "text"], ScalarType "text"),
    ("regexp_replace", [ScalarType "text",ScalarType "text",ScalarType "text",ScalarType "text"], ScalarType "text"),
    ("regexp_split_to_array", [ScalarType "text",ScalarType "text"], ArrayType (ScalarType "text")),
    ("regexp_split_to_array", [ScalarType "text",ScalarType "text",ScalarType "text"], ArrayType (ScalarType "text")),
    ("regexp_split_to_table", [ScalarType "text",ScalarType "text"], SetOfType (ScalarType "text")),
    ("regexp_split_to_table", [ScalarType "text",ScalarType "text",ScalarType "text"], SetOfType (ScalarType "text")),
    ("regoperatorin", [Pseudo Cstring], ScalarType "regoperator"),
    ("regoperatorout", [ScalarType "regoperator"], Pseudo Cstring),
    ("regoperatorsend", [ScalarType "regoperator"], ScalarType "bytea"),
    ("regoperin", [Pseudo Cstring], ScalarType "regoper"),
    ("regoperout", [ScalarType "regoper"], Pseudo Cstring),
    ("regopersend", [ScalarType "regoper"], ScalarType "bytea"),
    ("regprocedurein", [Pseudo Cstring], ScalarType "regprocedure"),
    ("regprocedureout", [ScalarType "regprocedure"], Pseudo Cstring),
    ("regproceduresend", [ScalarType "regprocedure"], ScalarType "bytea"),
    ("regprocin", [Pseudo Cstring], ScalarType "regproc"),
    ("regprocout", [ScalarType "regproc"], Pseudo Cstring),
    ("regprocsend", [ScalarType "regproc"], ScalarType "bytea"),
    ("regtypein", [Pseudo Cstring], ScalarType "regtype"),
    ("regtypeout", [ScalarType "regtype"], Pseudo Cstring),
    ("regtypesend", [ScalarType "regtype"], ScalarType "bytea"),
    ("reltime", [ScalarType "interval"], ScalarType "reltime"),
    ("reltimeeq", [ScalarType "reltime",ScalarType "reltime"], ScalarType "bool"),
    ("reltimege", [ScalarType "reltime",ScalarType "reltime"], ScalarType "bool"),
    ("reltimegt", [ScalarType "reltime",ScalarType "reltime"], ScalarType "bool"),
    ("reltimein", [Pseudo Cstring], ScalarType "reltime"),
    ("reltimele", [ScalarType "reltime",ScalarType "reltime"], ScalarType "bool"),
    ("reltimelt", [ScalarType "reltime",ScalarType "reltime"], ScalarType "bool"),
    ("reltimene", [ScalarType "reltime",ScalarType "reltime"], ScalarType "bool"),
    ("reltimeout", [ScalarType "reltime"], Pseudo Cstring),
    ("reltimesend", [ScalarType "reltime"], ScalarType "bytea"),
    ("repeat", [ScalarType "text",ScalarType "int4"], ScalarType "text"),
    ("replace", [ScalarType "text",ScalarType "text",ScalarType "text"], ScalarType "text"),
    ("round", [ScalarType "float8"], ScalarType "float8"),
    ("round", [ScalarType "numeric"], ScalarType "numeric"),
    ("round", [ScalarType "numeric",ScalarType "int4"], ScalarType "numeric"),
    ("rpad", [ScalarType "text",ScalarType "int4"], ScalarType "text"),
    ("rpad", [ScalarType "text",ScalarType "int4",ScalarType "text"], ScalarType "text"),
    ("rtrim", [ScalarType "text"], ScalarType "text"),
    ("rtrim", [ScalarType "text",ScalarType "text"], ScalarType "text"),
    ("schema_to_xml", [ScalarType "name",ScalarType "bool",ScalarType "bool",ScalarType "text"], ScalarType "xml"),
    ("schema_to_xml_and_xmlschema", [ScalarType "name",ScalarType "bool",ScalarType "bool",ScalarType "text"], ScalarType "xml"),
    ("schema_to_xmlschema", [ScalarType "name",ScalarType "bool",ScalarType "bool",ScalarType "text"], ScalarType "xml"),
    ("session_user", [], ScalarType "name"),
    ("set_bit", [ScalarType "bytea",ScalarType "int4",ScalarType "int4"], ScalarType "bytea"),
    ("set_byte", [ScalarType "bytea",ScalarType "int4",ScalarType "int4"], ScalarType "bytea"),
    ("set_config", [ScalarType "text",ScalarType "text",ScalarType "bool"], ScalarType "text"),
    ("set_masklen", [ScalarType "cidr",ScalarType "int4"], ScalarType "cidr"),
    ("set_masklen", [ScalarType "inet",ScalarType "int4"], ScalarType "inet"),
    ("setseed", [ScalarType "float8"], Pseudo Void),
    ("setval", [ScalarType "regclass",ScalarType "int8"], ScalarType "int8"),
    ("setval", [ScalarType "regclass",ScalarType "int8",ScalarType "bool"], ScalarType "int8"),
    ("setweight", [ScalarType "tsvector",ScalarType "char"], ScalarType "tsvector"),
    ("shobj_description", [ScalarType "oid",ScalarType "name"], ScalarType "text"),
    ("sign", [ScalarType "float8"], ScalarType "float8"),
    ("sign", [ScalarType "numeric"], ScalarType "numeric"),
    ("similar_escape", [ScalarType "text",ScalarType "text"], ScalarType "text"),
    ("sin", [ScalarType "float8"], ScalarType "float8"),
    ("slope", [ScalarType "point",ScalarType "point"], ScalarType "float8"),
    ("smgreq", [ScalarType "smgr",ScalarType "smgr"], ScalarType "bool"),
    ("smgrin", [Pseudo Cstring], ScalarType "smgr"),
    ("smgrne", [ScalarType "smgr",ScalarType "smgr"], ScalarType "bool"),
    ("smgrout", [ScalarType "smgr"], Pseudo Cstring),
    ("split_part", [ScalarType "text",ScalarType "text",ScalarType "int4"], ScalarType "text"),
    ("sqrt", [ScalarType "float8"], ScalarType "float8"),
    ("sqrt", [ScalarType "numeric"], ScalarType "numeric"),
    ("statement_timestamp", [], ScalarType "timestamptz"),
    ("string_to_array", [ScalarType "text",ScalarType "text"], ArrayType (ScalarType "text")),
    ("strip", [ScalarType "tsvector"], ScalarType "tsvector"),
    ("strpos", [ScalarType "text",ScalarType "text"], ScalarType "int4"),
    ("substr", [ScalarType "bytea",ScalarType "int4"], ScalarType "bytea"),
    ("substr", [ScalarType "bytea",ScalarType "int4",ScalarType "int4"], ScalarType "bytea"),
    ("substr", [ScalarType "text",ScalarType "int4"], ScalarType "text"),
    ("substr", [ScalarType "text",ScalarType "int4",ScalarType "int4"], ScalarType "text"),
    ("substring", [ScalarType "bit",ScalarType "int4"], ScalarType "bit"),
    ("substring", [ScalarType "bit",ScalarType "int4",ScalarType "int4"], ScalarType "bit"),
    ("substring", [ScalarType "bytea",ScalarType "int4"], ScalarType "bytea"),
    ("substring", [ScalarType "bytea",ScalarType "int4",ScalarType "int4"], ScalarType "bytea"),
    ("substring", [ScalarType "text",ScalarType "int4"], ScalarType "text"),
    ("substring", [ScalarType "text",ScalarType "int4",ScalarType "int4"], ScalarType "text"),
    ("substring", [ScalarType "text",ScalarType "text"], ScalarType "text"),
    ("substring", [ScalarType "text",ScalarType "text",ScalarType "text"], ScalarType "text"),
    ("suppress_redundant_updates_trigger", [], Pseudo Trigger),
    ("table_to_xml", [ScalarType "regclass",ScalarType "bool",ScalarType "bool",ScalarType "text"], ScalarType "xml"),
    ("table_to_xml_and_xmlschema", [ScalarType "regclass",ScalarType "bool",ScalarType "bool",ScalarType "text"], ScalarType "xml"),
    ("table_to_xmlschema", [ScalarType "regclass",ScalarType "bool",ScalarType "bool",ScalarType "text"], ScalarType "xml"),
    ("tan", [ScalarType "float8"], ScalarType "float8"),
    ("text", [ScalarType "bool"], ScalarType "text"),
    ("text", [ScalarType "bpchar"], ScalarType "text"),
    ("text", [ScalarType "char"], ScalarType "text"),
    ("text", [ScalarType "inet"], ScalarType "text"),
    ("text", [ScalarType "name"], ScalarType "text"),
    ("text", [ScalarType "xml"], ScalarType "text"),
    ("text_ge", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("text_gt", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("text_larger", [ScalarType "text",ScalarType "text"], ScalarType "text"),
    ("text_le", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("text_lt", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("text_pattern_ge", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("text_pattern_gt", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("text_pattern_le", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("text_pattern_lt", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("text_smaller", [ScalarType "text",ScalarType "text"], ScalarType "text"),
    ("textanycat", [ScalarType "text",Pseudo AnyNonArray], ScalarType "text"),
    ("textcat", [ScalarType "text",ScalarType "text"], ScalarType "text"),
    ("texteq", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("texticlike", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("texticnlike", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("texticregexeq", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("texticregexne", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("textin", [Pseudo Cstring], ScalarType "text"),
    ("textlen", [ScalarType "text"], ScalarType "int4"),
    ("textlike", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("textne", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("textnlike", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("textout", [ScalarType "text"], Pseudo Cstring),
    ("textregexeq", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("textregexne", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("textsend", [ScalarType "text"], ScalarType "bytea"),
    ("tideq", [ScalarType "tid",ScalarType "tid"], ScalarType "bool"),
    ("tidge", [ScalarType "tid",ScalarType "tid"], ScalarType "bool"),
    ("tidgt", [ScalarType "tid",ScalarType "tid"], ScalarType "bool"),
    ("tidin", [Pseudo Cstring], ScalarType "tid"),
    ("tidlarger", [ScalarType "tid",ScalarType "tid"], ScalarType "tid"),
    ("tidle", [ScalarType "tid",ScalarType "tid"], ScalarType "bool"),
    ("tidlt", [ScalarType "tid",ScalarType "tid"], ScalarType "bool"),
    ("tidne", [ScalarType "tid",ScalarType "tid"], ScalarType "bool"),
    ("tidout", [ScalarType "tid"], Pseudo Cstring),
    ("tidsend", [ScalarType "tid"], ScalarType "bytea"),
    ("tidsmaller", [ScalarType "tid",ScalarType "tid"], ScalarType "tid"),
    ("time", [ScalarType "abstime"], ScalarType "time"),
    ("time", [ScalarType "interval"], ScalarType "time"),
    ("time", [ScalarType "time",ScalarType "int4"], ScalarType "time"),
    ("time", [ScalarType "timestamp"], ScalarType "time"),
    ("time", [ScalarType "timestamptz"], ScalarType "time"),
    ("time", [ScalarType "timetz"], ScalarType "time"),
    ("time_cmp", [ScalarType "time",ScalarType "time"], ScalarType "int4"),
    ("time_eq", [ScalarType "time",ScalarType "time"], ScalarType "bool"),
    ("time_ge", [ScalarType "time",ScalarType "time"], ScalarType "bool"),
    ("time_gt", [ScalarType "time",ScalarType "time"], ScalarType "bool"),
    ("time_hash", [ScalarType "time"], ScalarType "int4"),
    ("time_in", [Pseudo Cstring,ScalarType "oid",ScalarType "int4"], ScalarType "time"),
    ("time_larger", [ScalarType "time",ScalarType "time"], ScalarType "time"),
    ("time_le", [ScalarType "time",ScalarType "time"], ScalarType "bool"),
    ("time_lt", [ScalarType "time",ScalarType "time"], ScalarType "bool"),
    ("time_mi_interval", [ScalarType "time",ScalarType "interval"], ScalarType "time"),
    ("time_mi_time", [ScalarType "time",ScalarType "time"], ScalarType "interval"),
    ("time_ne", [ScalarType "time",ScalarType "time"], ScalarType "bool"),
    ("time_out", [ScalarType "time"], Pseudo Cstring),
    ("time_pl_interval", [ScalarType "time",ScalarType "interval"], ScalarType "time"),
    ("time_send", [ScalarType "time"], ScalarType "bytea"),
    ("time_smaller", [ScalarType "time",ScalarType "time"], ScalarType "time"),
    ("timedate_pl", [ScalarType "time",ScalarType "date"], ScalarType "timestamp"),
    ("timemi", [ScalarType "abstime",ScalarType "reltime"], ScalarType "abstime"),
    ("timenow", [], ScalarType "abstime"),
    ("timeofday", [], ScalarType "text"),
    ("timepl", [ScalarType "abstime",ScalarType "reltime"], ScalarType "abstime"),
    ("timestamp", [ScalarType "abstime"], ScalarType "timestamp"),
    ("timestamp", [ScalarType "date"], ScalarType "timestamp"),
    ("timestamp", [ScalarType "date",ScalarType "time"], ScalarType "timestamp"),
    ("timestamp", [ScalarType "timestamp",ScalarType "int4"], ScalarType "timestamp"),
    ("timestamp", [ScalarType "timestamptz"], ScalarType "timestamp"),
    ("timestamp_cmp", [ScalarType "timestamp",ScalarType "timestamp"], ScalarType "int4"),
    ("timestamp_cmp_date", [ScalarType "timestamp",ScalarType "date"], ScalarType "int4"),
    ("timestamp_cmp_timestamptz", [ScalarType "timestamp",ScalarType "timestamptz"], ScalarType "int4"),
    ("timestamp_eq", [ScalarType "timestamp",ScalarType "timestamp"], ScalarType "bool"),
    ("timestamp_eq_date", [ScalarType "timestamp",ScalarType "date"], ScalarType "bool"),
    ("timestamp_eq_timestamptz", [ScalarType "timestamp",ScalarType "timestamptz"], ScalarType "bool"),
    ("timestamp_ge", [ScalarType "timestamp",ScalarType "timestamp"], ScalarType "bool"),
    ("timestamp_ge_date", [ScalarType "timestamp",ScalarType "date"], ScalarType "bool"),
    ("timestamp_ge_timestamptz", [ScalarType "timestamp",ScalarType "timestamptz"], ScalarType "bool"),
    ("timestamp_gt", [ScalarType "timestamp",ScalarType "timestamp"], ScalarType "bool"),
    ("timestamp_gt_date", [ScalarType "timestamp",ScalarType "date"], ScalarType "bool"),
    ("timestamp_gt_timestamptz", [ScalarType "timestamp",ScalarType "timestamptz"], ScalarType "bool"),
    ("timestamp_hash", [ScalarType "timestamp"], ScalarType "int4"),
    ("timestamp_in", [Pseudo Cstring,ScalarType "oid",ScalarType "int4"], ScalarType "timestamp"),
    ("timestamp_larger", [ScalarType "timestamp",ScalarType "timestamp"], ScalarType "timestamp"),
    ("timestamp_le", [ScalarType "timestamp",ScalarType "timestamp"], ScalarType "bool"),
    ("timestamp_le_date", [ScalarType "timestamp",ScalarType "date"], ScalarType "bool"),
    ("timestamp_le_timestamptz", [ScalarType "timestamp",ScalarType "timestamptz"], ScalarType "bool"),
    ("timestamp_lt", [ScalarType "timestamp",ScalarType "timestamp"], ScalarType "bool"),
    ("timestamp_lt_date", [ScalarType "timestamp",ScalarType "date"], ScalarType "bool"),
    ("timestamp_lt_timestamptz", [ScalarType "timestamp",ScalarType "timestamptz"], ScalarType "bool"),
    ("timestamp_mi", [ScalarType "timestamp",ScalarType "timestamp"], ScalarType "interval"),
    ("timestamp_mi_interval", [ScalarType "timestamp",ScalarType "interval"], ScalarType "timestamp"),
    ("timestamp_ne", [ScalarType "timestamp",ScalarType "timestamp"], ScalarType "bool"),
    ("timestamp_ne_date", [ScalarType "timestamp",ScalarType "date"], ScalarType "bool"),
    ("timestamp_ne_timestamptz", [ScalarType "timestamp",ScalarType "timestamptz"], ScalarType "bool"),
    ("timestamp_out", [ScalarType "timestamp"], Pseudo Cstring),
    ("timestamp_pl_interval", [ScalarType "timestamp",ScalarType "interval"], ScalarType "timestamp"),
    ("timestamp_send", [ScalarType "timestamp"], ScalarType "bytea"),
    ("timestamp_smaller", [ScalarType "timestamp",ScalarType "timestamp"], ScalarType "timestamp"),
    ("timestamptypmodin", [ArrayType (Pseudo Cstring)], ScalarType "int4"),
    ("timestamptypmodout", [ScalarType "int4"], Pseudo Cstring),
    ("timestamptz", [ScalarType "abstime"], ScalarType "timestamptz"),
    ("timestamptz", [ScalarType "date"], ScalarType "timestamptz"),
    ("timestamptz", [ScalarType "date",ScalarType "time"], ScalarType "timestamptz"),
    ("timestamptz", [ScalarType "date",ScalarType "timetz"], ScalarType "timestamptz"),
    ("timestamptz", [ScalarType "timestamp"], ScalarType "timestamptz"),
    ("timestamptz", [ScalarType "timestamptz",ScalarType "int4"], ScalarType "timestamptz"),
    ("timestamptz_cmp", [ScalarType "timestamptz",ScalarType "timestamptz"], ScalarType "int4"),
    ("timestamptz_cmp_date", [ScalarType "timestamptz",ScalarType "date"], ScalarType "int4"),
    ("timestamptz_cmp_timestamp", [ScalarType "timestamptz",ScalarType "timestamp"], ScalarType "int4"),
    ("timestamptz_eq", [ScalarType "timestamptz",ScalarType "timestamptz"], ScalarType "bool"),
    ("timestamptz_eq_date", [ScalarType "timestamptz",ScalarType "date"], ScalarType "bool"),
    ("timestamptz_eq_timestamp", [ScalarType "timestamptz",ScalarType "timestamp"], ScalarType "bool"),
    ("timestamptz_ge", [ScalarType "timestamptz",ScalarType "timestamptz"], ScalarType "bool"),
    ("timestamptz_ge_date", [ScalarType "timestamptz",ScalarType "date"], ScalarType "bool"),
    ("timestamptz_ge_timestamp", [ScalarType "timestamptz",ScalarType "timestamp"], ScalarType "bool"),
    ("timestamptz_gt", [ScalarType "timestamptz",ScalarType "timestamptz"], ScalarType "bool"),
    ("timestamptz_gt_date", [ScalarType "timestamptz",ScalarType "date"], ScalarType "bool"),
    ("timestamptz_gt_timestamp", [ScalarType "timestamptz",ScalarType "timestamp"], ScalarType "bool"),
    ("timestamptz_in", [Pseudo Cstring,ScalarType "oid",ScalarType "int4"], ScalarType "timestamptz"),
    ("timestamptz_larger", [ScalarType "timestamptz",ScalarType "timestamptz"], ScalarType "timestamptz"),
    ("timestamptz_le", [ScalarType "timestamptz",ScalarType "timestamptz"], ScalarType "bool"),
    ("timestamptz_le_date", [ScalarType "timestamptz",ScalarType "date"], ScalarType "bool"),
    ("timestamptz_le_timestamp", [ScalarType "timestamptz",ScalarType "timestamp"], ScalarType "bool"),
    ("timestamptz_lt", [ScalarType "timestamptz",ScalarType "timestamptz"], ScalarType "bool"),
    ("timestamptz_lt_date", [ScalarType "timestamptz",ScalarType "date"], ScalarType "bool"),
    ("timestamptz_lt_timestamp", [ScalarType "timestamptz",ScalarType "timestamp"], ScalarType "bool"),
    ("timestamptz_mi", [ScalarType "timestamptz",ScalarType "timestamptz"], ScalarType "interval"),
    ("timestamptz_mi_interval", [ScalarType "timestamptz",ScalarType "interval"], ScalarType "timestamptz"),
    ("timestamptz_ne", [ScalarType "timestamptz",ScalarType "timestamptz"], ScalarType "bool"),
    ("timestamptz_ne_date", [ScalarType "timestamptz",ScalarType "date"], ScalarType "bool"),
    ("timestamptz_ne_timestamp", [ScalarType "timestamptz",ScalarType "timestamp"], ScalarType "bool"),
    ("timestamptz_out", [ScalarType "timestamptz"], Pseudo Cstring),
    ("timestamptz_pl_interval", [ScalarType "timestamptz",ScalarType "interval"], ScalarType "timestamptz"),
    ("timestamptz_send", [ScalarType "timestamptz"], ScalarType "bytea"),
    ("timestamptz_smaller", [ScalarType "timestamptz",ScalarType "timestamptz"], ScalarType "timestamptz"),
    ("timestamptztypmodin", [ArrayType (Pseudo Cstring)], ScalarType "int4"),
    ("timestamptztypmodout", [ScalarType "int4"], Pseudo Cstring),
    ("timetypmodin", [ArrayType (Pseudo Cstring)], ScalarType "int4"),
    ("timetypmodout", [ScalarType "int4"], Pseudo Cstring),
    ("timetz", [ScalarType "time"], ScalarType "timetz"),
    ("timetz", [ScalarType "timestamptz"], ScalarType "timetz"),
    ("timetz", [ScalarType "timetz",ScalarType "int4"], ScalarType "timetz"),
    ("timetz_cmp", [ScalarType "timetz",ScalarType "timetz"], ScalarType "int4"),
    ("timetz_eq", [ScalarType "timetz",ScalarType "timetz"], ScalarType "bool"),
    ("timetz_ge", [ScalarType "timetz",ScalarType "timetz"], ScalarType "bool"),
    ("timetz_gt", [ScalarType "timetz",ScalarType "timetz"], ScalarType "bool"),
    ("timetz_hash", [ScalarType "timetz"], ScalarType "int4"),
    ("timetz_in", [Pseudo Cstring,ScalarType "oid",ScalarType "int4"], ScalarType "timetz"),
    ("timetz_larger", [ScalarType "timetz",ScalarType "timetz"], ScalarType "timetz"),
    ("timetz_le", [ScalarType "timetz",ScalarType "timetz"], ScalarType "bool"),
    ("timetz_lt", [ScalarType "timetz",ScalarType "timetz"], ScalarType "bool"),
    ("timetz_mi_interval", [ScalarType "timetz",ScalarType "interval"], ScalarType "timetz"),
    ("timetz_ne", [ScalarType "timetz",ScalarType "timetz"], ScalarType "bool"),
    ("timetz_out", [ScalarType "timetz"], Pseudo Cstring),
    ("timetz_pl_interval", [ScalarType "timetz",ScalarType "interval"], ScalarType "timetz"),
    ("timetz_send", [ScalarType "timetz"], ScalarType "bytea"),
    ("timetz_smaller", [ScalarType "timetz",ScalarType "timetz"], ScalarType "timetz"),
    ("timetzdate_pl", [ScalarType "timetz",ScalarType "date"], ScalarType "timestamptz"),
    ("timetztypmodin", [ArrayType (Pseudo Cstring)], ScalarType "int4"),
    ("timetztypmodout", [ScalarType "int4"], Pseudo Cstring),
    ("timezone", [ScalarType "interval",ScalarType "timestamp"], ScalarType "timestamptz"),
    ("timezone", [ScalarType "interval",ScalarType "timestamptz"], ScalarType "timestamp"),
    ("timezone", [ScalarType "interval",ScalarType "timetz"], ScalarType "timetz"),
    ("timezone", [ScalarType "text",ScalarType "timestamp"], ScalarType "timestamptz"),
    ("timezone", [ScalarType "text",ScalarType "timestamptz"], ScalarType "timestamp"),
    ("timezone", [ScalarType "text",ScalarType "timetz"], ScalarType "timetz"),
    ("tinterval", [ScalarType "abstime",ScalarType "abstime"], ScalarType "tinterval"),
    ("tintervalct", [ScalarType "tinterval",ScalarType "tinterval"], ScalarType "bool"),
    ("tintervalend", [ScalarType "tinterval"], ScalarType "abstime"),
    ("tintervaleq", [ScalarType "tinterval",ScalarType "tinterval"], ScalarType "bool"),
    ("tintervalge", [ScalarType "tinterval",ScalarType "tinterval"], ScalarType "bool"),
    ("tintervalgt", [ScalarType "tinterval",ScalarType "tinterval"], ScalarType "bool"),
    ("tintervalin", [Pseudo Cstring], ScalarType "tinterval"),
    ("tintervalle", [ScalarType "tinterval",ScalarType "tinterval"], ScalarType "bool"),
    ("tintervalleneq", [ScalarType "tinterval",ScalarType "reltime"], ScalarType "bool"),
    ("tintervallenge", [ScalarType "tinterval",ScalarType "reltime"], ScalarType "bool"),
    ("tintervallengt", [ScalarType "tinterval",ScalarType "reltime"], ScalarType "bool"),
    ("tintervallenle", [ScalarType "tinterval",ScalarType "reltime"], ScalarType "bool"),
    ("tintervallenlt", [ScalarType "tinterval",ScalarType "reltime"], ScalarType "bool"),
    ("tintervallenne", [ScalarType "tinterval",ScalarType "reltime"], ScalarType "bool"),
    ("tintervallt", [ScalarType "tinterval",ScalarType "tinterval"], ScalarType "bool"),
    ("tintervalne", [ScalarType "tinterval",ScalarType "tinterval"], ScalarType "bool"),
    ("tintervalout", [ScalarType "tinterval"], Pseudo Cstring),
    ("tintervalov", [ScalarType "tinterval",ScalarType "tinterval"], ScalarType "bool"),
    ("tintervalrel", [ScalarType "tinterval"], ScalarType "reltime"),
    ("tintervalsame", [ScalarType "tinterval",ScalarType "tinterval"], ScalarType "bool"),
    ("tintervalsend", [ScalarType "tinterval"], ScalarType "bytea"),
    ("tintervalstart", [ScalarType "tinterval"], ScalarType "abstime"),
    ("to_ascii", [ScalarType "text"], ScalarType "text"),
    ("to_ascii", [ScalarType "text",ScalarType "int4"], ScalarType "text"),
    ("to_ascii", [ScalarType "text",ScalarType "name"], ScalarType "text"),
    ("to_char", [ScalarType "float4",ScalarType "text"], ScalarType "text"),
    ("to_char", [ScalarType "float8",ScalarType "text"], ScalarType "text"),
    ("to_char", [ScalarType "int4",ScalarType "text"], ScalarType "text"),
    ("to_char", [ScalarType "int8",ScalarType "text"], ScalarType "text"),
    ("to_char", [ScalarType "interval",ScalarType "text"], ScalarType "text"),
    ("to_char", [ScalarType "numeric",ScalarType "text"], ScalarType "text"),
    ("to_char", [ScalarType "timestamp",ScalarType "text"], ScalarType "text"),
    ("to_char", [ScalarType "timestamptz",ScalarType "text"], ScalarType "text"),
    ("to_date", [ScalarType "text",ScalarType "text"], ScalarType "date"),
    ("to_hex", [ScalarType "int4"], ScalarType "text"),
    ("to_hex", [ScalarType "int8"], ScalarType "text"),
    ("to_number", [ScalarType "text",ScalarType "text"], ScalarType "numeric"),
    ("to_timestamp", [ScalarType "float8"], ScalarType "timestamptz"),
    ("to_timestamp", [ScalarType "text",ScalarType "text"], ScalarType "timestamptz"),
    ("to_tsquery", [ScalarType "regconfig",ScalarType "text"], ScalarType "tsquery"),
    ("to_tsquery", [ScalarType "text"], ScalarType "tsquery"),
    ("to_tsvector", [ScalarType "regconfig",ScalarType "text"], ScalarType "tsvector"),
    ("to_tsvector", [ScalarType "text"], ScalarType "tsvector"),
    ("transaction_timestamp", [], ScalarType "timestamptz"),
    ("translate", [ScalarType "text",ScalarType "text",ScalarType "text"], ScalarType "text"),
    ("trigger_in", [Pseudo Cstring], Pseudo Trigger),
    ("trigger_out", [Pseudo Trigger], Pseudo Cstring),
    ("trunc", [ScalarType "float8"], ScalarType "float8"),
    ("trunc", [ScalarType "macaddr"], ScalarType "macaddr"),
    ("trunc", [ScalarType "numeric"], ScalarType "numeric"),
    ("trunc", [ScalarType "numeric",ScalarType "int4"], ScalarType "numeric"),
    ("ts_debug", [ScalarType "regconfig",ScalarType "text"], SetOfType (Pseudo Record)),
    ("ts_debug", [ScalarType "text"], SetOfType (Pseudo Record)),
    ("ts_headline", [ScalarType "regconfig",ScalarType "text",ScalarType "tsquery"], ScalarType "text"),
    ("ts_headline", [ScalarType "regconfig",ScalarType "text",ScalarType "tsquery",ScalarType "text"], ScalarType "text"),
    ("ts_headline", [ScalarType "text",ScalarType "tsquery"], ScalarType "text"),
    ("ts_headline", [ScalarType "text",ScalarType "tsquery",ScalarType "text"], ScalarType "text"),
    ("ts_lexize", [ScalarType "regdictionary",ScalarType "text"], ArrayType (ScalarType "text")),
    ("ts_match_qv", [ScalarType "tsquery",ScalarType "tsvector"], ScalarType "bool"),
    ("ts_match_tq", [ScalarType "text",ScalarType "tsquery"], ScalarType "bool"),
    ("ts_match_tt", [ScalarType "text",ScalarType "text"], ScalarType "bool"),
    ("ts_match_vq", [ScalarType "tsvector",ScalarType "tsquery"], ScalarType "bool"),
    ("ts_parse", [ScalarType "oid",ScalarType "text"], SetOfType (Pseudo Record)),
    ("ts_parse", [ScalarType "text",ScalarType "text"], SetOfType (Pseudo Record)),
    ("ts_rank", [ArrayType (ScalarType "float4"),ScalarType "tsvector",ScalarType "tsquery"], ScalarType "float4"),
    ("ts_rank", [ArrayType (ScalarType "float4"),ScalarType "tsvector",ScalarType "tsquery",ScalarType "int4"], ScalarType "float4"),
    ("ts_rank", [ScalarType "tsvector",ScalarType "tsquery"], ScalarType "float4"),
    ("ts_rank", [ScalarType "tsvector",ScalarType "tsquery",ScalarType "int4"], ScalarType "float4"),
    ("ts_rank_cd", [ArrayType (ScalarType "float4"),ScalarType "tsvector",ScalarType "tsquery"], ScalarType "float4"),
    ("ts_rank_cd", [ArrayType (ScalarType "float4"),ScalarType "tsvector",ScalarType "tsquery",ScalarType "int4"], ScalarType "float4"),
    ("ts_rank_cd", [ScalarType "tsvector",ScalarType "tsquery"], ScalarType "float4"),
    ("ts_rank_cd", [ScalarType "tsvector",ScalarType "tsquery",ScalarType "int4"], ScalarType "float4"),
    ("ts_rewrite", [ScalarType "tsquery",ScalarType "text"], ScalarType "tsquery"),
    ("ts_rewrite", [ScalarType "tsquery",ScalarType "tsquery",ScalarType "tsquery"], ScalarType "tsquery"),
    ("ts_stat", [ScalarType "text"], SetOfType (Pseudo Record)),
    ("ts_stat", [ScalarType "text",ScalarType "text"], SetOfType (Pseudo Record)),
    ("ts_token_type", [ScalarType "oid"], SetOfType (Pseudo Record)),
    ("ts_token_type", [ScalarType "text"], SetOfType (Pseudo Record)),
    ("tsq_mcontained", [ScalarType "tsquery",ScalarType "tsquery"], ScalarType "bool"),
    ("tsq_mcontains", [ScalarType "tsquery",ScalarType "tsquery"], ScalarType "bool"),
    ("tsquery_and", [ScalarType "tsquery",ScalarType "tsquery"], ScalarType "tsquery"),
    ("tsquery_cmp", [ScalarType "tsquery",ScalarType "tsquery"], ScalarType "int4"),
    ("tsquery_eq", [ScalarType "tsquery",ScalarType "tsquery"], ScalarType "bool"),
    ("tsquery_ge", [ScalarType "tsquery",ScalarType "tsquery"], ScalarType "bool"),
    ("tsquery_gt", [ScalarType "tsquery",ScalarType "tsquery"], ScalarType "bool"),
    ("tsquery_le", [ScalarType "tsquery",ScalarType "tsquery"], ScalarType "bool"),
    ("tsquery_lt", [ScalarType "tsquery",ScalarType "tsquery"], ScalarType "bool"),
    ("tsquery_ne", [ScalarType "tsquery",ScalarType "tsquery"], ScalarType "bool"),
    ("tsquery_not", [ScalarType "tsquery"], ScalarType "tsquery"),
    ("tsquery_or", [ScalarType "tsquery",ScalarType "tsquery"], ScalarType "tsquery"),
    ("tsqueryin", [Pseudo Cstring], ScalarType "tsquery"),
    ("tsqueryout", [ScalarType "tsquery"], Pseudo Cstring),
    ("tsquerysend", [ScalarType "tsquery"], ScalarType "bytea"),
    ("tsvector_cmp", [ScalarType "tsvector",ScalarType "tsvector"], ScalarType "int4"),
    ("tsvector_concat", [ScalarType "tsvector",ScalarType "tsvector"], ScalarType "tsvector"),
    ("tsvector_eq", [ScalarType "tsvector",ScalarType "tsvector"], ScalarType "bool"),
    ("tsvector_ge", [ScalarType "tsvector",ScalarType "tsvector"], ScalarType "bool"),
    ("tsvector_gt", [ScalarType "tsvector",ScalarType "tsvector"], ScalarType "bool"),
    ("tsvector_le", [ScalarType "tsvector",ScalarType "tsvector"], ScalarType "bool"),
    ("tsvector_lt", [ScalarType "tsvector",ScalarType "tsvector"], ScalarType "bool"),
    ("tsvector_ne", [ScalarType "tsvector",ScalarType "tsvector"], ScalarType "bool"),
    ("tsvector_update_trigger", [], Pseudo Trigger),
    ("tsvector_update_trigger_column", [], Pseudo Trigger),
    ("tsvectorin", [Pseudo Cstring], ScalarType "tsvector"),
    ("tsvectorout", [ScalarType "tsvector"], Pseudo Cstring),
    ("tsvectorsend", [ScalarType "tsvector"], ScalarType "bytea"),
    ("txid_current", [], ScalarType "int8"),
    ("txid_current_snapshot", [], ScalarType "txid_snapshot"),
    ("txid_snapshot_in", [Pseudo Cstring], ScalarType "txid_snapshot"),
    ("txid_snapshot_out", [ScalarType "txid_snapshot"], Pseudo Cstring),
    ("txid_snapshot_send", [ScalarType "txid_snapshot"], ScalarType "bytea"),
    ("txid_snapshot_xip", [ScalarType "txid_snapshot"], SetOfType (ScalarType "int8")),
    ("txid_snapshot_xmax", [ScalarType "txid_snapshot"], ScalarType "int8"),
    ("txid_snapshot_xmin", [ScalarType "txid_snapshot"], ScalarType "int8"),
    ("txid_visible_in_snapshot", [ScalarType "int8",ScalarType "txid_snapshot"], ScalarType "bool"),
    ("unknownin", [Pseudo Cstring], ScalarType "unknown"),
    ("unknownout", [ScalarType "unknown"], Pseudo Cstring),
    ("unknownsend", [ScalarType "unknown"], ScalarType "bytea"),
    ("unnest", [Pseudo AnyArray], SetOfType (Pseudo AnyElement)),
    ("upper", [ScalarType "text"], ScalarType "text"),
    ("uuid_cmp", [ScalarType "uuid",ScalarType "uuid"], ScalarType "int4"),
    ("uuid_eq", [ScalarType "uuid",ScalarType "uuid"], ScalarType "bool"),
    ("uuid_ge", [ScalarType "uuid",ScalarType "uuid"], ScalarType "bool"),
    ("uuid_gt", [ScalarType "uuid",ScalarType "uuid"], ScalarType "bool"),
    ("uuid_hash", [ScalarType "uuid"], ScalarType "int4"),
    ("uuid_in", [Pseudo Cstring], ScalarType "uuid"),
    ("uuid_le", [ScalarType "uuid",ScalarType "uuid"], ScalarType "bool"),
    ("uuid_lt", [ScalarType "uuid",ScalarType "uuid"], ScalarType "bool"),
    ("uuid_ne", [ScalarType "uuid",ScalarType "uuid"], ScalarType "bool"),
    ("uuid_out", [ScalarType "uuid"], Pseudo Cstring),
    ("uuid_send", [ScalarType "uuid"], ScalarType "bytea"),
    ("varbit", [ScalarType "varbit",ScalarType "int4",ScalarType "bool"], ScalarType "varbit"),
    ("varbit_in", [Pseudo Cstring,ScalarType "oid",ScalarType "int4"], ScalarType "varbit"),
    ("varbit_out", [ScalarType "varbit"], Pseudo Cstring),
    ("varbit_send", [ScalarType "varbit"], ScalarType "bytea"),
    ("varbitcmp", [ScalarType "varbit",ScalarType "varbit"], ScalarType "int4"),
    ("varbiteq", [ScalarType "varbit",ScalarType "varbit"], ScalarType "bool"),
    ("varbitge", [ScalarType "varbit",ScalarType "varbit"], ScalarType "bool"),
    ("varbitgt", [ScalarType "varbit",ScalarType "varbit"], ScalarType "bool"),
    ("varbitle", [ScalarType "varbit",ScalarType "varbit"], ScalarType "bool"),
    ("varbitlt", [ScalarType "varbit",ScalarType "varbit"], ScalarType "bool"),
    ("varbitne", [ScalarType "varbit",ScalarType "varbit"], ScalarType "bool"),
    ("varbittypmodin", [ArrayType (Pseudo Cstring)], ScalarType "int4"),
    ("varbittypmodout", [ScalarType "int4"], Pseudo Cstring),
    ("varchar", [ScalarType "name"], ScalarType "varchar"),
    ("varchar", [ScalarType "varchar",ScalarType "int4",ScalarType "bool"], ScalarType "varchar"),
    ("varcharin", [Pseudo Cstring,ScalarType "oid",ScalarType "int4"], ScalarType "varchar"),
    ("varcharout", [ScalarType "varchar"], Pseudo Cstring),
    ("varcharsend", [ScalarType "varchar"], ScalarType "bytea"),
    ("varchartypmodin", [ArrayType (Pseudo Cstring)], ScalarType "int4"),
    ("varchartypmodout", [ScalarType "int4"], Pseudo Cstring),
    ("version", [], ScalarType "text"),
    ("void_in", [Pseudo Cstring], Pseudo Void),
    ("void_out", [Pseudo Void], Pseudo Cstring),
    ("width", [ScalarType "box"], ScalarType "float8"),
    ("width_bucket", [ScalarType "float8",ScalarType "float8",ScalarType "float8",ScalarType "int4"], ScalarType "int4"),
    ("width_bucket", [ScalarType "numeric",ScalarType "numeric",ScalarType "numeric",ScalarType "int4"], ScalarType "int4"),
    ("xideq", [ScalarType "xid",ScalarType "xid"], ScalarType "bool"),
    ("xideqint4", [ScalarType "xid",ScalarType "int4"], ScalarType "bool"),
    ("xidin", [Pseudo Cstring], ScalarType "xid"),
    ("xidout", [ScalarType "xid"], Pseudo Cstring),
    ("xidsend", [ScalarType "xid"], ScalarType "bytea"),
    ("xml", [ScalarType "text"], ScalarType "xml"),
    ("xml_in", [Pseudo Cstring], ScalarType "xml"),
    ("xml_out", [ScalarType "xml"], Pseudo Cstring),
    ("xml_send", [ScalarType "xml"], ScalarType "bytea"),
    ("xmlcomment", [ScalarType "text"], ScalarType "xml"),
    ("xmlconcat2", [ScalarType "xml",ScalarType "xml"], ScalarType "xml"),
    ("xmlvalidate", [ScalarType "xml",ScalarType "text"], ScalarType "bool"),
    ("xpath", [ScalarType "text",ScalarType "xml"], ArrayType (ScalarType "xml")),
    ("xpath", [ScalarType "text",ScalarType "xml",ArrayType (ScalarType "text")], ArrayType (ScalarType "xml"))
    ]
-- ArgCheck ----------------------------------------------------
data ArgCheck  = ArgCheck (TypePred) (TypePredError) 
-- cata
sem_ArgCheck :: ArgCheck  ->
                T_ArgCheck 
sem_ArgCheck (ArgCheck _typePred _typePredError )  =
    (sem_ArgCheck_ArgCheck _typePred _typePredError )
-- semantic domain
type T_ArgCheck  = ( )
data Inh_ArgCheck  = Inh_ArgCheck {}
data Syn_ArgCheck  = Syn_ArgCheck {}
wrap_ArgCheck :: T_ArgCheck  ->
                 Inh_ArgCheck  ->
                 Syn_ArgCheck 
wrap_ArgCheck sem (Inh_ArgCheck )  =
    (let ( ) =
             (sem )
     in  (Syn_ArgCheck ))
sem_ArgCheck_ArgCheck :: TypePred ->
                         TypePredError ->
                         T_ArgCheck 
sem_ArgCheck_ArgCheck typePred_ typePredError_  =
    (let 
     in  ( ))
-- ArgsCheck ---------------------------------------------------
data ArgsCheck  = AllSameType (Type) 
                | AllSameType1 (Type) 
                | AllSameType1Any 
                | AllSameTypeAny 
                | AllSameTypeNum (Type) (Int) 
                | AllSameTypeNumAny (Int) 
                | AllSameTypePredNum (ArgCheck) (Int) 
                | ExactList (TypeList) 
                | ExactPredList (ArgCheckList) 
-- cata
sem_ArgsCheck :: ArgsCheck  ->
                 T_ArgsCheck 
sem_ArgsCheck (AllSameType _type )  =
    (sem_ArgsCheck_AllSameType (sem_Type _type ) )
sem_ArgsCheck (AllSameType1 _type )  =
    (sem_ArgsCheck_AllSameType1 (sem_Type _type ) )
sem_ArgsCheck (AllSameType1Any )  =
    (sem_ArgsCheck_AllSameType1Any )
sem_ArgsCheck (AllSameTypeAny )  =
    (sem_ArgsCheck_AllSameTypeAny )
sem_ArgsCheck (AllSameTypeNum _type _int )  =
    (sem_ArgsCheck_AllSameTypeNum (sem_Type _type ) _int )
sem_ArgsCheck (AllSameTypeNumAny _int )  =
    (sem_ArgsCheck_AllSameTypeNumAny _int )
sem_ArgsCheck (AllSameTypePredNum _argCheck _int )  =
    (sem_ArgsCheck_AllSameTypePredNum (sem_ArgCheck _argCheck ) _int )
sem_ArgsCheck (ExactList _typeList )  =
    (sem_ArgsCheck_ExactList _typeList )
sem_ArgsCheck (ExactPredList _argCheckList )  =
    (sem_ArgsCheck_ExactPredList _argCheckList )
-- semantic domain
type T_ArgsCheck  = ( )
data Inh_ArgsCheck  = Inh_ArgsCheck {}
data Syn_ArgsCheck  = Syn_ArgsCheck {}
wrap_ArgsCheck :: T_ArgsCheck  ->
                  Inh_ArgsCheck  ->
                  Syn_ArgsCheck 
wrap_ArgsCheck sem (Inh_ArgsCheck )  =
    (let ( ) =
             (sem )
     in  (Syn_ArgsCheck ))
sem_ArgsCheck_AllSameType :: T_Type  ->
                             T_ArgsCheck 
sem_ArgsCheck_AllSameType type_  =
    (let 
     in  ( ))
sem_ArgsCheck_AllSameType1 :: T_Type  ->
                              T_ArgsCheck 
sem_ArgsCheck_AllSameType1 type_  =
    (let 
     in  ( ))
sem_ArgsCheck_AllSameType1Any :: T_ArgsCheck 
sem_ArgsCheck_AllSameType1Any  =
    (let 
     in  ( ))
sem_ArgsCheck_AllSameTypeAny :: T_ArgsCheck 
sem_ArgsCheck_AllSameTypeAny  =
    (let 
     in  ( ))
sem_ArgsCheck_AllSameTypeNum :: T_Type  ->
                                Int ->
                                T_ArgsCheck 
sem_ArgsCheck_AllSameTypeNum type_ int_  =
    (let 
     in  ( ))
sem_ArgsCheck_AllSameTypeNumAny :: Int ->
                                   T_ArgsCheck 
sem_ArgsCheck_AllSameTypeNumAny int_  =
    (let 
     in  ( ))
sem_ArgsCheck_AllSameTypePredNum :: T_ArgCheck  ->
                                    Int ->
                                    T_ArgsCheck 
sem_ArgsCheck_AllSameTypePredNum argCheck_ int_  =
    (let 
     in  ( ))
sem_ArgsCheck_ExactList :: TypeList ->
                           T_ArgsCheck 
sem_ArgsCheck_ExactList typeList_  =
    (let 
     in  ( ))
sem_ArgsCheck_ExactPredList :: ArgCheckList ->
                               T_ArgsCheck 
sem_ArgsCheck_ExactPredList argCheckList_  =
    (let 
     in  ( ))
-- AttributeDef ------------------------------------------------
data AttributeDef  = AttributeDef (String) (TypeName) (Maybe Expression) (RowConstraintList) 
                   deriving ( Eq,Show)
-- cata
sem_AttributeDef :: AttributeDef  ->
                    T_AttributeDef 
sem_AttributeDef (AttributeDef _name _typ _check _cons )  =
    (sem_AttributeDef_AttributeDef _name (sem_TypeName _typ ) _check (sem_RowConstraintList _cons ) )
-- semantic domain
type T_AttributeDef  = Bool ->
                       MySourcePos ->
                       ( ([Message]),Type)
data Inh_AttributeDef  = Inh_AttributeDef {inLoop_Inh_AttributeDef :: Bool,sourcePos_Inh_AttributeDef :: MySourcePos}
data Syn_AttributeDef  = Syn_AttributeDef {messages_Syn_AttributeDef :: [Message],nodeType_Syn_AttributeDef :: Type}
wrap_AttributeDef :: T_AttributeDef  ->
                     Inh_AttributeDef  ->
                     Syn_AttributeDef 
wrap_AttributeDef sem (Inh_AttributeDef _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_AttributeDef _lhsOmessages _lhsOnodeType ))
sem_AttributeDef_AttributeDef :: String ->
                                 T_TypeName  ->
                                 (Maybe Expression) ->
                                 T_RowConstraintList  ->
                                 T_AttributeDef 
sem_AttributeDef_AttributeDef name_ typ_ check_ cons_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _typOinLoop :: Bool
              _typOsourcePos :: MySourcePos
              _consOinLoop :: Bool
              _consOsourcePos :: MySourcePos
              _typImessages :: ([Message])
              _typInodeType :: Type
              _consImessages :: ([Message])
              _consInodeType :: Type
              _lhsOmessages =
                  _typImessages ++ _consImessages
              _lhsOnodeType =
                  _typInodeType `setUnknown` _consInodeType
              _typOinLoop =
                  _lhsIinLoop
              _typOsourcePos =
                  _lhsIsourcePos
              _consOinLoop =
                  _lhsIinLoop
              _consOsourcePos =
                  _lhsIsourcePos
              ( _typImessages,_typInodeType) =
                  (typ_ _typOinLoop _typOsourcePos )
              ( _consImessages,_consInodeType) =
                  (cons_ _consOinLoop _consOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
-- AttributeDefList --------------------------------------------
type AttributeDefList  = [(AttributeDef)]
-- cata
sem_AttributeDefList :: AttributeDefList  ->
                        T_AttributeDefList 
sem_AttributeDefList list  =
    (Prelude.foldr sem_AttributeDefList_Cons sem_AttributeDefList_Nil (Prelude.map sem_AttributeDef list) )
-- semantic domain
type T_AttributeDefList  = Bool ->
                           MySourcePos ->
                           ( ([Message]),Type)
data Inh_AttributeDefList  = Inh_AttributeDefList {inLoop_Inh_AttributeDefList :: Bool,sourcePos_Inh_AttributeDefList :: MySourcePos}
data Syn_AttributeDefList  = Syn_AttributeDefList {messages_Syn_AttributeDefList :: [Message],nodeType_Syn_AttributeDefList :: Type}
wrap_AttributeDefList :: T_AttributeDefList  ->
                         Inh_AttributeDefList  ->
                         Syn_AttributeDefList 
wrap_AttributeDefList sem (Inh_AttributeDefList _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_AttributeDefList _lhsOmessages _lhsOnodeType ))
sem_AttributeDefList_Cons :: T_AttributeDef  ->
                             T_AttributeDefList  ->
                             T_AttributeDefList 
sem_AttributeDefList_Cons hd_ tl_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _hdOinLoop :: Bool
              _hdOsourcePos :: MySourcePos
              _tlOinLoop :: Bool
              _tlOsourcePos :: MySourcePos
              _hdImessages :: ([Message])
              _hdInodeType :: Type
              _tlImessages :: ([Message])
              _tlInodeType :: Type
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _lhsOnodeType =
                  _hdInodeType `appendTypeList` _tlInodeType
              _hdOinLoop =
                  _lhsIinLoop
              _hdOsourcePos =
                  _lhsIsourcePos
              _tlOinLoop =
                  _lhsIinLoop
              _tlOsourcePos =
                  _lhsIsourcePos
              ( _hdImessages,_hdInodeType) =
                  (hd_ _hdOinLoop _hdOsourcePos )
              ( _tlImessages,_tlInodeType) =
                  (tl_ _tlOinLoop _tlOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_AttributeDefList_Nil :: T_AttributeDefList 
sem_AttributeDefList_Nil  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  TypeList []
          in  ( _lhsOmessages,_lhsOnodeType)))
-- Cascade -----------------------------------------------------
data Cascade  = Cascade 
              | Restrict 
              deriving ( Eq,Show)
-- cata
sem_Cascade :: Cascade  ->
               T_Cascade 
sem_Cascade (Cascade )  =
    (sem_Cascade_Cascade )
sem_Cascade (Restrict )  =
    (sem_Cascade_Restrict )
-- semantic domain
type T_Cascade  = Bool ->
                  MySourcePos ->
                  ( ([Message]),Type)
data Inh_Cascade  = Inh_Cascade {inLoop_Inh_Cascade :: Bool,sourcePos_Inh_Cascade :: MySourcePos}
data Syn_Cascade  = Syn_Cascade {messages_Syn_Cascade :: [Message],nodeType_Syn_Cascade :: Type}
wrap_Cascade :: T_Cascade  ->
                Inh_Cascade  ->
                Syn_Cascade 
wrap_Cascade sem (Inh_Cascade _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_Cascade _lhsOmessages _lhsOnodeType ))
sem_Cascade_Cascade :: T_Cascade 
sem_Cascade_Cascade  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Cascade_Restrict :: T_Cascade 
sem_Cascade_Restrict  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
-- CaseExpressionList ------------------------------------------
type CaseExpressionList  = [(Expression)]
-- cata
sem_CaseExpressionList :: CaseExpressionList  ->
                          T_CaseExpressionList 
sem_CaseExpressionList list  =
    (Prelude.foldr sem_CaseExpressionList_Cons sem_CaseExpressionList_Nil (Prelude.map sem_Expression list) )
-- semantic domain
type T_CaseExpressionList  = Bool ->
                             MySourcePos ->
                             ( ([Message]),Type)
data Inh_CaseExpressionList  = Inh_CaseExpressionList {inLoop_Inh_CaseExpressionList :: Bool,sourcePos_Inh_CaseExpressionList :: MySourcePos}
data Syn_CaseExpressionList  = Syn_CaseExpressionList {messages_Syn_CaseExpressionList :: [Message],nodeType_Syn_CaseExpressionList :: Type}
wrap_CaseExpressionList :: T_CaseExpressionList  ->
                           Inh_CaseExpressionList  ->
                           Syn_CaseExpressionList 
wrap_CaseExpressionList sem (Inh_CaseExpressionList _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_CaseExpressionList _lhsOmessages _lhsOnodeType ))
sem_CaseExpressionList_Cons :: T_Expression  ->
                               T_CaseExpressionList  ->
                               T_CaseExpressionList 
sem_CaseExpressionList_Cons hd_ tl_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _hdOinLoop :: Bool
              _hdOsourcePos :: MySourcePos
              _tlOinLoop :: Bool
              _tlOsourcePos :: MySourcePos
              _hdImessages :: ([Message])
              _hdInodeType :: Type
              _tlImessages :: ([Message])
              _tlInodeType :: Type
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _lhsOnodeType =
                  _hdInodeType `appendTypeList` _tlInodeType
              _hdOinLoop =
                  _lhsIinLoop
              _hdOsourcePos =
                  _lhsIsourcePos
              _tlOinLoop =
                  _lhsIinLoop
              _tlOsourcePos =
                  _lhsIsourcePos
              ( _hdImessages,_hdInodeType) =
                  (hd_ _hdOinLoop _hdOsourcePos )
              ( _tlImessages,_tlInodeType) =
                  (tl_ _tlOinLoop _tlOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_CaseExpressionList_Nil :: T_CaseExpressionList 
sem_CaseExpressionList_Nil  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  TypeList []
          in  ( _lhsOmessages,_lhsOnodeType)))
-- CaseExpressionListExpressionPair ----------------------------
type CaseExpressionListExpressionPair  = ( (CaseExpressionList),(Expression))
-- cata
sem_CaseExpressionListExpressionPair :: CaseExpressionListExpressionPair  ->
                                        T_CaseExpressionListExpressionPair 
sem_CaseExpressionListExpressionPair ( x1,x2)  =
    (sem_CaseExpressionListExpressionPair_Tuple (sem_CaseExpressionList x1 ) (sem_Expression x2 ) )
-- semantic domain
type T_CaseExpressionListExpressionPair  = Bool ->
                                           MySourcePos ->
                                           ( ([Message]),Type)
data Inh_CaseExpressionListExpressionPair  = Inh_CaseExpressionListExpressionPair {inLoop_Inh_CaseExpressionListExpressionPair :: Bool,sourcePos_Inh_CaseExpressionListExpressionPair :: MySourcePos}
data Syn_CaseExpressionListExpressionPair  = Syn_CaseExpressionListExpressionPair {messages_Syn_CaseExpressionListExpressionPair :: [Message],nodeType_Syn_CaseExpressionListExpressionPair :: Type}
wrap_CaseExpressionListExpressionPair :: T_CaseExpressionListExpressionPair  ->
                                         Inh_CaseExpressionListExpressionPair  ->
                                         Syn_CaseExpressionListExpressionPair 
wrap_CaseExpressionListExpressionPair sem (Inh_CaseExpressionListExpressionPair _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_CaseExpressionListExpressionPair _lhsOmessages _lhsOnodeType ))
sem_CaseExpressionListExpressionPair_Tuple :: T_CaseExpressionList  ->
                                              T_Expression  ->
                                              T_CaseExpressionListExpressionPair 
sem_CaseExpressionListExpressionPair_Tuple x1_ x2_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOmessages :: ([Message])
              _x1OinLoop :: Bool
              _x1OsourcePos :: MySourcePos
              _x2OinLoop :: Bool
              _x2OsourcePos :: MySourcePos
              _x1Imessages :: ([Message])
              _x1InodeType :: Type
              _x2Imessages :: ([Message])
              _x2InodeType :: Type
              _lhsOnodeType =
                  checkTypes
                    _lhsIsourcePos
                    _x1InodeType
                    (AllSameType $ typeBool)
                    (ConstRetType _x2InodeType)
              _lhsOmessages =
                  _x1Imessages ++ _x2Imessages
              _x1OinLoop =
                  _lhsIinLoop
              _x1OsourcePos =
                  _lhsIsourcePos
              _x2OinLoop =
                  _lhsIinLoop
              _x2OsourcePos =
                  _lhsIsourcePos
              ( _x1Imessages,_x1InodeType) =
                  (x1_ _x1OinLoop _x1OsourcePos )
              ( _x2Imessages,_x2InodeType) =
                  (x2_ _x2OinLoop _x2OsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
-- CaseExpressionListExpressionPairList ------------------------
type CaseExpressionListExpressionPairList  = [(CaseExpressionListExpressionPair)]
-- cata
sem_CaseExpressionListExpressionPairList :: CaseExpressionListExpressionPairList  ->
                                            T_CaseExpressionListExpressionPairList 
sem_CaseExpressionListExpressionPairList list  =
    (Prelude.foldr sem_CaseExpressionListExpressionPairList_Cons sem_CaseExpressionListExpressionPairList_Nil (Prelude.map sem_CaseExpressionListExpressionPair list) )
-- semantic domain
type T_CaseExpressionListExpressionPairList  = Bool ->
                                               MySourcePos ->
                                               ( ([Message]),Type)
data Inh_CaseExpressionListExpressionPairList  = Inh_CaseExpressionListExpressionPairList {inLoop_Inh_CaseExpressionListExpressionPairList :: Bool,sourcePos_Inh_CaseExpressionListExpressionPairList :: MySourcePos}
data Syn_CaseExpressionListExpressionPairList  = Syn_CaseExpressionListExpressionPairList {messages_Syn_CaseExpressionListExpressionPairList :: [Message],nodeType_Syn_CaseExpressionListExpressionPairList :: Type}
wrap_CaseExpressionListExpressionPairList :: T_CaseExpressionListExpressionPairList  ->
                                             Inh_CaseExpressionListExpressionPairList  ->
                                             Syn_CaseExpressionListExpressionPairList 
wrap_CaseExpressionListExpressionPairList sem (Inh_CaseExpressionListExpressionPairList _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_CaseExpressionListExpressionPairList _lhsOmessages _lhsOnodeType ))
sem_CaseExpressionListExpressionPairList_Cons :: T_CaseExpressionListExpressionPair  ->
                                                 T_CaseExpressionListExpressionPairList  ->
                                                 T_CaseExpressionListExpressionPairList 
sem_CaseExpressionListExpressionPairList_Cons hd_ tl_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _hdOinLoop :: Bool
              _hdOsourcePos :: MySourcePos
              _tlOinLoop :: Bool
              _tlOsourcePos :: MySourcePos
              _hdImessages :: ([Message])
              _hdInodeType :: Type
              _tlImessages :: ([Message])
              _tlInodeType :: Type
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _lhsOnodeType =
                  _hdInodeType `appendTypeList` _tlInodeType
              _hdOinLoop =
                  _lhsIinLoop
              _hdOsourcePos =
                  _lhsIsourcePos
              _tlOinLoop =
                  _lhsIinLoop
              _tlOsourcePos =
                  _lhsIsourcePos
              ( _hdImessages,_hdInodeType) =
                  (hd_ _hdOinLoop _hdOsourcePos )
              ( _tlImessages,_tlInodeType) =
                  (tl_ _tlOinLoop _tlOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_CaseExpressionListExpressionPairList_Nil :: T_CaseExpressionListExpressionPairList 
sem_CaseExpressionListExpressionPairList_Nil  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  TypeList []
          in  ( _lhsOmessages,_lhsOnodeType)))
-- CombineType -------------------------------------------------
data CombineType  = Except 
                  | Intersect 
                  | Union 
                  | UnionAll 
                  deriving ( Eq,Show)
-- cata
sem_CombineType :: CombineType  ->
                   T_CombineType 
sem_CombineType (Except )  =
    (sem_CombineType_Except )
sem_CombineType (Intersect )  =
    (sem_CombineType_Intersect )
sem_CombineType (Union )  =
    (sem_CombineType_Union )
sem_CombineType (UnionAll )  =
    (sem_CombineType_UnionAll )
-- semantic domain
type T_CombineType  = Bool ->
                      MySourcePos ->
                      ( ([Message]),Type)
data Inh_CombineType  = Inh_CombineType {inLoop_Inh_CombineType :: Bool,sourcePos_Inh_CombineType :: MySourcePos}
data Syn_CombineType  = Syn_CombineType {messages_Syn_CombineType :: [Message],nodeType_Syn_CombineType :: Type}
wrap_CombineType :: T_CombineType  ->
                    Inh_CombineType  ->
                    Syn_CombineType 
wrap_CombineType sem (Inh_CombineType _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_CombineType _lhsOmessages _lhsOnodeType ))
sem_CombineType_Except :: T_CombineType 
sem_CombineType_Except  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_CombineType_Intersect :: T_CombineType 
sem_CombineType_Intersect  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_CombineType_Union :: T_CombineType 
sem_CombineType_Union  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_CombineType_UnionAll :: T_CombineType 
sem_CombineType_UnionAll  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
-- Constraint --------------------------------------------------
data Constraint  = CheckConstraint (Expression) 
                 | PrimaryKeyConstraint (StringList) 
                 | ReferenceConstraint (StringList) (String) (StringList) (Cascade) (Cascade) 
                 | UniqueConstraint (StringList) 
                 deriving ( Eq,Show)
-- cata
sem_Constraint :: Constraint  ->
                  T_Constraint 
sem_Constraint (CheckConstraint _expression )  =
    (sem_Constraint_CheckConstraint (sem_Expression _expression ) )
sem_Constraint (PrimaryKeyConstraint _stringList )  =
    (sem_Constraint_PrimaryKeyConstraint (sem_StringList _stringList ) )
sem_Constraint (ReferenceConstraint _atts _table _tableAtts _onUpdate _onDelete )  =
    (sem_Constraint_ReferenceConstraint (sem_StringList _atts ) _table (sem_StringList _tableAtts ) (sem_Cascade _onUpdate ) (sem_Cascade _onDelete ) )
sem_Constraint (UniqueConstraint _stringList )  =
    (sem_Constraint_UniqueConstraint (sem_StringList _stringList ) )
-- semantic domain
type T_Constraint  = Bool ->
                     MySourcePos ->
                     ( ([Message]),Type)
data Inh_Constraint  = Inh_Constraint {inLoop_Inh_Constraint :: Bool,sourcePos_Inh_Constraint :: MySourcePos}
data Syn_Constraint  = Syn_Constraint {messages_Syn_Constraint :: [Message],nodeType_Syn_Constraint :: Type}
wrap_Constraint :: T_Constraint  ->
                   Inh_Constraint  ->
                   Syn_Constraint 
wrap_Constraint sem (Inh_Constraint _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_Constraint _lhsOmessages _lhsOnodeType ))
sem_Constraint_CheckConstraint :: T_Expression  ->
                                  T_Constraint 
sem_Constraint_CheckConstraint expression_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _expressionOinLoop :: Bool
              _expressionOsourcePos :: MySourcePos
              _expressionImessages :: ([Message])
              _expressionInodeType :: Type
              _lhsOmessages =
                  _expressionImessages
              _lhsOnodeType =
                  _expressionInodeType
              _expressionOinLoop =
                  _lhsIinLoop
              _expressionOsourcePos =
                  _lhsIsourcePos
              ( _expressionImessages,_expressionInodeType) =
                  (expression_ _expressionOinLoop _expressionOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Constraint_PrimaryKeyConstraint :: T_StringList  ->
                                       T_Constraint 
sem_Constraint_PrimaryKeyConstraint stringList_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _stringListOinLoop :: Bool
              _stringListOsourcePos :: MySourcePos
              _stringListImessages :: ([Message])
              _stringListInodeType :: Type
              _lhsOmessages =
                  _stringListImessages
              _lhsOnodeType =
                  _stringListInodeType
              _stringListOinLoop =
                  _lhsIinLoop
              _stringListOsourcePos =
                  _lhsIsourcePos
              ( _stringListImessages,_stringListInodeType) =
                  (stringList_ _stringListOinLoop _stringListOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Constraint_ReferenceConstraint :: T_StringList  ->
                                      String ->
                                      T_StringList  ->
                                      T_Cascade  ->
                                      T_Cascade  ->
                                      T_Constraint 
sem_Constraint_ReferenceConstraint atts_ table_ tableAtts_ onUpdate_ onDelete_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _attsOinLoop :: Bool
              _attsOsourcePos :: MySourcePos
              _tableAttsOinLoop :: Bool
              _tableAttsOsourcePos :: MySourcePos
              _onUpdateOinLoop :: Bool
              _onUpdateOsourcePos :: MySourcePos
              _onDeleteOinLoop :: Bool
              _onDeleteOsourcePos :: MySourcePos
              _attsImessages :: ([Message])
              _attsInodeType :: Type
              _tableAttsImessages :: ([Message])
              _tableAttsInodeType :: Type
              _onUpdateImessages :: ([Message])
              _onUpdateInodeType :: Type
              _onDeleteImessages :: ([Message])
              _onDeleteInodeType :: Type
              _lhsOmessages =
                  _attsImessages ++ _tableAttsImessages ++ _onUpdateImessages ++ _onDeleteImessages
              _lhsOnodeType =
                  _attsInodeType `setUnknown` _tableAttsInodeType `setUnknown` _onUpdateInodeType `setUnknown` _onDeleteInodeType
              _attsOinLoop =
                  _lhsIinLoop
              _attsOsourcePos =
                  _lhsIsourcePos
              _tableAttsOinLoop =
                  _lhsIinLoop
              _tableAttsOsourcePos =
                  _lhsIsourcePos
              _onUpdateOinLoop =
                  _lhsIinLoop
              _onUpdateOsourcePos =
                  _lhsIsourcePos
              _onDeleteOinLoop =
                  _lhsIinLoop
              _onDeleteOsourcePos =
                  _lhsIsourcePos
              ( _attsImessages,_attsInodeType) =
                  (atts_ _attsOinLoop _attsOsourcePos )
              ( _tableAttsImessages,_tableAttsInodeType) =
                  (tableAtts_ _tableAttsOinLoop _tableAttsOsourcePos )
              ( _onUpdateImessages,_onUpdateInodeType) =
                  (onUpdate_ _onUpdateOinLoop _onUpdateOsourcePos )
              ( _onDeleteImessages,_onDeleteInodeType) =
                  (onDelete_ _onDeleteOinLoop _onDeleteOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Constraint_UniqueConstraint :: T_StringList  ->
                                   T_Constraint 
sem_Constraint_UniqueConstraint stringList_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _stringListOinLoop :: Bool
              _stringListOsourcePos :: MySourcePos
              _stringListImessages :: ([Message])
              _stringListInodeType :: Type
              _lhsOmessages =
                  _stringListImessages
              _lhsOnodeType =
                  _stringListInodeType
              _stringListOinLoop =
                  _lhsIinLoop
              _stringListOsourcePos =
                  _lhsIsourcePos
              ( _stringListImessages,_stringListInodeType) =
                  (stringList_ _stringListOinLoop _stringListOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
-- ConstraintList ----------------------------------------------
type ConstraintList  = [(Constraint)]
-- cata
sem_ConstraintList :: ConstraintList  ->
                      T_ConstraintList 
sem_ConstraintList list  =
    (Prelude.foldr sem_ConstraintList_Cons sem_ConstraintList_Nil (Prelude.map sem_Constraint list) )
-- semantic domain
type T_ConstraintList  = Bool ->
                         MySourcePos ->
                         ( ([Message]),Type)
data Inh_ConstraintList  = Inh_ConstraintList {inLoop_Inh_ConstraintList :: Bool,sourcePos_Inh_ConstraintList :: MySourcePos}
data Syn_ConstraintList  = Syn_ConstraintList {messages_Syn_ConstraintList :: [Message],nodeType_Syn_ConstraintList :: Type}
wrap_ConstraintList :: T_ConstraintList  ->
                       Inh_ConstraintList  ->
                       Syn_ConstraintList 
wrap_ConstraintList sem (Inh_ConstraintList _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_ConstraintList _lhsOmessages _lhsOnodeType ))
sem_ConstraintList_Cons :: T_Constraint  ->
                           T_ConstraintList  ->
                           T_ConstraintList 
sem_ConstraintList_Cons hd_ tl_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _hdOinLoop :: Bool
              _hdOsourcePos :: MySourcePos
              _tlOinLoop :: Bool
              _tlOsourcePos :: MySourcePos
              _hdImessages :: ([Message])
              _hdInodeType :: Type
              _tlImessages :: ([Message])
              _tlInodeType :: Type
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _lhsOnodeType =
                  _hdInodeType `appendTypeList` _tlInodeType
              _hdOinLoop =
                  _lhsIinLoop
              _hdOsourcePos =
                  _lhsIsourcePos
              _tlOinLoop =
                  _lhsIinLoop
              _tlOsourcePos =
                  _lhsIsourcePos
              ( _hdImessages,_hdInodeType) =
                  (hd_ _hdOinLoop _hdOsourcePos )
              ( _tlImessages,_tlInodeType) =
                  (tl_ _tlOinLoop _tlOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_ConstraintList_Nil :: T_ConstraintList 
sem_ConstraintList_Nil  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  TypeList []
          in  ( _lhsOmessages,_lhsOnodeType)))
-- CopySource --------------------------------------------------
data CopySource  = CopyFilename (String) 
                 | Stdin 
                 deriving ( Eq,Show)
-- cata
sem_CopySource :: CopySource  ->
                  T_CopySource 
sem_CopySource (CopyFilename _string )  =
    (sem_CopySource_CopyFilename _string )
sem_CopySource (Stdin )  =
    (sem_CopySource_Stdin )
-- semantic domain
type T_CopySource  = Bool ->
                     MySourcePos ->
                     ( ([Message]),Type)
data Inh_CopySource  = Inh_CopySource {inLoop_Inh_CopySource :: Bool,sourcePos_Inh_CopySource :: MySourcePos}
data Syn_CopySource  = Syn_CopySource {messages_Syn_CopySource :: [Message],nodeType_Syn_CopySource :: Type}
wrap_CopySource :: T_CopySource  ->
                   Inh_CopySource  ->
                   Syn_CopySource 
wrap_CopySource sem (Inh_CopySource _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_CopySource _lhsOmessages _lhsOnodeType ))
sem_CopySource_CopyFilename :: String ->
                               T_CopySource 
sem_CopySource_CopyFilename string_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_CopySource_Stdin :: T_CopySource 
sem_CopySource_Stdin  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
-- Direction ---------------------------------------------------
data Direction  = Asc 
                | Desc 
                deriving ( Eq,Show)
-- cata
sem_Direction :: Direction  ->
                 T_Direction 
sem_Direction (Asc )  =
    (sem_Direction_Asc )
sem_Direction (Desc )  =
    (sem_Direction_Desc )
-- semantic domain
type T_Direction  = Bool ->
                    MySourcePos ->
                    ( ([Message]),Type)
data Inh_Direction  = Inh_Direction {inLoop_Inh_Direction :: Bool,sourcePos_Inh_Direction :: MySourcePos}
data Syn_Direction  = Syn_Direction {messages_Syn_Direction :: [Message],nodeType_Syn_Direction :: Type}
wrap_Direction :: T_Direction  ->
                  Inh_Direction  ->
                  Syn_Direction 
wrap_Direction sem (Inh_Direction _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_Direction _lhsOmessages _lhsOnodeType ))
sem_Direction_Asc :: T_Direction 
sem_Direction_Asc  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Direction_Desc :: T_Direction 
sem_Direction_Desc  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
-- Distinct ----------------------------------------------------
data Distinct  = Distinct 
               | Dupes 
               deriving ( Eq,Show)
-- cata
sem_Distinct :: Distinct  ->
                T_Distinct 
sem_Distinct (Distinct )  =
    (sem_Distinct_Distinct )
sem_Distinct (Dupes )  =
    (sem_Distinct_Dupes )
-- semantic domain
type T_Distinct  = Bool ->
                   MySourcePos ->
                   ( ([Message]),Type)
data Inh_Distinct  = Inh_Distinct {inLoop_Inh_Distinct :: Bool,sourcePos_Inh_Distinct :: MySourcePos}
data Syn_Distinct  = Syn_Distinct {messages_Syn_Distinct :: [Message],nodeType_Syn_Distinct :: Type}
wrap_Distinct :: T_Distinct  ->
                 Inh_Distinct  ->
                 Syn_Distinct 
wrap_Distinct sem (Inh_Distinct _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_Distinct _lhsOmessages _lhsOnodeType ))
sem_Distinct_Distinct :: T_Distinct 
sem_Distinct_Distinct  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Distinct_Dupes :: T_Distinct 
sem_Distinct_Dupes  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
-- DropType ----------------------------------------------------
data DropType  = Domain 
               | Table 
               | Type 
               | View 
               deriving ( Eq,Show)
-- cata
sem_DropType :: DropType  ->
                T_DropType 
sem_DropType (Domain )  =
    (sem_DropType_Domain )
sem_DropType (Table )  =
    (sem_DropType_Table )
sem_DropType (Type )  =
    (sem_DropType_Type )
sem_DropType (View )  =
    (sem_DropType_View )
-- semantic domain
type T_DropType  = Bool ->
                   MySourcePos ->
                   ( ([Message]),Type)
data Inh_DropType  = Inh_DropType {inLoop_Inh_DropType :: Bool,sourcePos_Inh_DropType :: MySourcePos}
data Syn_DropType  = Syn_DropType {messages_Syn_DropType :: [Message],nodeType_Syn_DropType :: Type}
wrap_DropType :: T_DropType  ->
                 Inh_DropType  ->
                 Syn_DropType 
wrap_DropType sem (Inh_DropType _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_DropType _lhsOmessages _lhsOnodeType ))
sem_DropType_Domain :: T_DropType 
sem_DropType_Domain  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_DropType_Table :: T_DropType 
sem_DropType_Table  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_DropType_Type :: T_DropType 
sem_DropType_Type  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_DropType_View :: T_DropType 
sem_DropType_View  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
-- Expression --------------------------------------------------
data Expression  = BooleanLit (Bool) 
                 | Case (CaseExpressionListExpressionPairList) (MaybeExpression) 
                 | Cast (Expression) (TypeName) 
                 | Exists (SelectExpression) 
                 | FloatLit (Double) 
                 | FunCall (FunName) (ExpressionList) 
                 | Identifier (String) 
                 | InPredicate (Expression) (Bool) (InList) 
                 | IntegerLit (Integer) 
                 | NullLit 
                 | PositionalArg (Integer) 
                 | ScalarSubQuery (SelectExpression) 
                 | StringLit (String) (String) 
                 | WindowFn (Expression) (ExpressionList) (ExpressionList) (Direction) 
                 deriving ( Eq,Show)
-- cata
sem_Expression :: Expression  ->
                  T_Expression 
sem_Expression (BooleanLit _bool )  =
    (sem_Expression_BooleanLit _bool )
sem_Expression (Case _cases _els )  =
    (sem_Expression_Case (sem_CaseExpressionListExpressionPairList _cases ) (sem_MaybeExpression _els ) )
sem_Expression (Cast _expr _tn )  =
    (sem_Expression_Cast (sem_Expression _expr ) (sem_TypeName _tn ) )
sem_Expression (Exists _sel )  =
    (sem_Expression_Exists (sem_SelectExpression _sel ) )
sem_Expression (FloatLit _double )  =
    (sem_Expression_FloatLit _double )
sem_Expression (FunCall _funName _args )  =
    (sem_Expression_FunCall (sem_FunName _funName ) (sem_ExpressionList _args ) )
sem_Expression (Identifier _string )  =
    (sem_Expression_Identifier _string )
sem_Expression (InPredicate _expression _bool _inList )  =
    (sem_Expression_InPredicate (sem_Expression _expression ) _bool (sem_InList _inList ) )
sem_Expression (IntegerLit _integer )  =
    (sem_Expression_IntegerLit _integer )
sem_Expression (NullLit )  =
    (sem_Expression_NullLit )
sem_Expression (PositionalArg _integer )  =
    (sem_Expression_PositionalArg _integer )
sem_Expression (ScalarSubQuery _sel )  =
    (sem_Expression_ScalarSubQuery (sem_SelectExpression _sel ) )
sem_Expression (StringLit _quote _value )  =
    (sem_Expression_StringLit _quote _value )
sem_Expression (WindowFn _fn _partitionBy _orderBy _dir )  =
    (sem_Expression_WindowFn (sem_Expression _fn ) (sem_ExpressionList _partitionBy ) (sem_ExpressionList _orderBy ) (sem_Direction _dir ) )
-- semantic domain
type T_Expression  = Bool ->
                     MySourcePos ->
                     ( ([Message]),Type)
data Inh_Expression  = Inh_Expression {inLoop_Inh_Expression :: Bool,sourcePos_Inh_Expression :: MySourcePos}
data Syn_Expression  = Syn_Expression {messages_Syn_Expression :: [Message],nodeType_Syn_Expression :: Type}
wrap_Expression :: T_Expression  ->
                   Inh_Expression  ->
                   Syn_Expression 
wrap_Expression sem (Inh_Expression _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_Expression _lhsOmessages _lhsOnodeType ))
sem_Expression_BooleanLit :: Bool ->
                             T_Expression 
sem_Expression_BooleanLit bool_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOmessages :: ([Message])
              _lhsOnodeType =
                  typeBool
              _lhsOmessages =
                  []
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Expression_Case :: T_CaseExpressionListExpressionPairList  ->
                       T_MaybeExpression  ->
                       T_Expression 
sem_Expression_Case cases_ els_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOmessages :: ([Message])
              _casesOinLoop :: Bool
              _casesOsourcePos :: MySourcePos
              _elsOinLoop :: Bool
              _elsOsourcePos :: MySourcePos
              _casesImessages :: ([Message])
              _casesInodeType :: Type
              _elsImessages :: ([Message])
              _elsInodeType :: Type
              _lhsOnodeType =
                  checkTypes
                     _lhsIsourcePos
                     (case _elsInodeType of
                        Pseudo AnyElement -> _casesInodeType
                        e -> TypeList
                               ((typesFromTypeList _casesInodeType)
                                ++ [e]))
                     AllSameTypeAny
                     (RetTypeAsArgN 0)
              _lhsOmessages =
                  _casesImessages ++ _elsImessages
              _casesOinLoop =
                  _lhsIinLoop
              _casesOsourcePos =
                  _lhsIsourcePos
              _elsOinLoop =
                  _lhsIinLoop
              _elsOsourcePos =
                  _lhsIsourcePos
              ( _casesImessages,_casesInodeType) =
                  (cases_ _casesOinLoop _casesOsourcePos )
              ( _elsImessages,_elsInodeType) =
                  (els_ _elsOinLoop _elsOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Expression_Cast :: T_Expression  ->
                       T_TypeName  ->
                       T_Expression 
sem_Expression_Cast expr_ tn_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOmessages :: ([Message])
              _exprOinLoop :: Bool
              _exprOsourcePos :: MySourcePos
              _tnOinLoop :: Bool
              _tnOsourcePos :: MySourcePos
              _exprImessages :: ([Message])
              _exprInodeType :: Type
              _tnImessages :: ([Message])
              _tnInodeType :: Type
              _lhsOnodeType =
                  propagateUnknownError
                    (TypeList
                     [_exprInodeType])
                    _tnInodeType
              _lhsOmessages =
                  _exprImessages ++ _tnImessages
              _exprOinLoop =
                  _lhsIinLoop
              _exprOsourcePos =
                  _lhsIsourcePos
              _tnOinLoop =
                  _lhsIinLoop
              _tnOsourcePos =
                  _lhsIsourcePos
              ( _exprImessages,_exprInodeType) =
                  (expr_ _exprOinLoop _exprOsourcePos )
              ( _tnImessages,_tnInodeType) =
                  (tn_ _tnOinLoop _tnOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Expression_Exists :: T_SelectExpression  ->
                         T_Expression 
sem_Expression_Exists sel_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _selOinLoop :: Bool
              _selOsourcePos :: MySourcePos
              _selImessages :: ([Message])
              _selInodeType :: Type
              _lhsOmessages =
                  _selImessages
              _lhsOnodeType =
                  _selInodeType
              _selOinLoop =
                  _lhsIinLoop
              _selOsourcePos =
                  _lhsIsourcePos
              ( _selImessages,_selInodeType) =
                  (sel_ _selOinLoop _selOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Expression_FloatLit :: Double ->
                           T_Expression 
sem_Expression_FloatLit double_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOmessages :: ([Message])
              _lhsOnodeType =
                  typeFloat4
              _lhsOmessages =
                  []
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Expression_FunCall :: T_FunName  ->
                          T_ExpressionList  ->
                          T_Expression 
sem_Expression_FunCall funName_ args_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOmessages :: ([Message])
              _funNameOinLoop :: Bool
              _funNameOsourcePos :: MySourcePos
              _argsOinLoop :: Bool
              _argsOsourcePos :: MySourcePos
              _funNameImessages :: ([Message])
              _funNameInodeType :: Type
              _funNameIval :: FunName
              _argsImessages :: ([Message])
              _argsInodeType :: Type
              _lhsOnodeType =
                  case _funNameIval of
                    ArrayCtor -> ct AllSameType1Any
                                   (RetTypeFun (\t -> ArrayType $ head t))
                    Substring -> ct
                                   (ExactList [ScalarType "text"
                                              ,ScalarType "int4"
                                              ,ScalarType "int4"])
                                   (ConstRetType (ScalarType "text"))
                    Between -> ct
                                   (AllSameTypeNumAny 3)
                                   (ConstRetType (typeBool))
                    ArraySub -> ct
                                   (ExactPredList
                                     [ArgCheck isArrayType NotArrayType
                                     ,exactType (ScalarType "int4")])
                                   (RetTypeFun (\t -> typeFromArray $ head t))
                    Operator s -> lookupFn s (typesFromTypeList _argsInodeType)
                    KOperator k -> lookupKop k (typesFromTypeList _argsInodeType)
                    SimpleFun f -> lookupFn f (typesFromTypeList _argsInodeType)
                    _ -> UnknownType
                  where
                    ct = checkTypes _lhsIsourcePos _argsInodeType
                    lookupFn s1 args = let s = (if s1 == "u-" then "-" else s1)
                                           cands = filter (\(o,a,_) ->
                                                             (o,a) == (s,args))
                                                     allOpsAndFns
                                       in case () of
                                           _ | length cands == 0 -> TypeError _lhsIsourcePos (NoMatchingOperator s args)
                                             | length cands == 1 -> let (_,_,rettype) = (head cands)
                                                                    in rettype
                                             | otherwise -> TypeError _lhsIsourcePos (MultipleMatchingOperators s args)
                    lookupKop s args = let cands = filter (\(o,a,_) ->
                                                             (o,a) == (s,args))
                                                     allKeywordOps
                                       in case () of
                                           _ | length cands == 0 -> TypeError _lhsIsourcePos (NoMatchingKOperator s args)
                                             | length cands == 1 -> let (_,_,rettype) = (head cands)
                                                                    in rettype
                                             | otherwise -> TypeError _lhsIsourcePos (MultipleMatchingKOperators s args)
              _lhsOmessages =
                  _funNameImessages ++ _argsImessages
              _funNameOinLoop =
                  _lhsIinLoop
              _funNameOsourcePos =
                  _lhsIsourcePos
              _argsOinLoop =
                  _lhsIinLoop
              _argsOsourcePos =
                  _lhsIsourcePos
              ( _funNameImessages,_funNameInodeType,_funNameIval) =
                  (funName_ _funNameOinLoop _funNameOsourcePos )
              ( _argsImessages,_argsInodeType) =
                  (args_ _argsOinLoop _argsOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Expression_Identifier :: String ->
                             T_Expression 
sem_Expression_Identifier string_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Expression_InPredicate :: T_Expression  ->
                              Bool ->
                              T_InList  ->
                              T_Expression 
sem_Expression_InPredicate expression_ bool_ inList_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _expressionOinLoop :: Bool
              _expressionOsourcePos :: MySourcePos
              _inListOinLoop :: Bool
              _inListOsourcePos :: MySourcePos
              _expressionImessages :: ([Message])
              _expressionInodeType :: Type
              _inListImessages :: ([Message])
              _inListInodeType :: Type
              _lhsOmessages =
                  _expressionImessages ++ _inListImessages
              _lhsOnodeType =
                  _expressionInodeType `setUnknown` _inListInodeType
              _expressionOinLoop =
                  _lhsIinLoop
              _expressionOsourcePos =
                  _lhsIsourcePos
              _inListOinLoop =
                  _lhsIinLoop
              _inListOsourcePos =
                  _lhsIsourcePos
              ( _expressionImessages,_expressionInodeType) =
                  (expression_ _expressionOinLoop _expressionOsourcePos )
              ( _inListImessages,_inListInodeType) =
                  (inList_ _inListOinLoop _inListOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Expression_IntegerLit :: Integer ->
                             T_Expression 
sem_Expression_IntegerLit integer_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOmessages :: ([Message])
              _lhsOnodeType =
                  typeInt
              _lhsOmessages =
                  []
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Expression_NullLit :: T_Expression 
sem_Expression_NullLit  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Expression_PositionalArg :: Integer ->
                                T_Expression 
sem_Expression_PositionalArg integer_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Expression_ScalarSubQuery :: T_SelectExpression  ->
                                 T_Expression 
sem_Expression_ScalarSubQuery sel_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _selOinLoop :: Bool
              _selOsourcePos :: MySourcePos
              _selImessages :: ([Message])
              _selInodeType :: Type
              _lhsOmessages =
                  _selImessages
              _lhsOnodeType =
                  _selInodeType
              _selOinLoop =
                  _lhsIinLoop
              _selOsourcePos =
                  _lhsIsourcePos
              ( _selImessages,_selInodeType) =
                  (sel_ _selOinLoop _selOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Expression_StringLit :: String ->
                            String ->
                            T_Expression 
sem_Expression_StringLit quote_ value_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOmessages :: ([Message])
              _lhsOnodeType =
                  ScalarType "text"
              _lhsOmessages =
                  []
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Expression_WindowFn :: T_Expression  ->
                           T_ExpressionList  ->
                           T_ExpressionList  ->
                           T_Direction  ->
                           T_Expression 
sem_Expression_WindowFn fn_ partitionBy_ orderBy_ dir_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _fnOinLoop :: Bool
              _fnOsourcePos :: MySourcePos
              _partitionByOinLoop :: Bool
              _partitionByOsourcePos :: MySourcePos
              _orderByOinLoop :: Bool
              _orderByOsourcePos :: MySourcePos
              _dirOinLoop :: Bool
              _dirOsourcePos :: MySourcePos
              _fnImessages :: ([Message])
              _fnInodeType :: Type
              _partitionByImessages :: ([Message])
              _partitionByInodeType :: Type
              _orderByImessages :: ([Message])
              _orderByInodeType :: Type
              _dirImessages :: ([Message])
              _dirInodeType :: Type
              _lhsOmessages =
                  _fnImessages ++ _partitionByImessages ++ _orderByImessages ++ _dirImessages
              _lhsOnodeType =
                  _fnInodeType `setUnknown` _partitionByInodeType `setUnknown` _orderByInodeType `setUnknown` _dirInodeType
              _fnOinLoop =
                  _lhsIinLoop
              _fnOsourcePos =
                  _lhsIsourcePos
              _partitionByOinLoop =
                  _lhsIinLoop
              _partitionByOsourcePos =
                  _lhsIsourcePos
              _orderByOinLoop =
                  _lhsIinLoop
              _orderByOsourcePos =
                  _lhsIsourcePos
              _dirOinLoop =
                  _lhsIinLoop
              _dirOsourcePos =
                  _lhsIsourcePos
              ( _fnImessages,_fnInodeType) =
                  (fn_ _fnOinLoop _fnOsourcePos )
              ( _partitionByImessages,_partitionByInodeType) =
                  (partitionBy_ _partitionByOinLoop _partitionByOsourcePos )
              ( _orderByImessages,_orderByInodeType) =
                  (orderBy_ _orderByOinLoop _orderByOsourcePos )
              ( _dirImessages,_dirInodeType) =
                  (dir_ _dirOinLoop _dirOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
-- ExpressionList ----------------------------------------------
type ExpressionList  = [(Expression)]
-- cata
sem_ExpressionList :: ExpressionList  ->
                      T_ExpressionList 
sem_ExpressionList list  =
    (Prelude.foldr sem_ExpressionList_Cons sem_ExpressionList_Nil (Prelude.map sem_Expression list) )
-- semantic domain
type T_ExpressionList  = Bool ->
                         MySourcePos ->
                         ( ([Message]),Type)
data Inh_ExpressionList  = Inh_ExpressionList {inLoop_Inh_ExpressionList :: Bool,sourcePos_Inh_ExpressionList :: MySourcePos}
data Syn_ExpressionList  = Syn_ExpressionList {messages_Syn_ExpressionList :: [Message],nodeType_Syn_ExpressionList :: Type}
wrap_ExpressionList :: T_ExpressionList  ->
                       Inh_ExpressionList  ->
                       Syn_ExpressionList 
wrap_ExpressionList sem (Inh_ExpressionList _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_ExpressionList _lhsOmessages _lhsOnodeType ))
sem_ExpressionList_Cons :: T_Expression  ->
                           T_ExpressionList  ->
                           T_ExpressionList 
sem_ExpressionList_Cons hd_ tl_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _hdOinLoop :: Bool
              _hdOsourcePos :: MySourcePos
              _tlOinLoop :: Bool
              _tlOsourcePos :: MySourcePos
              _hdImessages :: ([Message])
              _hdInodeType :: Type
              _tlImessages :: ([Message])
              _tlInodeType :: Type
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _lhsOnodeType =
                  _hdInodeType `appendTypeList` _tlInodeType
              _hdOinLoop =
                  _lhsIinLoop
              _hdOsourcePos =
                  _lhsIsourcePos
              _tlOinLoop =
                  _lhsIinLoop
              _tlOsourcePos =
                  _lhsIsourcePos
              ( _hdImessages,_hdInodeType) =
                  (hd_ _hdOinLoop _hdOsourcePos )
              ( _tlImessages,_tlInodeType) =
                  (tl_ _tlOinLoop _tlOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_ExpressionList_Nil :: T_ExpressionList 
sem_ExpressionList_Nil  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  TypeList []
          in  ( _lhsOmessages,_lhsOnodeType)))
-- ExpressionListList ------------------------------------------
type ExpressionListList  = [(ExpressionList)]
-- cata
sem_ExpressionListList :: ExpressionListList  ->
                          T_ExpressionListList 
sem_ExpressionListList list  =
    (Prelude.foldr sem_ExpressionListList_Cons sem_ExpressionListList_Nil (Prelude.map sem_ExpressionList list) )
-- semantic domain
type T_ExpressionListList  = Bool ->
                             MySourcePos ->
                             ( ([Message]),Type)
data Inh_ExpressionListList  = Inh_ExpressionListList {inLoop_Inh_ExpressionListList :: Bool,sourcePos_Inh_ExpressionListList :: MySourcePos}
data Syn_ExpressionListList  = Syn_ExpressionListList {messages_Syn_ExpressionListList :: [Message],nodeType_Syn_ExpressionListList :: Type}
wrap_ExpressionListList :: T_ExpressionListList  ->
                           Inh_ExpressionListList  ->
                           Syn_ExpressionListList 
wrap_ExpressionListList sem (Inh_ExpressionListList _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_ExpressionListList _lhsOmessages _lhsOnodeType ))
sem_ExpressionListList_Cons :: T_ExpressionList  ->
                               T_ExpressionListList  ->
                               T_ExpressionListList 
sem_ExpressionListList_Cons hd_ tl_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _hdOinLoop :: Bool
              _hdOsourcePos :: MySourcePos
              _tlOinLoop :: Bool
              _tlOsourcePos :: MySourcePos
              _hdImessages :: ([Message])
              _hdInodeType :: Type
              _tlImessages :: ([Message])
              _tlInodeType :: Type
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _lhsOnodeType =
                  _hdInodeType `appendTypeList` _tlInodeType
              _hdOinLoop =
                  _lhsIinLoop
              _hdOsourcePos =
                  _lhsIsourcePos
              _tlOinLoop =
                  _lhsIinLoop
              _tlOsourcePos =
                  _lhsIsourcePos
              ( _hdImessages,_hdInodeType) =
                  (hd_ _hdOinLoop _hdOsourcePos )
              ( _tlImessages,_tlInodeType) =
                  (tl_ _tlOinLoop _tlOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_ExpressionListList_Nil :: T_ExpressionListList 
sem_ExpressionListList_Nil  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  TypeList []
          in  ( _lhsOmessages,_lhsOnodeType)))
-- ExpressionListStatementListPair -----------------------------
type ExpressionListStatementListPair  = ( (ExpressionList),(StatementList))
-- cata
sem_ExpressionListStatementListPair :: ExpressionListStatementListPair  ->
                                       T_ExpressionListStatementListPair 
sem_ExpressionListStatementListPair ( x1,x2)  =
    (sem_ExpressionListStatementListPair_Tuple (sem_ExpressionList x1 ) (sem_StatementList x2 ) )
-- semantic domain
type T_ExpressionListStatementListPair  = Bool ->
                                          MySourcePos ->
                                          ( ([Message]),Type)
data Inh_ExpressionListStatementListPair  = Inh_ExpressionListStatementListPair {inLoop_Inh_ExpressionListStatementListPair :: Bool,sourcePos_Inh_ExpressionListStatementListPair :: MySourcePos}
data Syn_ExpressionListStatementListPair  = Syn_ExpressionListStatementListPair {messages_Syn_ExpressionListStatementListPair :: [Message],nodeType_Syn_ExpressionListStatementListPair :: Type}
wrap_ExpressionListStatementListPair :: T_ExpressionListStatementListPair  ->
                                        Inh_ExpressionListStatementListPair  ->
                                        Syn_ExpressionListStatementListPair 
wrap_ExpressionListStatementListPair sem (Inh_ExpressionListStatementListPair _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_ExpressionListStatementListPair _lhsOmessages _lhsOnodeType ))
sem_ExpressionListStatementListPair_Tuple :: T_ExpressionList  ->
                                             T_StatementList  ->
                                             T_ExpressionListStatementListPair 
sem_ExpressionListStatementListPair_Tuple x1_ x2_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _x1OinLoop :: Bool
              _x1OsourcePos :: MySourcePos
              _x2OinLoop :: Bool
              _x2OsourcePos :: MySourcePos
              _x1Imessages :: ([Message])
              _x1InodeType :: Type
              _x2Imessages :: ([Message])
              _x2InodeType :: Type
              _lhsOmessages =
                  _x1Imessages ++ _x2Imessages
              _lhsOnodeType =
                  _x1InodeType `setUnknown` _x2InodeType
              _x1OinLoop =
                  _lhsIinLoop
              _x1OsourcePos =
                  _lhsIsourcePos
              _x2OinLoop =
                  _lhsIinLoop
              _x2OsourcePos =
                  _lhsIsourcePos
              ( _x1Imessages,_x1InodeType) =
                  (x1_ _x1OinLoop _x1OsourcePos )
              ( _x2Imessages,_x2InodeType) =
                  (x2_ _x2OinLoop _x2OsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
-- ExpressionListStatementListPairList -------------------------
type ExpressionListStatementListPairList  = [(ExpressionListStatementListPair)]
-- cata
sem_ExpressionListStatementListPairList :: ExpressionListStatementListPairList  ->
                                           T_ExpressionListStatementListPairList 
sem_ExpressionListStatementListPairList list  =
    (Prelude.foldr sem_ExpressionListStatementListPairList_Cons sem_ExpressionListStatementListPairList_Nil (Prelude.map sem_ExpressionListStatementListPair list) )
-- semantic domain
type T_ExpressionListStatementListPairList  = Bool ->
                                              MySourcePos ->
                                              ( ([Message]),Type)
data Inh_ExpressionListStatementListPairList  = Inh_ExpressionListStatementListPairList {inLoop_Inh_ExpressionListStatementListPairList :: Bool,sourcePos_Inh_ExpressionListStatementListPairList :: MySourcePos}
data Syn_ExpressionListStatementListPairList  = Syn_ExpressionListStatementListPairList {messages_Syn_ExpressionListStatementListPairList :: [Message],nodeType_Syn_ExpressionListStatementListPairList :: Type}
wrap_ExpressionListStatementListPairList :: T_ExpressionListStatementListPairList  ->
                                            Inh_ExpressionListStatementListPairList  ->
                                            Syn_ExpressionListStatementListPairList 
wrap_ExpressionListStatementListPairList sem (Inh_ExpressionListStatementListPairList _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_ExpressionListStatementListPairList _lhsOmessages _lhsOnodeType ))
sem_ExpressionListStatementListPairList_Cons :: T_ExpressionListStatementListPair  ->
                                                T_ExpressionListStatementListPairList  ->
                                                T_ExpressionListStatementListPairList 
sem_ExpressionListStatementListPairList_Cons hd_ tl_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _hdOinLoop :: Bool
              _hdOsourcePos :: MySourcePos
              _tlOinLoop :: Bool
              _tlOsourcePos :: MySourcePos
              _hdImessages :: ([Message])
              _hdInodeType :: Type
              _tlImessages :: ([Message])
              _tlInodeType :: Type
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _lhsOnodeType =
                  _hdInodeType `appendTypeList` _tlInodeType
              _hdOinLoop =
                  _lhsIinLoop
              _hdOsourcePos =
                  _lhsIsourcePos
              _tlOinLoop =
                  _lhsIinLoop
              _tlOsourcePos =
                  _lhsIsourcePos
              ( _hdImessages,_hdInodeType) =
                  (hd_ _hdOinLoop _hdOsourcePos )
              ( _tlImessages,_tlInodeType) =
                  (tl_ _tlOinLoop _tlOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_ExpressionListStatementListPairList_Nil :: T_ExpressionListStatementListPairList 
sem_ExpressionListStatementListPairList_Nil  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  TypeList []
          in  ( _lhsOmessages,_lhsOnodeType)))
-- ExpressionRoot ----------------------------------------------
data ExpressionRoot  = ExpressionRoot (Expression) 
                     deriving ( Show)
-- cata
sem_ExpressionRoot :: ExpressionRoot  ->
                      T_ExpressionRoot 
sem_ExpressionRoot (ExpressionRoot _expr )  =
    (sem_ExpressionRoot_ExpressionRoot (sem_Expression _expr ) )
-- semantic domain
type T_ExpressionRoot  = ( ([Message]),Type)
data Inh_ExpressionRoot  = Inh_ExpressionRoot {}
data Syn_ExpressionRoot  = Syn_ExpressionRoot {messages_Syn_ExpressionRoot :: [Message],nodeType_Syn_ExpressionRoot :: Type}
wrap_ExpressionRoot :: T_ExpressionRoot  ->
                       Inh_ExpressionRoot  ->
                       Syn_ExpressionRoot 
wrap_ExpressionRoot sem (Inh_ExpressionRoot )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem )
     in  (Syn_ExpressionRoot _lhsOmessages _lhsOnodeType ))
sem_ExpressionRoot_ExpressionRoot :: T_Expression  ->
                                     T_ExpressionRoot 
sem_ExpressionRoot_ExpressionRoot expr_  =
    (let _exprOsourcePos :: MySourcePos
         _exprOinLoop :: Bool
         _lhsOmessages :: ([Message])
         _lhsOnodeType :: Type
         _exprImessages :: ([Message])
         _exprInodeType :: Type
         _exprOsourcePos =
             ("",0,0)
         _exprOinLoop =
             False
         _lhsOmessages =
             _exprImessages
         _lhsOnodeType =
             _exprInodeType
         ( _exprImessages,_exprInodeType) =
             (expr_ _exprOinLoop _exprOsourcePos )
     in  ( _lhsOmessages,_lhsOnodeType))
-- ExpressionStatementListPair ---------------------------------
type ExpressionStatementListPair  = ( (Expression),(StatementList))
-- cata
sem_ExpressionStatementListPair :: ExpressionStatementListPair  ->
                                   T_ExpressionStatementListPair 
sem_ExpressionStatementListPair ( x1,x2)  =
    (sem_ExpressionStatementListPair_Tuple (sem_Expression x1 ) (sem_StatementList x2 ) )
-- semantic domain
type T_ExpressionStatementListPair  = Bool ->
                                      MySourcePos ->
                                      ( ([Message]),Type)
data Inh_ExpressionStatementListPair  = Inh_ExpressionStatementListPair {inLoop_Inh_ExpressionStatementListPair :: Bool,sourcePos_Inh_ExpressionStatementListPair :: MySourcePos}
data Syn_ExpressionStatementListPair  = Syn_ExpressionStatementListPair {messages_Syn_ExpressionStatementListPair :: [Message],nodeType_Syn_ExpressionStatementListPair :: Type}
wrap_ExpressionStatementListPair :: T_ExpressionStatementListPair  ->
                                    Inh_ExpressionStatementListPair  ->
                                    Syn_ExpressionStatementListPair 
wrap_ExpressionStatementListPair sem (Inh_ExpressionStatementListPair _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_ExpressionStatementListPair _lhsOmessages _lhsOnodeType ))
sem_ExpressionStatementListPair_Tuple :: T_Expression  ->
                                         T_StatementList  ->
                                         T_ExpressionStatementListPair 
sem_ExpressionStatementListPair_Tuple x1_ x2_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _x1OinLoop :: Bool
              _x1OsourcePos :: MySourcePos
              _x2OinLoop :: Bool
              _x2OsourcePos :: MySourcePos
              _x1Imessages :: ([Message])
              _x1InodeType :: Type
              _x2Imessages :: ([Message])
              _x2InodeType :: Type
              _lhsOmessages =
                  _x1Imessages ++ _x2Imessages
              _lhsOnodeType =
                  _x1InodeType `setUnknown` _x2InodeType
              _x1OinLoop =
                  _lhsIinLoop
              _x1OsourcePos =
                  _lhsIsourcePos
              _x2OinLoop =
                  _lhsIinLoop
              _x2OsourcePos =
                  _lhsIsourcePos
              ( _x1Imessages,_x1InodeType) =
                  (x1_ _x1OinLoop _x1OsourcePos )
              ( _x2Imessages,_x2InodeType) =
                  (x2_ _x2OinLoop _x2OsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
-- ExpressionStatementListPairList -----------------------------
type ExpressionStatementListPairList  = [(ExpressionStatementListPair)]
-- cata
sem_ExpressionStatementListPairList :: ExpressionStatementListPairList  ->
                                       T_ExpressionStatementListPairList 
sem_ExpressionStatementListPairList list  =
    (Prelude.foldr sem_ExpressionStatementListPairList_Cons sem_ExpressionStatementListPairList_Nil (Prelude.map sem_ExpressionStatementListPair list) )
-- semantic domain
type T_ExpressionStatementListPairList  = Bool ->
                                          MySourcePos ->
                                          ( ([Message]),Type)
data Inh_ExpressionStatementListPairList  = Inh_ExpressionStatementListPairList {inLoop_Inh_ExpressionStatementListPairList :: Bool,sourcePos_Inh_ExpressionStatementListPairList :: MySourcePos}
data Syn_ExpressionStatementListPairList  = Syn_ExpressionStatementListPairList {messages_Syn_ExpressionStatementListPairList :: [Message],nodeType_Syn_ExpressionStatementListPairList :: Type}
wrap_ExpressionStatementListPairList :: T_ExpressionStatementListPairList  ->
                                        Inh_ExpressionStatementListPairList  ->
                                        Syn_ExpressionStatementListPairList 
wrap_ExpressionStatementListPairList sem (Inh_ExpressionStatementListPairList _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_ExpressionStatementListPairList _lhsOmessages _lhsOnodeType ))
sem_ExpressionStatementListPairList_Cons :: T_ExpressionStatementListPair  ->
                                            T_ExpressionStatementListPairList  ->
                                            T_ExpressionStatementListPairList 
sem_ExpressionStatementListPairList_Cons hd_ tl_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _hdOinLoop :: Bool
              _hdOsourcePos :: MySourcePos
              _tlOinLoop :: Bool
              _tlOsourcePos :: MySourcePos
              _hdImessages :: ([Message])
              _hdInodeType :: Type
              _tlImessages :: ([Message])
              _tlInodeType :: Type
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _lhsOnodeType =
                  _hdInodeType `appendTypeList` _tlInodeType
              _hdOinLoop =
                  _lhsIinLoop
              _hdOsourcePos =
                  _lhsIsourcePos
              _tlOinLoop =
                  _lhsIinLoop
              _tlOsourcePos =
                  _lhsIsourcePos
              ( _hdImessages,_hdInodeType) =
                  (hd_ _hdOinLoop _hdOsourcePos )
              ( _tlImessages,_tlInodeType) =
                  (tl_ _tlOinLoop _tlOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_ExpressionStatementListPairList_Nil :: T_ExpressionStatementListPairList 
sem_ExpressionStatementListPairList_Nil  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  TypeList []
          in  ( _lhsOmessages,_lhsOnodeType)))
-- FnBody ------------------------------------------------------
data FnBody  = PlpgsqlFnBody (VarDefList) (StatementList) 
             | SqlFnBody (StatementList) 
             deriving ( Eq,Show)
-- cata
sem_FnBody :: FnBody  ->
              T_FnBody 
sem_FnBody (PlpgsqlFnBody _varDefList _sts )  =
    (sem_FnBody_PlpgsqlFnBody (sem_VarDefList _varDefList ) (sem_StatementList _sts ) )
sem_FnBody (SqlFnBody _sts )  =
    (sem_FnBody_SqlFnBody (sem_StatementList _sts ) )
-- semantic domain
type T_FnBody  = Bool ->
                 MySourcePos ->
                 ( ([Message]),Type)
data Inh_FnBody  = Inh_FnBody {inLoop_Inh_FnBody :: Bool,sourcePos_Inh_FnBody :: MySourcePos}
data Syn_FnBody  = Syn_FnBody {messages_Syn_FnBody :: [Message],nodeType_Syn_FnBody :: Type}
wrap_FnBody :: T_FnBody  ->
               Inh_FnBody  ->
               Syn_FnBody 
wrap_FnBody sem (Inh_FnBody _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_FnBody _lhsOmessages _lhsOnodeType ))
sem_FnBody_PlpgsqlFnBody :: T_VarDefList  ->
                            T_StatementList  ->
                            T_FnBody 
sem_FnBody_PlpgsqlFnBody varDefList_ sts_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _varDefListOinLoop :: Bool
              _varDefListOsourcePos :: MySourcePos
              _stsOinLoop :: Bool
              _stsOsourcePos :: MySourcePos
              _varDefListImessages :: ([Message])
              _varDefListInodeType :: Type
              _stsImessages :: ([Message])
              _stsInodeType :: Type
              _lhsOmessages =
                  _varDefListImessages ++ _stsImessages
              _lhsOnodeType =
                  _varDefListInodeType `setUnknown` _stsInodeType
              _varDefListOinLoop =
                  _lhsIinLoop
              _varDefListOsourcePos =
                  _lhsIsourcePos
              _stsOinLoop =
                  _lhsIinLoop
              _stsOsourcePos =
                  _lhsIsourcePos
              ( _varDefListImessages,_varDefListInodeType) =
                  (varDefList_ _varDefListOinLoop _varDefListOsourcePos )
              ( _stsImessages,_stsInodeType) =
                  (sts_ _stsOinLoop _stsOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_FnBody_SqlFnBody :: T_StatementList  ->
                        T_FnBody 
sem_FnBody_SqlFnBody sts_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _stsOinLoop :: Bool
              _stsOsourcePos :: MySourcePos
              _stsImessages :: ([Message])
              _stsInodeType :: Type
              _lhsOmessages =
                  _stsImessages
              _lhsOnodeType =
                  _stsInodeType
              _stsOinLoop =
                  _lhsIinLoop
              _stsOsourcePos =
                  _lhsIsourcePos
              ( _stsImessages,_stsInodeType) =
                  (sts_ _stsOinLoop _stsOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
-- FunName -----------------------------------------------------
data FunName  = ArrayCtor 
              | ArraySub 
              | Between 
              | KOperator (KeywordOperator) 
              | Operator (String) 
              | RowCtor 
              | SimpleFun (String) 
              | Substring 
              deriving ( Eq,Show)
-- cata
sem_FunName :: FunName  ->
               T_FunName 
sem_FunName (ArrayCtor )  =
    (sem_FunName_ArrayCtor )
sem_FunName (ArraySub )  =
    (sem_FunName_ArraySub )
sem_FunName (Between )  =
    (sem_FunName_Between )
sem_FunName (KOperator _keywordOperator )  =
    (sem_FunName_KOperator (sem_KeywordOperator _keywordOperator ) )
sem_FunName (Operator _string )  =
    (sem_FunName_Operator _string )
sem_FunName (RowCtor )  =
    (sem_FunName_RowCtor )
sem_FunName (SimpleFun _string )  =
    (sem_FunName_SimpleFun _string )
sem_FunName (Substring )  =
    (sem_FunName_Substring )
-- semantic domain
type T_FunName  = Bool ->
                  MySourcePos ->
                  ( ([Message]),Type,FunName)
data Inh_FunName  = Inh_FunName {inLoop_Inh_FunName :: Bool,sourcePos_Inh_FunName :: MySourcePos}
data Syn_FunName  = Syn_FunName {messages_Syn_FunName :: [Message],nodeType_Syn_FunName :: Type,val_Syn_FunName :: FunName}
wrap_FunName :: T_FunName  ->
                Inh_FunName  ->
                Syn_FunName 
wrap_FunName sem (Inh_FunName _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType,_lhsOval) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_FunName _lhsOmessages _lhsOnodeType _lhsOval ))
sem_FunName_ArrayCtor :: T_FunName 
sem_FunName_ArrayCtor  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOval :: FunName
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _val =
                  ArrayCtor
              _lhsOval =
                  _val
          in  ( _lhsOmessages,_lhsOnodeType,_lhsOval)))
sem_FunName_ArraySub :: T_FunName 
sem_FunName_ArraySub  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOval :: FunName
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _val =
                  ArraySub
              _lhsOval =
                  _val
          in  ( _lhsOmessages,_lhsOnodeType,_lhsOval)))
sem_FunName_Between :: T_FunName 
sem_FunName_Between  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOval :: FunName
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _val =
                  Between
              _lhsOval =
                  _val
          in  ( _lhsOmessages,_lhsOnodeType,_lhsOval)))
sem_FunName_KOperator :: T_KeywordOperator  ->
                         T_FunName 
sem_FunName_KOperator keywordOperator_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOval :: FunName
              _keywordOperatorOinLoop :: Bool
              _keywordOperatorOsourcePos :: MySourcePos
              _keywordOperatorImessages :: ([Message])
              _keywordOperatorIval :: KeywordOperator
              _lhsOmessages =
                  _keywordOperatorImessages
              _lhsOnodeType =
                  UnknownType
              _val =
                  KOperator _keywordOperatorIval
              _lhsOval =
                  _val
              _keywordOperatorOinLoop =
                  _lhsIinLoop
              _keywordOperatorOsourcePos =
                  _lhsIsourcePos
              ( _keywordOperatorImessages,_keywordOperatorIval) =
                  (keywordOperator_ _keywordOperatorOinLoop _keywordOperatorOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType,_lhsOval)))
sem_FunName_Operator :: String ->
                        T_FunName 
sem_FunName_Operator string_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOval :: FunName
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _val =
                  Operator string_
              _lhsOval =
                  _val
          in  ( _lhsOmessages,_lhsOnodeType,_lhsOval)))
sem_FunName_RowCtor :: T_FunName 
sem_FunName_RowCtor  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOval :: FunName
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _val =
                  RowCtor
              _lhsOval =
                  _val
          in  ( _lhsOmessages,_lhsOnodeType,_lhsOval)))
sem_FunName_SimpleFun :: String ->
                         T_FunName 
sem_FunName_SimpleFun string_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOval :: FunName
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _val =
                  SimpleFun string_
              _lhsOval =
                  _val
          in  ( _lhsOmessages,_lhsOnodeType,_lhsOval)))
sem_FunName_Substring :: T_FunName 
sem_FunName_Substring  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOval :: FunName
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
              _val =
                  Substring
              _lhsOval =
                  _val
          in  ( _lhsOmessages,_lhsOnodeType,_lhsOval)))
-- IfExists ----------------------------------------------------
data IfExists  = IfExists 
               | Require 
               deriving ( Eq,Show)
-- cata
sem_IfExists :: IfExists  ->
                T_IfExists 
sem_IfExists (IfExists )  =
    (sem_IfExists_IfExists )
sem_IfExists (Require )  =
    (sem_IfExists_Require )
-- semantic domain
type T_IfExists  = Bool ->
                   MySourcePos ->
                   ( ([Message]),Type)
data Inh_IfExists  = Inh_IfExists {inLoop_Inh_IfExists :: Bool,sourcePos_Inh_IfExists :: MySourcePos}
data Syn_IfExists  = Syn_IfExists {messages_Syn_IfExists :: [Message],nodeType_Syn_IfExists :: Type}
wrap_IfExists :: T_IfExists  ->
                 Inh_IfExists  ->
                 Syn_IfExists 
wrap_IfExists sem (Inh_IfExists _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_IfExists _lhsOmessages _lhsOnodeType ))
sem_IfExists_IfExists :: T_IfExists 
sem_IfExists_IfExists  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_IfExists_Require :: T_IfExists 
sem_IfExists_Require  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
-- InList ------------------------------------------------------
data InList  = InList (ExpressionList) 
             | InSelect (SelectExpression) 
             deriving ( Eq,Show)
-- cata
sem_InList :: InList  ->
              T_InList 
sem_InList (InList _expressionList )  =
    (sem_InList_InList (sem_ExpressionList _expressionList ) )
sem_InList (InSelect _sel )  =
    (sem_InList_InSelect (sem_SelectExpression _sel ) )
-- semantic domain
type T_InList  = Bool ->
                 MySourcePos ->
                 ( ([Message]),Type)
data Inh_InList  = Inh_InList {inLoop_Inh_InList :: Bool,sourcePos_Inh_InList :: MySourcePos}
data Syn_InList  = Syn_InList {messages_Syn_InList :: [Message],nodeType_Syn_InList :: Type}
wrap_InList :: T_InList  ->
               Inh_InList  ->
               Syn_InList 
wrap_InList sem (Inh_InList _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_InList _lhsOmessages _lhsOnodeType ))
sem_InList_InList :: T_ExpressionList  ->
                     T_InList 
sem_InList_InList expressionList_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _expressionListOinLoop :: Bool
              _expressionListOsourcePos :: MySourcePos
              _expressionListImessages :: ([Message])
              _expressionListInodeType :: Type
              _lhsOmessages =
                  _expressionListImessages
              _lhsOnodeType =
                  _expressionListInodeType
              _expressionListOinLoop =
                  _lhsIinLoop
              _expressionListOsourcePos =
                  _lhsIsourcePos
              ( _expressionListImessages,_expressionListInodeType) =
                  (expressionList_ _expressionListOinLoop _expressionListOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_InList_InSelect :: T_SelectExpression  ->
                       T_InList 
sem_InList_InSelect sel_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _selOinLoop :: Bool
              _selOsourcePos :: MySourcePos
              _selImessages :: ([Message])
              _selInodeType :: Type
              _lhsOmessages =
                  _selImessages
              _lhsOnodeType =
                  _selInodeType
              _selOinLoop =
                  _lhsIinLoop
              _selOsourcePos =
                  _lhsIsourcePos
              ( _selImessages,_selInodeType) =
                  (sel_ _selOinLoop _selOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
-- JoinExpression ----------------------------------------------
data JoinExpression  = JoinOn (Expression) 
                     | JoinUsing (StringList) 
                     deriving ( Eq,Show)
-- cata
sem_JoinExpression :: JoinExpression  ->
                      T_JoinExpression 
sem_JoinExpression (JoinOn _expression )  =
    (sem_JoinExpression_JoinOn (sem_Expression _expression ) )
sem_JoinExpression (JoinUsing _stringList )  =
    (sem_JoinExpression_JoinUsing (sem_StringList _stringList ) )
-- semantic domain
type T_JoinExpression  = Bool ->
                         MySourcePos ->
                         ( ([Message]),Type)
data Inh_JoinExpression  = Inh_JoinExpression {inLoop_Inh_JoinExpression :: Bool,sourcePos_Inh_JoinExpression :: MySourcePos}
data Syn_JoinExpression  = Syn_JoinExpression {messages_Syn_JoinExpression :: [Message],nodeType_Syn_JoinExpression :: Type}
wrap_JoinExpression :: T_JoinExpression  ->
                       Inh_JoinExpression  ->
                       Syn_JoinExpression 
wrap_JoinExpression sem (Inh_JoinExpression _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_JoinExpression _lhsOmessages _lhsOnodeType ))
sem_JoinExpression_JoinOn :: T_Expression  ->
                             T_JoinExpression 
sem_JoinExpression_JoinOn expression_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _expressionOinLoop :: Bool
              _expressionOsourcePos :: MySourcePos
              _expressionImessages :: ([Message])
              _expressionInodeType :: Type
              _lhsOmessages =
                  _expressionImessages
              _lhsOnodeType =
                  _expressionInodeType
              _expressionOinLoop =
                  _lhsIinLoop
              _expressionOsourcePos =
                  _lhsIsourcePos
              ( _expressionImessages,_expressionInodeType) =
                  (expression_ _expressionOinLoop _expressionOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_JoinExpression_JoinUsing :: T_StringList  ->
                                T_JoinExpression 
sem_JoinExpression_JoinUsing stringList_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _stringListOinLoop :: Bool
              _stringListOsourcePos :: MySourcePos
              _stringListImessages :: ([Message])
              _stringListInodeType :: Type
              _lhsOmessages =
                  _stringListImessages
              _lhsOnodeType =
                  _stringListInodeType
              _stringListOinLoop =
                  _lhsIinLoop
              _stringListOsourcePos =
                  _lhsIsourcePos
              ( _stringListImessages,_stringListInodeType) =
                  (stringList_ _stringListOinLoop _stringListOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
-- JoinType ----------------------------------------------------
data JoinType  = Cross 
               | FullOuter 
               | Inner 
               | LeftOuter 
               | RightOuter 
               deriving ( Eq,Show)
-- cata
sem_JoinType :: JoinType  ->
                T_JoinType 
sem_JoinType (Cross )  =
    (sem_JoinType_Cross )
sem_JoinType (FullOuter )  =
    (sem_JoinType_FullOuter )
sem_JoinType (Inner )  =
    (sem_JoinType_Inner )
sem_JoinType (LeftOuter )  =
    (sem_JoinType_LeftOuter )
sem_JoinType (RightOuter )  =
    (sem_JoinType_RightOuter )
-- semantic domain
type T_JoinType  = Bool ->
                   MySourcePos ->
                   ( ([Message]),Type)
data Inh_JoinType  = Inh_JoinType {inLoop_Inh_JoinType :: Bool,sourcePos_Inh_JoinType :: MySourcePos}
data Syn_JoinType  = Syn_JoinType {messages_Syn_JoinType :: [Message],nodeType_Syn_JoinType :: Type}
wrap_JoinType :: T_JoinType  ->
                 Inh_JoinType  ->
                 Syn_JoinType 
wrap_JoinType sem (Inh_JoinType _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_JoinType _lhsOmessages _lhsOnodeType ))
sem_JoinType_Cross :: T_JoinType 
sem_JoinType_Cross  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_JoinType_FullOuter :: T_JoinType 
sem_JoinType_FullOuter  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_JoinType_Inner :: T_JoinType 
sem_JoinType_Inner  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_JoinType_LeftOuter :: T_JoinType 
sem_JoinType_LeftOuter  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_JoinType_RightOuter :: T_JoinType 
sem_JoinType_RightOuter  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
-- KeywordOperator ---------------------------------------------
data KeywordOperator  = And 
                      | IsNotNull 
                      | IsNull 
                      | Like 
                      | Not 
                      | Or 
                      deriving ( Eq,Show)
-- cata
sem_KeywordOperator :: KeywordOperator  ->
                       T_KeywordOperator 
sem_KeywordOperator (And )  =
    (sem_KeywordOperator_And )
sem_KeywordOperator (IsNotNull )  =
    (sem_KeywordOperator_IsNotNull )
sem_KeywordOperator (IsNull )  =
    (sem_KeywordOperator_IsNull )
sem_KeywordOperator (Like )  =
    (sem_KeywordOperator_Like )
sem_KeywordOperator (Not )  =
    (sem_KeywordOperator_Not )
sem_KeywordOperator (Or )  =
    (sem_KeywordOperator_Or )
-- semantic domain
type T_KeywordOperator  = Bool ->
                          MySourcePos ->
                          ( ([Message]),KeywordOperator)
data Inh_KeywordOperator  = Inh_KeywordOperator {inLoop_Inh_KeywordOperator :: Bool,sourcePos_Inh_KeywordOperator :: MySourcePos}
data Syn_KeywordOperator  = Syn_KeywordOperator {messages_Syn_KeywordOperator :: [Message],val_Syn_KeywordOperator :: KeywordOperator}
wrap_KeywordOperator :: T_KeywordOperator  ->
                        Inh_KeywordOperator  ->
                        Syn_KeywordOperator 
wrap_KeywordOperator sem (Inh_KeywordOperator _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOval) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_KeywordOperator _lhsOmessages _lhsOval ))
sem_KeywordOperator_And :: T_KeywordOperator 
sem_KeywordOperator_And  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOval :: KeywordOperator
              _lhsOmessages =
                  []
              _val =
                  And
              _lhsOval =
                  _val
          in  ( _lhsOmessages,_lhsOval)))
sem_KeywordOperator_IsNotNull :: T_KeywordOperator 
sem_KeywordOperator_IsNotNull  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOval :: KeywordOperator
              _lhsOmessages =
                  []
              _val =
                  IsNotNull
              _lhsOval =
                  _val
          in  ( _lhsOmessages,_lhsOval)))
sem_KeywordOperator_IsNull :: T_KeywordOperator 
sem_KeywordOperator_IsNull  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOval :: KeywordOperator
              _lhsOmessages =
                  []
              _val =
                  IsNull
              _lhsOval =
                  _val
          in  ( _lhsOmessages,_lhsOval)))
sem_KeywordOperator_Like :: T_KeywordOperator 
sem_KeywordOperator_Like  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOval :: KeywordOperator
              _lhsOmessages =
                  []
              _val =
                  Like
              _lhsOval =
                  _val
          in  ( _lhsOmessages,_lhsOval)))
sem_KeywordOperator_Not :: T_KeywordOperator 
sem_KeywordOperator_Not  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOval :: KeywordOperator
              _lhsOmessages =
                  []
              _val =
                  Not
              _lhsOval =
                  _val
          in  ( _lhsOmessages,_lhsOval)))
sem_KeywordOperator_Or :: T_KeywordOperator 
sem_KeywordOperator_Or  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOval :: KeywordOperator
              _lhsOmessages =
                  []
              _val =
                  Or
              _lhsOval =
                  _val
          in  ( _lhsOmessages,_lhsOval)))
-- Language ----------------------------------------------------
data Language  = Plpgsql 
               | Sql 
               deriving ( Eq,Show)
-- cata
sem_Language :: Language  ->
                T_Language 
sem_Language (Plpgsql )  =
    (sem_Language_Plpgsql )
sem_Language (Sql )  =
    (sem_Language_Sql )
-- semantic domain
type T_Language  = Bool ->
                   MySourcePos ->
                   ( ([Message]),Type)
data Inh_Language  = Inh_Language {inLoop_Inh_Language :: Bool,sourcePos_Inh_Language :: MySourcePos}
data Syn_Language  = Syn_Language {messages_Syn_Language :: [Message],nodeType_Syn_Language :: Type}
wrap_Language :: T_Language  ->
                 Inh_Language  ->
                 Syn_Language 
wrap_Language sem (Inh_Language _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_Language _lhsOmessages _lhsOnodeType ))
sem_Language_Plpgsql :: T_Language 
sem_Language_Plpgsql  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Language_Sql :: T_Language 
sem_Language_Sql  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
-- MaybeExpression ---------------------------------------------
type MaybeExpression  = (Maybe (Expression))
-- cata
sem_MaybeExpression :: MaybeExpression  ->
                       T_MaybeExpression 
sem_MaybeExpression (Prelude.Just x )  =
    (sem_MaybeExpression_Just (sem_Expression x ) )
sem_MaybeExpression Prelude.Nothing  =
    sem_MaybeExpression_Nothing
-- semantic domain
type T_MaybeExpression  = Bool ->
                          MySourcePos ->
                          ( ([Message]),Type)
data Inh_MaybeExpression  = Inh_MaybeExpression {inLoop_Inh_MaybeExpression :: Bool,sourcePos_Inh_MaybeExpression :: MySourcePos}
data Syn_MaybeExpression  = Syn_MaybeExpression {messages_Syn_MaybeExpression :: [Message],nodeType_Syn_MaybeExpression :: Type}
wrap_MaybeExpression :: T_MaybeExpression  ->
                        Inh_MaybeExpression  ->
                        Syn_MaybeExpression 
wrap_MaybeExpression sem (Inh_MaybeExpression _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_MaybeExpression _lhsOmessages _lhsOnodeType ))
sem_MaybeExpression_Just :: T_Expression  ->
                            T_MaybeExpression 
sem_MaybeExpression_Just just_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOmessages :: ([Message])
              _justOinLoop :: Bool
              _justOsourcePos :: MySourcePos
              _justImessages :: ([Message])
              _justInodeType :: Type
              _lhsOnodeType =
                  _justInodeType
              _lhsOmessages =
                  _justImessages
              _justOinLoop =
                  _lhsIinLoop
              _justOsourcePos =
                  _lhsIsourcePos
              ( _justImessages,_justInodeType) =
                  (just_ _justOinLoop _justOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_MaybeExpression_Nothing :: T_MaybeExpression 
sem_MaybeExpression_Nothing  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOmessages :: ([Message])
              _lhsOnodeType =
                  Pseudo AnyElement
              _lhsOmessages =
                  []
          in  ( _lhsOmessages,_lhsOnodeType)))
-- Message -----------------------------------------------------
data Message  = Error (MySourcePos) (MessageStuff) 
              | Notice (MySourcePos) (MessageStuff) 
              | Warning (MySourcePos) (MessageStuff) 
              deriving ( Eq)
-- cata
sem_Message :: Message  ->
               T_Message 
sem_Message (Error _mySourcePos _messageStuff )  =
    (sem_Message_Error (sem_MySourcePos _mySourcePos ) (sem_MessageStuff _messageStuff ) )
sem_Message (Notice _mySourcePos _messageStuff )  =
    (sem_Message_Notice (sem_MySourcePos _mySourcePos ) (sem_MessageStuff _messageStuff ) )
sem_Message (Warning _mySourcePos _messageStuff )  =
    (sem_Message_Warning (sem_MySourcePos _mySourcePos ) (sem_MessageStuff _messageStuff ) )
-- semantic domain
type T_Message  = ( )
data Inh_Message  = Inh_Message {}
data Syn_Message  = Syn_Message {}
wrap_Message :: T_Message  ->
                Inh_Message  ->
                Syn_Message 
wrap_Message sem (Inh_Message )  =
    (let ( ) =
             (sem )
     in  (Syn_Message ))
sem_Message_Error :: T_MySourcePos  ->
                     T_MessageStuff  ->
                     T_Message 
sem_Message_Error mySourcePos_ messageStuff_  =
    (let _mySourcePosIval :: MySourcePos
         ( _mySourcePosIval) =
             (mySourcePos_ )
     in  ( ))
sem_Message_Notice :: T_MySourcePos  ->
                      T_MessageStuff  ->
                      T_Message 
sem_Message_Notice mySourcePos_ messageStuff_  =
    (let _mySourcePosIval :: MySourcePos
         ( _mySourcePosIval) =
             (mySourcePos_ )
     in  ( ))
sem_Message_Warning :: T_MySourcePos  ->
                       T_MessageStuff  ->
                       T_Message 
sem_Message_Warning mySourcePos_ messageStuff_  =
    (let _mySourcePosIval :: MySourcePos
         ( _mySourcePosIval) =
             (mySourcePos_ )
     in  ( ))
-- MessageStuff ------------------------------------------------
data MessageStuff  = ContinueNotInLoop 
                   | CustomMessage (String) 
                   deriving ( Eq,Show)
-- cata
sem_MessageStuff :: MessageStuff  ->
                    T_MessageStuff 
sem_MessageStuff (ContinueNotInLoop )  =
    (sem_MessageStuff_ContinueNotInLoop )
sem_MessageStuff (CustomMessage _string )  =
    (sem_MessageStuff_CustomMessage _string )
-- semantic domain
type T_MessageStuff  = ( )
data Inh_MessageStuff  = Inh_MessageStuff {}
data Syn_MessageStuff  = Syn_MessageStuff {}
wrap_MessageStuff :: T_MessageStuff  ->
                     Inh_MessageStuff  ->
                     Syn_MessageStuff 
wrap_MessageStuff sem (Inh_MessageStuff )  =
    (let ( ) =
             (sem )
     in  (Syn_MessageStuff ))
sem_MessageStuff_ContinueNotInLoop :: T_MessageStuff 
sem_MessageStuff_ContinueNotInLoop  =
    (let 
     in  ( ))
sem_MessageStuff_CustomMessage :: String ->
                                  T_MessageStuff 
sem_MessageStuff_CustomMessage string_  =
    (let 
     in  ( ))
-- MySourcePos -------------------------------------------------
type MySourcePos  = ( (String),(Int),(Int))
-- cata
sem_MySourcePos :: MySourcePos  ->
                   T_MySourcePos 
sem_MySourcePos ( x1,x2,x3)  =
    (sem_MySourcePos_Tuple x1 x2 x3 )
-- semantic domain
type T_MySourcePos  = ( MySourcePos)
data Inh_MySourcePos  = Inh_MySourcePos {}
data Syn_MySourcePos  = Syn_MySourcePos {val_Syn_MySourcePos :: MySourcePos}
wrap_MySourcePos :: T_MySourcePos  ->
                    Inh_MySourcePos  ->
                    Syn_MySourcePos 
wrap_MySourcePos sem (Inh_MySourcePos )  =
    (let ( _lhsOval) =
             (sem )
     in  (Syn_MySourcePos _lhsOval ))
sem_MySourcePos_Tuple :: String ->
                         Int ->
                         Int ->
                         T_MySourcePos 
sem_MySourcePos_Tuple x1_ x2_ x3_  =
    (let _lhsOval :: MySourcePos
         _val =
             (x1_,x2_,x3_)
         _lhsOval =
             _val
     in  ( _lhsOval))
-- Natural -----------------------------------------------------
data Natural  = Natural 
              | Unnatural 
              deriving ( Eq,Show)
-- cata
sem_Natural :: Natural  ->
               T_Natural 
sem_Natural (Natural )  =
    (sem_Natural_Natural )
sem_Natural (Unnatural )  =
    (sem_Natural_Unnatural )
-- semantic domain
type T_Natural  = Bool ->
                  MySourcePos ->
                  ( ([Message]),Type)
data Inh_Natural  = Inh_Natural {inLoop_Inh_Natural :: Bool,sourcePos_Inh_Natural :: MySourcePos}
data Syn_Natural  = Syn_Natural {messages_Syn_Natural :: [Message],nodeType_Syn_Natural :: Type}
wrap_Natural :: T_Natural  ->
                Inh_Natural  ->
                Syn_Natural 
wrap_Natural sem (Inh_Natural _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_Natural _lhsOmessages _lhsOnodeType ))
sem_Natural_Natural :: T_Natural 
sem_Natural_Natural  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Natural_Unnatural :: T_Natural 
sem_Natural_Unnatural  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
-- OperatorType ------------------------------------------------
data OperatorType  = BinaryOp 
                   | PostfixOp 
                   | PrefixOp 
-- cata
sem_OperatorType :: OperatorType  ->
                    T_OperatorType 
sem_OperatorType (BinaryOp )  =
    (sem_OperatorType_BinaryOp )
sem_OperatorType (PostfixOp )  =
    (sem_OperatorType_PostfixOp )
sem_OperatorType (PrefixOp )  =
    (sem_OperatorType_PrefixOp )
-- semantic domain
type T_OperatorType  = ( )
data Inh_OperatorType  = Inh_OperatorType {}
data Syn_OperatorType  = Syn_OperatorType {}
wrap_OperatorType :: T_OperatorType  ->
                     Inh_OperatorType  ->
                     Syn_OperatorType 
wrap_OperatorType sem (Inh_OperatorType )  =
    (let ( ) =
             (sem )
     in  (Syn_OperatorType ))
sem_OperatorType_BinaryOp :: T_OperatorType 
sem_OperatorType_BinaryOp  =
    (let 
     in  ( ))
sem_OperatorType_PostfixOp :: T_OperatorType 
sem_OperatorType_PostfixOp  =
    (let 
     in  ( ))
sem_OperatorType_PrefixOp :: T_OperatorType 
sem_OperatorType_PrefixOp  =
    (let 
     in  ( ))
-- ParamDef ----------------------------------------------------
data ParamDef  = ParamDef (String) (TypeName) 
               | ParamDefTp (TypeName) 
               deriving ( Eq,Show)
-- cata
sem_ParamDef :: ParamDef  ->
                T_ParamDef 
sem_ParamDef (ParamDef _string _typeName )  =
    (sem_ParamDef_ParamDef _string (sem_TypeName _typeName ) )
sem_ParamDef (ParamDefTp _typeName )  =
    (sem_ParamDef_ParamDefTp (sem_TypeName _typeName ) )
-- semantic domain
type T_ParamDef  = Bool ->
                   MySourcePos ->
                   ( ([Message]),Type)
data Inh_ParamDef  = Inh_ParamDef {inLoop_Inh_ParamDef :: Bool,sourcePos_Inh_ParamDef :: MySourcePos}
data Syn_ParamDef  = Syn_ParamDef {messages_Syn_ParamDef :: [Message],nodeType_Syn_ParamDef :: Type}
wrap_ParamDef :: T_ParamDef  ->
                 Inh_ParamDef  ->
                 Syn_ParamDef 
wrap_ParamDef sem (Inh_ParamDef _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_ParamDef _lhsOmessages _lhsOnodeType ))
sem_ParamDef_ParamDef :: String ->
                         T_TypeName  ->
                         T_ParamDef 
sem_ParamDef_ParamDef string_ typeName_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _typeNameOinLoop :: Bool
              _typeNameOsourcePos :: MySourcePos
              _typeNameImessages :: ([Message])
              _typeNameInodeType :: Type
              _lhsOmessages =
                  _typeNameImessages
              _lhsOnodeType =
                  _typeNameInodeType
              _typeNameOinLoop =
                  _lhsIinLoop
              _typeNameOsourcePos =
                  _lhsIsourcePos
              ( _typeNameImessages,_typeNameInodeType) =
                  (typeName_ _typeNameOinLoop _typeNameOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_ParamDef_ParamDefTp :: T_TypeName  ->
                           T_ParamDef 
sem_ParamDef_ParamDefTp typeName_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _typeNameOinLoop :: Bool
              _typeNameOsourcePos :: MySourcePos
              _typeNameImessages :: ([Message])
              _typeNameInodeType :: Type
              _lhsOmessages =
                  _typeNameImessages
              _lhsOnodeType =
                  _typeNameInodeType
              _typeNameOinLoop =
                  _lhsIinLoop
              _typeNameOsourcePos =
                  _lhsIsourcePos
              ( _typeNameImessages,_typeNameInodeType) =
                  (typeName_ _typeNameOinLoop _typeNameOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
-- ParamDefList ------------------------------------------------
type ParamDefList  = [(ParamDef)]
-- cata
sem_ParamDefList :: ParamDefList  ->
                    T_ParamDefList 
sem_ParamDefList list  =
    (Prelude.foldr sem_ParamDefList_Cons sem_ParamDefList_Nil (Prelude.map sem_ParamDef list) )
-- semantic domain
type T_ParamDefList  = Bool ->
                       MySourcePos ->
                       ( ([Message]),Type)
data Inh_ParamDefList  = Inh_ParamDefList {inLoop_Inh_ParamDefList :: Bool,sourcePos_Inh_ParamDefList :: MySourcePos}
data Syn_ParamDefList  = Syn_ParamDefList {messages_Syn_ParamDefList :: [Message],nodeType_Syn_ParamDefList :: Type}
wrap_ParamDefList :: T_ParamDefList  ->
                     Inh_ParamDefList  ->
                     Syn_ParamDefList 
wrap_ParamDefList sem (Inh_ParamDefList _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_ParamDefList _lhsOmessages _lhsOnodeType ))
sem_ParamDefList_Cons :: T_ParamDef  ->
                         T_ParamDefList  ->
                         T_ParamDefList 
sem_ParamDefList_Cons hd_ tl_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _hdOinLoop :: Bool
              _hdOsourcePos :: MySourcePos
              _tlOinLoop :: Bool
              _tlOsourcePos :: MySourcePos
              _hdImessages :: ([Message])
              _hdInodeType :: Type
              _tlImessages :: ([Message])
              _tlInodeType :: Type
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _lhsOnodeType =
                  _hdInodeType `appendTypeList` _tlInodeType
              _hdOinLoop =
                  _lhsIinLoop
              _hdOsourcePos =
                  _lhsIsourcePos
              _tlOinLoop =
                  _lhsIinLoop
              _tlOsourcePos =
                  _lhsIsourcePos
              ( _hdImessages,_hdInodeType) =
                  (hd_ _hdOinLoop _hdOsourcePos )
              ( _tlImessages,_tlInodeType) =
                  (tl_ _tlOinLoop _tlOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_ParamDefList_Nil :: T_ParamDefList 
sem_ParamDefList_Nil  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  TypeList []
          in  ( _lhsOmessages,_lhsOnodeType)))
-- PseudoType --------------------------------------------------
data PseudoType  = Any 
                 | AnyArray 
                 | AnyElement 
                 | AnyEnum 
                 | AnyNonArray 
                 | Cstring 
                 | Internal 
                 | LanguageHandler 
                 | Opaque 
                 | Record 
                 | Trigger 
                 | UnknownStringLit 
                 | Void 
                 deriving ( Eq,Show)
-- cata
sem_PseudoType :: PseudoType  ->
                  T_PseudoType 
sem_PseudoType (Any )  =
    (sem_PseudoType_Any )
sem_PseudoType (AnyArray )  =
    (sem_PseudoType_AnyArray )
sem_PseudoType (AnyElement )  =
    (sem_PseudoType_AnyElement )
sem_PseudoType (AnyEnum )  =
    (sem_PseudoType_AnyEnum )
sem_PseudoType (AnyNonArray )  =
    (sem_PseudoType_AnyNonArray )
sem_PseudoType (Cstring )  =
    (sem_PseudoType_Cstring )
sem_PseudoType (Internal )  =
    (sem_PseudoType_Internal )
sem_PseudoType (LanguageHandler )  =
    (sem_PseudoType_LanguageHandler )
sem_PseudoType (Opaque )  =
    (sem_PseudoType_Opaque )
sem_PseudoType (Record )  =
    (sem_PseudoType_Record )
sem_PseudoType (Trigger )  =
    (sem_PseudoType_Trigger )
sem_PseudoType (UnknownStringLit )  =
    (sem_PseudoType_UnknownStringLit )
sem_PseudoType (Void )  =
    (sem_PseudoType_Void )
-- semantic domain
type T_PseudoType  = ( )
data Inh_PseudoType  = Inh_PseudoType {}
data Syn_PseudoType  = Syn_PseudoType {}
wrap_PseudoType :: T_PseudoType  ->
                   Inh_PseudoType  ->
                   Syn_PseudoType 
wrap_PseudoType sem (Inh_PseudoType )  =
    (let ( ) =
             (sem )
     in  (Syn_PseudoType ))
sem_PseudoType_Any :: T_PseudoType 
sem_PseudoType_Any  =
    (let 
     in  ( ))
sem_PseudoType_AnyArray :: T_PseudoType 
sem_PseudoType_AnyArray  =
    (let 
     in  ( ))
sem_PseudoType_AnyElement :: T_PseudoType 
sem_PseudoType_AnyElement  =
    (let 
     in  ( ))
sem_PseudoType_AnyEnum :: T_PseudoType 
sem_PseudoType_AnyEnum  =
    (let 
     in  ( ))
sem_PseudoType_AnyNonArray :: T_PseudoType 
sem_PseudoType_AnyNonArray  =
    (let 
     in  ( ))
sem_PseudoType_Cstring :: T_PseudoType 
sem_PseudoType_Cstring  =
    (let 
     in  ( ))
sem_PseudoType_Internal :: T_PseudoType 
sem_PseudoType_Internal  =
    (let 
     in  ( ))
sem_PseudoType_LanguageHandler :: T_PseudoType 
sem_PseudoType_LanguageHandler  =
    (let 
     in  ( ))
sem_PseudoType_Opaque :: T_PseudoType 
sem_PseudoType_Opaque  =
    (let 
     in  ( ))
sem_PseudoType_Record :: T_PseudoType 
sem_PseudoType_Record  =
    (let 
     in  ( ))
sem_PseudoType_Trigger :: T_PseudoType 
sem_PseudoType_Trigger  =
    (let 
     in  ( ))
sem_PseudoType_UnknownStringLit :: T_PseudoType 
sem_PseudoType_UnknownStringLit  =
    (let 
     in  ( ))
sem_PseudoType_Void :: T_PseudoType 
sem_PseudoType_Void  =
    (let 
     in  ( ))
-- RaiseType ---------------------------------------------------
data RaiseType  = RError 
                | RException 
                | RNotice 
                deriving ( Eq,Show)
-- cata
sem_RaiseType :: RaiseType  ->
                 T_RaiseType 
sem_RaiseType (RError )  =
    (sem_RaiseType_RError )
sem_RaiseType (RException )  =
    (sem_RaiseType_RException )
sem_RaiseType (RNotice )  =
    (sem_RaiseType_RNotice )
-- semantic domain
type T_RaiseType  = Bool ->
                    MySourcePos ->
                    ( ([Message]),Type)
data Inh_RaiseType  = Inh_RaiseType {inLoop_Inh_RaiseType :: Bool,sourcePos_Inh_RaiseType :: MySourcePos}
data Syn_RaiseType  = Syn_RaiseType {messages_Syn_RaiseType :: [Message],nodeType_Syn_RaiseType :: Type}
wrap_RaiseType :: T_RaiseType  ->
                  Inh_RaiseType  ->
                  Syn_RaiseType 
wrap_RaiseType sem (Inh_RaiseType _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_RaiseType _lhsOmessages _lhsOnodeType ))
sem_RaiseType_RError :: T_RaiseType 
sem_RaiseType_RError  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_RaiseType_RException :: T_RaiseType 
sem_RaiseType_RException  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_RaiseType_RNotice :: T_RaiseType 
sem_RaiseType_RNotice  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
-- RestartIdentity ---------------------------------------------
data RestartIdentity  = ContinueIdentity 
                      | RestartIdentity 
                      deriving ( Eq,Show)
-- cata
sem_RestartIdentity :: RestartIdentity  ->
                       T_RestartIdentity 
sem_RestartIdentity (ContinueIdentity )  =
    (sem_RestartIdentity_ContinueIdentity )
sem_RestartIdentity (RestartIdentity )  =
    (sem_RestartIdentity_RestartIdentity )
-- semantic domain
type T_RestartIdentity  = Bool ->
                          MySourcePos ->
                          ( ([Message]),Type)
data Inh_RestartIdentity  = Inh_RestartIdentity {inLoop_Inh_RestartIdentity :: Bool,sourcePos_Inh_RestartIdentity :: MySourcePos}
data Syn_RestartIdentity  = Syn_RestartIdentity {messages_Syn_RestartIdentity :: [Message],nodeType_Syn_RestartIdentity :: Type}
wrap_RestartIdentity :: T_RestartIdentity  ->
                        Inh_RestartIdentity  ->
                        Syn_RestartIdentity 
wrap_RestartIdentity sem (Inh_RestartIdentity _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_RestartIdentity _lhsOmessages _lhsOnodeType ))
sem_RestartIdentity_ContinueIdentity :: T_RestartIdentity 
sem_RestartIdentity_ContinueIdentity  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_RestartIdentity_RestartIdentity :: T_RestartIdentity 
sem_RestartIdentity_RestartIdentity  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
-- RetType -----------------------------------------------------
data RetType  = ConstRetType (Type) 
              | RetTypeAsArgN (Int) 
              | RetTypeFun (RetTypeFunner) 
-- cata
sem_RetType :: RetType  ->
               T_RetType 
sem_RetType (ConstRetType _type )  =
    (sem_RetType_ConstRetType (sem_Type _type ) )
sem_RetType (RetTypeAsArgN _int )  =
    (sem_RetType_RetTypeAsArgN _int )
sem_RetType (RetTypeFun _retTypeFunner )  =
    (sem_RetType_RetTypeFun _retTypeFunner )
-- semantic domain
type T_RetType  = ( )
data Inh_RetType  = Inh_RetType {}
data Syn_RetType  = Syn_RetType {}
wrap_RetType :: T_RetType  ->
                Inh_RetType  ->
                Syn_RetType 
wrap_RetType sem (Inh_RetType )  =
    (let ( ) =
             (sem )
     in  (Syn_RetType ))
sem_RetType_ConstRetType :: T_Type  ->
                            T_RetType 
sem_RetType_ConstRetType type_  =
    (let 
     in  ( ))
sem_RetType_RetTypeAsArgN :: Int ->
                             T_RetType 
sem_RetType_RetTypeAsArgN int_  =
    (let 
     in  ( ))
sem_RetType_RetTypeFun :: RetTypeFunner ->
                          T_RetType 
sem_RetType_RetTypeFun retTypeFunner_  =
    (let 
     in  ( ))
-- Root --------------------------------------------------------
data Root  = Root (StatementList) 
           deriving ( Show)
-- cata
sem_Root :: Root  ->
            T_Root 
sem_Root (Root _statements )  =
    (sem_Root_Root (sem_StatementList _statements ) )
-- semantic domain
type T_Root  = ( ([Message]),Type)
data Inh_Root  = Inh_Root {}
data Syn_Root  = Syn_Root {messages_Syn_Root :: [Message],nodeType_Syn_Root :: Type}
wrap_Root :: T_Root  ->
             Inh_Root  ->
             Syn_Root 
wrap_Root sem (Inh_Root )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem )
     in  (Syn_Root _lhsOmessages _lhsOnodeType ))
sem_Root_Root :: T_StatementList  ->
                 T_Root 
sem_Root_Root statements_  =
    (let _statementsOsourcePos :: MySourcePos
         _statementsOinLoop :: Bool
         _lhsOmessages :: ([Message])
         _lhsOnodeType :: Type
         _statementsImessages :: ([Message])
         _statementsInodeType :: Type
         _statementsOsourcePos =
             ("",0,0)
         _statementsOinLoop =
             False
         _lhsOmessages =
             _statementsImessages
         _lhsOnodeType =
             _statementsInodeType
         ( _statementsImessages,_statementsInodeType) =
             (statements_ _statementsOinLoop _statementsOsourcePos )
     in  ( _lhsOmessages,_lhsOnodeType))
-- RowConstraint -----------------------------------------------
data RowConstraint  = NotNullConstraint 
                    | NullConstraint 
                    | RowCheckConstraint (Expression) 
                    | RowPrimaryKeyConstraint 
                    | RowReferenceConstraint (String) (Maybe String) (Cascade) (Cascade) 
                    | RowUniqueConstraint 
                    deriving ( Eq,Show)
-- cata
sem_RowConstraint :: RowConstraint  ->
                     T_RowConstraint 
sem_RowConstraint (NotNullConstraint )  =
    (sem_RowConstraint_NotNullConstraint )
sem_RowConstraint (NullConstraint )  =
    (sem_RowConstraint_NullConstraint )
sem_RowConstraint (RowCheckConstraint _expression )  =
    (sem_RowConstraint_RowCheckConstraint (sem_Expression _expression ) )
sem_RowConstraint (RowPrimaryKeyConstraint )  =
    (sem_RowConstraint_RowPrimaryKeyConstraint )
sem_RowConstraint (RowReferenceConstraint _table _att _onUpdate _onDelete )  =
    (sem_RowConstraint_RowReferenceConstraint _table _att (sem_Cascade _onUpdate ) (sem_Cascade _onDelete ) )
sem_RowConstraint (RowUniqueConstraint )  =
    (sem_RowConstraint_RowUniqueConstraint )
-- semantic domain
type T_RowConstraint  = Bool ->
                        MySourcePos ->
                        ( ([Message]),Type)
data Inh_RowConstraint  = Inh_RowConstraint {inLoop_Inh_RowConstraint :: Bool,sourcePos_Inh_RowConstraint :: MySourcePos}
data Syn_RowConstraint  = Syn_RowConstraint {messages_Syn_RowConstraint :: [Message],nodeType_Syn_RowConstraint :: Type}
wrap_RowConstraint :: T_RowConstraint  ->
                      Inh_RowConstraint  ->
                      Syn_RowConstraint 
wrap_RowConstraint sem (Inh_RowConstraint _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_RowConstraint _lhsOmessages _lhsOnodeType ))
sem_RowConstraint_NotNullConstraint :: T_RowConstraint 
sem_RowConstraint_NotNullConstraint  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_RowConstraint_NullConstraint :: T_RowConstraint 
sem_RowConstraint_NullConstraint  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_RowConstraint_RowCheckConstraint :: T_Expression  ->
                                        T_RowConstraint 
sem_RowConstraint_RowCheckConstraint expression_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _expressionOinLoop :: Bool
              _expressionOsourcePos :: MySourcePos
              _expressionImessages :: ([Message])
              _expressionInodeType :: Type
              _lhsOmessages =
                  _expressionImessages
              _lhsOnodeType =
                  _expressionInodeType
              _expressionOinLoop =
                  _lhsIinLoop
              _expressionOsourcePos =
                  _lhsIsourcePos
              ( _expressionImessages,_expressionInodeType) =
                  (expression_ _expressionOinLoop _expressionOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_RowConstraint_RowPrimaryKeyConstraint :: T_RowConstraint 
sem_RowConstraint_RowPrimaryKeyConstraint  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_RowConstraint_RowReferenceConstraint :: String ->
                                            (Maybe String) ->
                                            T_Cascade  ->
                                            T_Cascade  ->
                                            T_RowConstraint 
sem_RowConstraint_RowReferenceConstraint table_ att_ onUpdate_ onDelete_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _onUpdateOinLoop :: Bool
              _onUpdateOsourcePos :: MySourcePos
              _onDeleteOinLoop :: Bool
              _onDeleteOsourcePos :: MySourcePos
              _onUpdateImessages :: ([Message])
              _onUpdateInodeType :: Type
              _onDeleteImessages :: ([Message])
              _onDeleteInodeType :: Type
              _lhsOmessages =
                  _onUpdateImessages ++ _onDeleteImessages
              _lhsOnodeType =
                  _onUpdateInodeType `setUnknown` _onDeleteInodeType
              _onUpdateOinLoop =
                  _lhsIinLoop
              _onUpdateOsourcePos =
                  _lhsIsourcePos
              _onDeleteOinLoop =
                  _lhsIinLoop
              _onDeleteOsourcePos =
                  _lhsIsourcePos
              ( _onUpdateImessages,_onUpdateInodeType) =
                  (onUpdate_ _onUpdateOinLoop _onUpdateOsourcePos )
              ( _onDeleteImessages,_onDeleteInodeType) =
                  (onDelete_ _onDeleteOinLoop _onDeleteOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_RowConstraint_RowUniqueConstraint :: T_RowConstraint 
sem_RowConstraint_RowUniqueConstraint  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
-- RowConstraintList -------------------------------------------
type RowConstraintList  = [(RowConstraint)]
-- cata
sem_RowConstraintList :: RowConstraintList  ->
                         T_RowConstraintList 
sem_RowConstraintList list  =
    (Prelude.foldr sem_RowConstraintList_Cons sem_RowConstraintList_Nil (Prelude.map sem_RowConstraint list) )
-- semantic domain
type T_RowConstraintList  = Bool ->
                            MySourcePos ->
                            ( ([Message]),Type)
data Inh_RowConstraintList  = Inh_RowConstraintList {inLoop_Inh_RowConstraintList :: Bool,sourcePos_Inh_RowConstraintList :: MySourcePos}
data Syn_RowConstraintList  = Syn_RowConstraintList {messages_Syn_RowConstraintList :: [Message],nodeType_Syn_RowConstraintList :: Type}
wrap_RowConstraintList :: T_RowConstraintList  ->
                          Inh_RowConstraintList  ->
                          Syn_RowConstraintList 
wrap_RowConstraintList sem (Inh_RowConstraintList _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_RowConstraintList _lhsOmessages _lhsOnodeType ))
sem_RowConstraintList_Cons :: T_RowConstraint  ->
                              T_RowConstraintList  ->
                              T_RowConstraintList 
sem_RowConstraintList_Cons hd_ tl_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _hdOinLoop :: Bool
              _hdOsourcePos :: MySourcePos
              _tlOinLoop :: Bool
              _tlOsourcePos :: MySourcePos
              _hdImessages :: ([Message])
              _hdInodeType :: Type
              _tlImessages :: ([Message])
              _tlInodeType :: Type
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _lhsOnodeType =
                  _hdInodeType `appendTypeList` _tlInodeType
              _hdOinLoop =
                  _lhsIinLoop
              _hdOsourcePos =
                  _lhsIsourcePos
              _tlOinLoop =
                  _lhsIinLoop
              _tlOsourcePos =
                  _lhsIsourcePos
              ( _hdImessages,_hdInodeType) =
                  (hd_ _hdOinLoop _hdOsourcePos )
              ( _tlImessages,_tlInodeType) =
                  (tl_ _tlOinLoop _tlOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_RowConstraintList_Nil :: T_RowConstraintList 
sem_RowConstraintList_Nil  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  TypeList []
          in  ( _lhsOmessages,_lhsOnodeType)))
-- SelectExpression --------------------------------------------
data SelectExpression  = CombineSelect (CombineType) (SelectExpression) (SelectExpression) 
                       | Select (Distinct) (SelectList) (Maybe TableRef) (Maybe Expression) (ExpressionList) (Maybe Expression) (ExpressionList) (Direction) (Maybe Expression) (Maybe Expression) 
                       | Values (ExpressionListList) 
                       deriving ( Eq,Show)
-- cata
sem_SelectExpression :: SelectExpression  ->
                        T_SelectExpression 
sem_SelectExpression (CombineSelect _ctype _sel1 _sel2 )  =
    (sem_SelectExpression_CombineSelect (sem_CombineType _ctype ) (sem_SelectExpression _sel1 ) (sem_SelectExpression _sel2 ) )
sem_SelectExpression (Select _selDistinct _selSelectList _selTref _selWhere _selGroupBy _selHaving _selOrderBy _selDir _selLimit _selOffset )  =
    (sem_SelectExpression_Select (sem_Distinct _selDistinct ) (sem_SelectList _selSelectList ) _selTref _selWhere (sem_ExpressionList _selGroupBy ) _selHaving (sem_ExpressionList _selOrderBy ) (sem_Direction _selDir ) _selLimit _selOffset )
sem_SelectExpression (Values _expressionListList )  =
    (sem_SelectExpression_Values (sem_ExpressionListList _expressionListList ) )
-- semantic domain
type T_SelectExpression  = Bool ->
                           MySourcePos ->
                           ( ([Message]),Type)
data Inh_SelectExpression  = Inh_SelectExpression {inLoop_Inh_SelectExpression :: Bool,sourcePos_Inh_SelectExpression :: MySourcePos}
data Syn_SelectExpression  = Syn_SelectExpression {messages_Syn_SelectExpression :: [Message],nodeType_Syn_SelectExpression :: Type}
wrap_SelectExpression :: T_SelectExpression  ->
                         Inh_SelectExpression  ->
                         Syn_SelectExpression 
wrap_SelectExpression sem (Inh_SelectExpression _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_SelectExpression _lhsOmessages _lhsOnodeType ))
sem_SelectExpression_CombineSelect :: T_CombineType  ->
                                      T_SelectExpression  ->
                                      T_SelectExpression  ->
                                      T_SelectExpression 
sem_SelectExpression_CombineSelect ctype_ sel1_ sel2_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _ctypeOinLoop :: Bool
              _ctypeOsourcePos :: MySourcePos
              _sel1OinLoop :: Bool
              _sel1OsourcePos :: MySourcePos
              _sel2OinLoop :: Bool
              _sel2OsourcePos :: MySourcePos
              _ctypeImessages :: ([Message])
              _ctypeInodeType :: Type
              _sel1Imessages :: ([Message])
              _sel1InodeType :: Type
              _sel2Imessages :: ([Message])
              _sel2InodeType :: Type
              _lhsOmessages =
                  _ctypeImessages ++ _sel1Imessages ++ _sel2Imessages
              _lhsOnodeType =
                  _ctypeInodeType `setUnknown` _sel1InodeType `setUnknown` _sel2InodeType
              _ctypeOinLoop =
                  _lhsIinLoop
              _ctypeOsourcePos =
                  _lhsIsourcePos
              _sel1OinLoop =
                  _lhsIinLoop
              _sel1OsourcePos =
                  _lhsIsourcePos
              _sel2OinLoop =
                  _lhsIinLoop
              _sel2OsourcePos =
                  _lhsIsourcePos
              ( _ctypeImessages,_ctypeInodeType) =
                  (ctype_ _ctypeOinLoop _ctypeOsourcePos )
              ( _sel1Imessages,_sel1InodeType) =
                  (sel1_ _sel1OinLoop _sel1OsourcePos )
              ( _sel2Imessages,_sel2InodeType) =
                  (sel2_ _sel2OinLoop _sel2OsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_SelectExpression_Select :: T_Distinct  ->
                               T_SelectList  ->
                               (Maybe TableRef) ->
                               (Maybe Expression) ->
                               T_ExpressionList  ->
                               (Maybe Expression) ->
                               T_ExpressionList  ->
                               T_Direction  ->
                               (Maybe Expression) ->
                               (Maybe Expression) ->
                               T_SelectExpression 
sem_SelectExpression_Select selDistinct_ selSelectList_ selTref_ selWhere_ selGroupBy_ selHaving_ selOrderBy_ selDir_ selLimit_ selOffset_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _selDistinctOinLoop :: Bool
              _selDistinctOsourcePos :: MySourcePos
              _selSelectListOinLoop :: Bool
              _selSelectListOsourcePos :: MySourcePos
              _selGroupByOinLoop :: Bool
              _selGroupByOsourcePos :: MySourcePos
              _selOrderByOinLoop :: Bool
              _selOrderByOsourcePos :: MySourcePos
              _selDirOinLoop :: Bool
              _selDirOsourcePos :: MySourcePos
              _selDistinctImessages :: ([Message])
              _selDistinctInodeType :: Type
              _selSelectListImessages :: ([Message])
              _selSelectListInodeType :: Type
              _selGroupByImessages :: ([Message])
              _selGroupByInodeType :: Type
              _selOrderByImessages :: ([Message])
              _selOrderByInodeType :: Type
              _selDirImessages :: ([Message])
              _selDirInodeType :: Type
              _lhsOmessages =
                  _selDistinctImessages ++ _selSelectListImessages ++ _selGroupByImessages ++ _selOrderByImessages ++ _selDirImessages
              _lhsOnodeType =
                  _selDistinctInodeType `setUnknown` _selSelectListInodeType `setUnknown` _selGroupByInodeType `setUnknown` _selOrderByInodeType `setUnknown` _selDirInodeType
              _selDistinctOinLoop =
                  _lhsIinLoop
              _selDistinctOsourcePos =
                  _lhsIsourcePos
              _selSelectListOinLoop =
                  _lhsIinLoop
              _selSelectListOsourcePos =
                  _lhsIsourcePos
              _selGroupByOinLoop =
                  _lhsIinLoop
              _selGroupByOsourcePos =
                  _lhsIsourcePos
              _selOrderByOinLoop =
                  _lhsIinLoop
              _selOrderByOsourcePos =
                  _lhsIsourcePos
              _selDirOinLoop =
                  _lhsIinLoop
              _selDirOsourcePos =
                  _lhsIsourcePos
              ( _selDistinctImessages,_selDistinctInodeType) =
                  (selDistinct_ _selDistinctOinLoop _selDistinctOsourcePos )
              ( _selSelectListImessages,_selSelectListInodeType) =
                  (selSelectList_ _selSelectListOinLoop _selSelectListOsourcePos )
              ( _selGroupByImessages,_selGroupByInodeType) =
                  (selGroupBy_ _selGroupByOinLoop _selGroupByOsourcePos )
              ( _selOrderByImessages,_selOrderByInodeType) =
                  (selOrderBy_ _selOrderByOinLoop _selOrderByOsourcePos )
              ( _selDirImessages,_selDirInodeType) =
                  (selDir_ _selDirOinLoop _selDirOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_SelectExpression_Values :: T_ExpressionListList  ->
                               T_SelectExpression 
sem_SelectExpression_Values expressionListList_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _expressionListListOinLoop :: Bool
              _expressionListListOsourcePos :: MySourcePos
              _expressionListListImessages :: ([Message])
              _expressionListListInodeType :: Type
              _lhsOmessages =
                  _expressionListListImessages
              _lhsOnodeType =
                  _expressionListListInodeType
              _expressionListListOinLoop =
                  _lhsIinLoop
              _expressionListListOsourcePos =
                  _lhsIsourcePos
              ( _expressionListListImessages,_expressionListListInodeType) =
                  (expressionListList_ _expressionListListOinLoop _expressionListListOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
-- SelectItem --------------------------------------------------
data SelectItem  = SelExp (Expression) 
                 | SelectItem (Expression) (String) 
                 deriving ( Eq,Show)
-- cata
sem_SelectItem :: SelectItem  ->
                  T_SelectItem 
sem_SelectItem (SelExp _expression )  =
    (sem_SelectItem_SelExp (sem_Expression _expression ) )
sem_SelectItem (SelectItem _expression _string )  =
    (sem_SelectItem_SelectItem (sem_Expression _expression ) _string )
-- semantic domain
type T_SelectItem  = Bool ->
                     MySourcePos ->
                     ( ([Message]),Type)
data Inh_SelectItem  = Inh_SelectItem {inLoop_Inh_SelectItem :: Bool,sourcePos_Inh_SelectItem :: MySourcePos}
data Syn_SelectItem  = Syn_SelectItem {messages_Syn_SelectItem :: [Message],nodeType_Syn_SelectItem :: Type}
wrap_SelectItem :: T_SelectItem  ->
                   Inh_SelectItem  ->
                   Syn_SelectItem 
wrap_SelectItem sem (Inh_SelectItem _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_SelectItem _lhsOmessages _lhsOnodeType ))
sem_SelectItem_SelExp :: T_Expression  ->
                         T_SelectItem 
sem_SelectItem_SelExp expression_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _expressionOinLoop :: Bool
              _expressionOsourcePos :: MySourcePos
              _expressionImessages :: ([Message])
              _expressionInodeType :: Type
              _lhsOmessages =
                  _expressionImessages
              _lhsOnodeType =
                  _expressionInodeType
              _expressionOinLoop =
                  _lhsIinLoop
              _expressionOsourcePos =
                  _lhsIsourcePos
              ( _expressionImessages,_expressionInodeType) =
                  (expression_ _expressionOinLoop _expressionOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_SelectItem_SelectItem :: T_Expression  ->
                             String ->
                             T_SelectItem 
sem_SelectItem_SelectItem expression_ string_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _expressionOinLoop :: Bool
              _expressionOsourcePos :: MySourcePos
              _expressionImessages :: ([Message])
              _expressionInodeType :: Type
              _lhsOmessages =
                  _expressionImessages
              _lhsOnodeType =
                  _expressionInodeType
              _expressionOinLoop =
                  _lhsIinLoop
              _expressionOsourcePos =
                  _lhsIsourcePos
              ( _expressionImessages,_expressionInodeType) =
                  (expression_ _expressionOinLoop _expressionOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
-- SelectItemList ----------------------------------------------
type SelectItemList  = [(SelectItem)]
-- cata
sem_SelectItemList :: SelectItemList  ->
                      T_SelectItemList 
sem_SelectItemList list  =
    (Prelude.foldr sem_SelectItemList_Cons sem_SelectItemList_Nil (Prelude.map sem_SelectItem list) )
-- semantic domain
type T_SelectItemList  = Bool ->
                         MySourcePos ->
                         ( ([Message]),Type)
data Inh_SelectItemList  = Inh_SelectItemList {inLoop_Inh_SelectItemList :: Bool,sourcePos_Inh_SelectItemList :: MySourcePos}
data Syn_SelectItemList  = Syn_SelectItemList {messages_Syn_SelectItemList :: [Message],nodeType_Syn_SelectItemList :: Type}
wrap_SelectItemList :: T_SelectItemList  ->
                       Inh_SelectItemList  ->
                       Syn_SelectItemList 
wrap_SelectItemList sem (Inh_SelectItemList _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_SelectItemList _lhsOmessages _lhsOnodeType ))
sem_SelectItemList_Cons :: T_SelectItem  ->
                           T_SelectItemList  ->
                           T_SelectItemList 
sem_SelectItemList_Cons hd_ tl_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _hdOinLoop :: Bool
              _hdOsourcePos :: MySourcePos
              _tlOinLoop :: Bool
              _tlOsourcePos :: MySourcePos
              _hdImessages :: ([Message])
              _hdInodeType :: Type
              _tlImessages :: ([Message])
              _tlInodeType :: Type
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _lhsOnodeType =
                  _hdInodeType `appendTypeList` _tlInodeType
              _hdOinLoop =
                  _lhsIinLoop
              _hdOsourcePos =
                  _lhsIsourcePos
              _tlOinLoop =
                  _lhsIinLoop
              _tlOsourcePos =
                  _lhsIsourcePos
              ( _hdImessages,_hdInodeType) =
                  (hd_ _hdOinLoop _hdOsourcePos )
              ( _tlImessages,_tlInodeType) =
                  (tl_ _tlOinLoop _tlOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_SelectItemList_Nil :: T_SelectItemList 
sem_SelectItemList_Nil  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  TypeList []
          in  ( _lhsOmessages,_lhsOnodeType)))
-- SelectList --------------------------------------------------
data SelectList  = SelectList (SelectItemList) (StringList) 
                 deriving ( Eq,Show)
-- cata
sem_SelectList :: SelectList  ->
                  T_SelectList 
sem_SelectList (SelectList _selectItemList _stringList )  =
    (sem_SelectList_SelectList (sem_SelectItemList _selectItemList ) (sem_StringList _stringList ) )
-- semantic domain
type T_SelectList  = Bool ->
                     MySourcePos ->
                     ( ([Message]),Type)
data Inh_SelectList  = Inh_SelectList {inLoop_Inh_SelectList :: Bool,sourcePos_Inh_SelectList :: MySourcePos}
data Syn_SelectList  = Syn_SelectList {messages_Syn_SelectList :: [Message],nodeType_Syn_SelectList :: Type}
wrap_SelectList :: T_SelectList  ->
                   Inh_SelectList  ->
                   Syn_SelectList 
wrap_SelectList sem (Inh_SelectList _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_SelectList _lhsOmessages _lhsOnodeType ))
sem_SelectList_SelectList :: T_SelectItemList  ->
                             T_StringList  ->
                             T_SelectList 
sem_SelectList_SelectList selectItemList_ stringList_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _selectItemListOinLoop :: Bool
              _selectItemListOsourcePos :: MySourcePos
              _stringListOinLoop :: Bool
              _stringListOsourcePos :: MySourcePos
              _selectItemListImessages :: ([Message])
              _selectItemListInodeType :: Type
              _stringListImessages :: ([Message])
              _stringListInodeType :: Type
              _lhsOmessages =
                  _selectItemListImessages ++ _stringListImessages
              _lhsOnodeType =
                  _selectItemListInodeType `appendTypeList` _stringListInodeType
              _selectItemListOinLoop =
                  _lhsIinLoop
              _selectItemListOsourcePos =
                  _lhsIsourcePos
              _stringListOinLoop =
                  _lhsIinLoop
              _stringListOsourcePos =
                  _lhsIsourcePos
              ( _selectItemListImessages,_selectItemListInodeType) =
                  (selectItemList_ _selectItemListOinLoop _selectItemListOsourcePos )
              ( _stringListImessages,_stringListInodeType) =
                  (stringList_ _stringListOinLoop _stringListOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
-- SetClause ---------------------------------------------------
data SetClause  = RowSetClause (StringList) (ExpressionList) 
                | SetClause (String) (Expression) 
                deriving ( Eq,Show)
-- cata
sem_SetClause :: SetClause  ->
                 T_SetClause 
sem_SetClause (RowSetClause _stringList _expressionList )  =
    (sem_SetClause_RowSetClause (sem_StringList _stringList ) (sem_ExpressionList _expressionList ) )
sem_SetClause (SetClause _string _expression )  =
    (sem_SetClause_SetClause _string (sem_Expression _expression ) )
-- semantic domain
type T_SetClause  = Bool ->
                    MySourcePos ->
                    ( ([Message]),Type)
data Inh_SetClause  = Inh_SetClause {inLoop_Inh_SetClause :: Bool,sourcePos_Inh_SetClause :: MySourcePos}
data Syn_SetClause  = Syn_SetClause {messages_Syn_SetClause :: [Message],nodeType_Syn_SetClause :: Type}
wrap_SetClause :: T_SetClause  ->
                  Inh_SetClause  ->
                  Syn_SetClause 
wrap_SetClause sem (Inh_SetClause _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_SetClause _lhsOmessages _lhsOnodeType ))
sem_SetClause_RowSetClause :: T_StringList  ->
                              T_ExpressionList  ->
                              T_SetClause 
sem_SetClause_RowSetClause stringList_ expressionList_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _stringListOinLoop :: Bool
              _stringListOsourcePos :: MySourcePos
              _expressionListOinLoop :: Bool
              _expressionListOsourcePos :: MySourcePos
              _stringListImessages :: ([Message])
              _stringListInodeType :: Type
              _expressionListImessages :: ([Message])
              _expressionListInodeType :: Type
              _lhsOmessages =
                  _stringListImessages ++ _expressionListImessages
              _lhsOnodeType =
                  _stringListInodeType `setUnknown` _expressionListInodeType
              _stringListOinLoop =
                  _lhsIinLoop
              _stringListOsourcePos =
                  _lhsIsourcePos
              _expressionListOinLoop =
                  _lhsIinLoop
              _expressionListOsourcePos =
                  _lhsIsourcePos
              ( _stringListImessages,_stringListInodeType) =
                  (stringList_ _stringListOinLoop _stringListOsourcePos )
              ( _expressionListImessages,_expressionListInodeType) =
                  (expressionList_ _expressionListOinLoop _expressionListOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_SetClause_SetClause :: String ->
                           T_Expression  ->
                           T_SetClause 
sem_SetClause_SetClause string_ expression_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _expressionOinLoop :: Bool
              _expressionOsourcePos :: MySourcePos
              _expressionImessages :: ([Message])
              _expressionInodeType :: Type
              _lhsOmessages =
                  _expressionImessages
              _lhsOnodeType =
                  _expressionInodeType
              _expressionOinLoop =
                  _lhsIinLoop
              _expressionOsourcePos =
                  _lhsIsourcePos
              ( _expressionImessages,_expressionInodeType) =
                  (expression_ _expressionOinLoop _expressionOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
-- SetClauseList -----------------------------------------------
type SetClauseList  = [(SetClause)]
-- cata
sem_SetClauseList :: SetClauseList  ->
                     T_SetClauseList 
sem_SetClauseList list  =
    (Prelude.foldr sem_SetClauseList_Cons sem_SetClauseList_Nil (Prelude.map sem_SetClause list) )
-- semantic domain
type T_SetClauseList  = Bool ->
                        MySourcePos ->
                        ( ([Message]),Type)
data Inh_SetClauseList  = Inh_SetClauseList {inLoop_Inh_SetClauseList :: Bool,sourcePos_Inh_SetClauseList :: MySourcePos}
data Syn_SetClauseList  = Syn_SetClauseList {messages_Syn_SetClauseList :: [Message],nodeType_Syn_SetClauseList :: Type}
wrap_SetClauseList :: T_SetClauseList  ->
                      Inh_SetClauseList  ->
                      Syn_SetClauseList 
wrap_SetClauseList sem (Inh_SetClauseList _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_SetClauseList _lhsOmessages _lhsOnodeType ))
sem_SetClauseList_Cons :: T_SetClause  ->
                          T_SetClauseList  ->
                          T_SetClauseList 
sem_SetClauseList_Cons hd_ tl_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _hdOinLoop :: Bool
              _hdOsourcePos :: MySourcePos
              _tlOinLoop :: Bool
              _tlOsourcePos :: MySourcePos
              _hdImessages :: ([Message])
              _hdInodeType :: Type
              _tlImessages :: ([Message])
              _tlInodeType :: Type
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _lhsOnodeType =
                  _hdInodeType `appendTypeList` _tlInodeType
              _hdOinLoop =
                  _lhsIinLoop
              _hdOsourcePos =
                  _lhsIsourcePos
              _tlOinLoop =
                  _lhsIinLoop
              _tlOsourcePos =
                  _lhsIsourcePos
              ( _hdImessages,_hdInodeType) =
                  (hd_ _hdOinLoop _hdOsourcePos )
              ( _tlImessages,_tlInodeType) =
                  (tl_ _tlOinLoop _tlOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_SetClauseList_Nil :: T_SetClauseList 
sem_SetClauseList_Nil  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  TypeList []
          in  ( _lhsOmessages,_lhsOnodeType)))
-- SourcePosStatement ------------------------------------------
type SourcePosStatement  = ( (MySourcePos),(Statement))
-- cata
sem_SourcePosStatement :: SourcePosStatement  ->
                          T_SourcePosStatement 
sem_SourcePosStatement ( x1,x2)  =
    (sem_SourcePosStatement_Tuple (sem_MySourcePos x1 ) (sem_Statement x2 ) )
-- semantic domain
type T_SourcePosStatement  = Bool ->
                             MySourcePos ->
                             ( ([Message]),Type)
data Inh_SourcePosStatement  = Inh_SourcePosStatement {inLoop_Inh_SourcePosStatement :: Bool,sourcePos_Inh_SourcePosStatement :: MySourcePos}
data Syn_SourcePosStatement  = Syn_SourcePosStatement {messages_Syn_SourcePosStatement :: [Message],nodeType_Syn_SourcePosStatement :: Type}
wrap_SourcePosStatement :: T_SourcePosStatement  ->
                           Inh_SourcePosStatement  ->
                           Syn_SourcePosStatement 
wrap_SourcePosStatement sem (Inh_SourcePosStatement _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_SourcePosStatement _lhsOmessages _lhsOnodeType ))
sem_SourcePosStatement_Tuple :: T_MySourcePos  ->
                                T_Statement  ->
                                T_SourcePosStatement 
sem_SourcePosStatement_Tuple x1_ x2_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _x2OsourcePos :: MySourcePos
              _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _x2OinLoop :: Bool
              _x1Ival :: MySourcePos
              _x2Imessages :: ([Message])
              _x2InodeType :: Type
              _x2OsourcePos =
                  _x1Ival
              _lhsOmessages =
                  _x2Imessages
              _lhsOnodeType =
                  _x2InodeType
              _x2OinLoop =
                  _lhsIinLoop
              ( _x1Ival) =
                  (x1_ )
              ( _x2Imessages,_x2InodeType) =
                  (x2_ _x2OinLoop _x2OsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
-- Statement ---------------------------------------------------
data Statement  = Assignment (String) (Expression) 
                | CaseStatement (Expression) (ExpressionListStatementListPairList) (StatementList) 
                | ContinueStatement 
                | Copy (String) (StringList) (CopySource) 
                | CopyData (String) 
                | CreateDomain (String) (TypeName) (Maybe Expression) 
                | CreateFunction (Language) (String) (ParamDefList) (TypeName) (String) (FnBody) (Volatility) 
                | CreateTable (String) (AttributeDefList) (ConstraintList) 
                | CreateTableAs (String) (SelectExpression) 
                | CreateType (String) (TypeAttributeDefList) 
                | CreateView (String) (SelectExpression) 
                | Delete (String) (Maybe Expression) (Maybe SelectList) 
                | DropFunction (IfExists) (StringStringListPairList) (Cascade) 
                | DropSomething (DropType) (IfExists) (StringList) (Cascade) 
                | Execute (Expression) 
                | ExecuteInto (Expression) (StringList) 
                | ForIntegerStatement (String) (Expression) (Expression) (StatementList) 
                | ForSelectStatement (String) (SelectExpression) (StatementList) 
                | If (ExpressionStatementListPairList) (StatementList) 
                | Insert (String) (StringList) (SelectExpression) (Maybe SelectList) 
                | NullStatement 
                | Perform (Expression) 
                | Raise (RaiseType) (String) (ExpressionList) 
                | Return (Maybe Expression) 
                | ReturnNext (Expression) 
                | ReturnQuery (SelectExpression) 
                | SelectStatement (SelectExpression) 
                | Truncate (StringList) (RestartIdentity) (Cascade) 
                | Update (String) (SetClauseList) (Maybe Expression) (Maybe SelectList) 
                | WhileStatement (Expression) (StatementList) 
                deriving ( Eq,Show)
-- cata
sem_Statement :: Statement  ->
                 T_Statement 
sem_Statement (Assignment _target _value )  =
    (sem_Statement_Assignment _target (sem_Expression _value ) )
sem_Statement (CaseStatement _val _cases _els )  =
    (sem_Statement_CaseStatement (sem_Expression _val ) (sem_ExpressionListStatementListPairList _cases ) (sem_StatementList _els ) )
sem_Statement (ContinueStatement )  =
    (sem_Statement_ContinueStatement )
sem_Statement (Copy _table _targetCols _source )  =
    (sem_Statement_Copy _table (sem_StringList _targetCols ) (sem_CopySource _source ) )
sem_Statement (CopyData _insData )  =
    (sem_Statement_CopyData _insData )
sem_Statement (CreateDomain _name _typ _check )  =
    (sem_Statement_CreateDomain _name (sem_TypeName _typ ) _check )
sem_Statement (CreateFunction _lang _name _params _rettype _bodyQuote _body _vol )  =
    (sem_Statement_CreateFunction (sem_Language _lang ) _name (sem_ParamDefList _params ) (sem_TypeName _rettype ) _bodyQuote (sem_FnBody _body ) (sem_Volatility _vol ) )
sem_Statement (CreateTable _name _atts _cons )  =
    (sem_Statement_CreateTable _name (sem_AttributeDefList _atts ) (sem_ConstraintList _cons ) )
sem_Statement (CreateTableAs _name _expr )  =
    (sem_Statement_CreateTableAs _name (sem_SelectExpression _expr ) )
sem_Statement (CreateType _name _atts )  =
    (sem_Statement_CreateType _name (sem_TypeAttributeDefList _atts ) )
sem_Statement (CreateView _name _expr )  =
    (sem_Statement_CreateView _name (sem_SelectExpression _expr ) )
sem_Statement (Delete _table _whr _returning )  =
    (sem_Statement_Delete _table _whr _returning )
sem_Statement (DropFunction _ifE _sigs _cascade )  =
    (sem_Statement_DropFunction (sem_IfExists _ifE ) (sem_StringStringListPairList _sigs ) (sem_Cascade _cascade ) )
sem_Statement (DropSomething _dropType _ifE _names _cascade )  =
    (sem_Statement_DropSomething (sem_DropType _dropType ) (sem_IfExists _ifE ) (sem_StringList _names ) (sem_Cascade _cascade ) )
sem_Statement (Execute _expr )  =
    (sem_Statement_Execute (sem_Expression _expr ) )
sem_Statement (ExecuteInto _expr _targets )  =
    (sem_Statement_ExecuteInto (sem_Expression _expr ) (sem_StringList _targets ) )
sem_Statement (ForIntegerStatement _var _from _to _sts )  =
    (sem_Statement_ForIntegerStatement _var (sem_Expression _from ) (sem_Expression _to ) (sem_StatementList _sts ) )
sem_Statement (ForSelectStatement _var _sel _sts )  =
    (sem_Statement_ForSelectStatement _var (sem_SelectExpression _sel ) (sem_StatementList _sts ) )
sem_Statement (If _cases _els )  =
    (sem_Statement_If (sem_ExpressionStatementListPairList _cases ) (sem_StatementList _els ) )
sem_Statement (Insert _table _targetCols _insData _returning )  =
    (sem_Statement_Insert _table (sem_StringList _targetCols ) (sem_SelectExpression _insData ) _returning )
sem_Statement (NullStatement )  =
    (sem_Statement_NullStatement )
sem_Statement (Perform _expr )  =
    (sem_Statement_Perform (sem_Expression _expr ) )
sem_Statement (Raise _level _message _args )  =
    (sem_Statement_Raise (sem_RaiseType _level ) _message (sem_ExpressionList _args ) )
sem_Statement (Return _value )  =
    (sem_Statement_Return _value )
sem_Statement (ReturnNext _expr )  =
    (sem_Statement_ReturnNext (sem_Expression _expr ) )
sem_Statement (ReturnQuery _sel )  =
    (sem_Statement_ReturnQuery (sem_SelectExpression _sel ) )
sem_Statement (SelectStatement _selectExpression )  =
    (sem_Statement_SelectStatement (sem_SelectExpression _selectExpression ) )
sem_Statement (Truncate _tables _restartIdentity _cascade )  =
    (sem_Statement_Truncate (sem_StringList _tables ) (sem_RestartIdentity _restartIdentity ) (sem_Cascade _cascade ) )
sem_Statement (Update _table _assigns _whr _returning )  =
    (sem_Statement_Update _table (sem_SetClauseList _assigns ) _whr _returning )
sem_Statement (WhileStatement _expr _sts )  =
    (sem_Statement_WhileStatement (sem_Expression _expr ) (sem_StatementList _sts ) )
-- semantic domain
type T_Statement  = Bool ->
                    MySourcePos ->
                    ( ([Message]),Type)
data Inh_Statement  = Inh_Statement {inLoop_Inh_Statement :: Bool,sourcePos_Inh_Statement :: MySourcePos}
data Syn_Statement  = Syn_Statement {messages_Syn_Statement :: [Message],nodeType_Syn_Statement :: Type}
wrap_Statement :: T_Statement  ->
                  Inh_Statement  ->
                  Syn_Statement 
wrap_Statement sem (Inh_Statement _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_Statement _lhsOmessages _lhsOnodeType ))
sem_Statement_Assignment :: String ->
                            T_Expression  ->
                            T_Statement 
sem_Statement_Assignment target_ value_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _valueOinLoop :: Bool
              _valueOsourcePos :: MySourcePos
              _valueImessages :: ([Message])
              _valueInodeType :: Type
              _lhsOmessages =
                  _valueImessages
              _lhsOnodeType =
                  _valueInodeType
              _valueOinLoop =
                  _lhsIinLoop
              _valueOsourcePos =
                  _lhsIsourcePos
              ( _valueImessages,_valueInodeType) =
                  (value_ _valueOinLoop _valueOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Statement_CaseStatement :: T_Expression  ->
                               T_ExpressionListStatementListPairList  ->
                               T_StatementList  ->
                               T_Statement 
sem_Statement_CaseStatement val_ cases_ els_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _valOinLoop :: Bool
              _valOsourcePos :: MySourcePos
              _casesOinLoop :: Bool
              _casesOsourcePos :: MySourcePos
              _elsOinLoop :: Bool
              _elsOsourcePos :: MySourcePos
              _valImessages :: ([Message])
              _valInodeType :: Type
              _casesImessages :: ([Message])
              _casesInodeType :: Type
              _elsImessages :: ([Message])
              _elsInodeType :: Type
              _lhsOmessages =
                  _valImessages ++ _casesImessages ++ _elsImessages
              _lhsOnodeType =
                  _valInodeType `setUnknown` _casesInodeType `setUnknown` _elsInodeType
              _valOinLoop =
                  _lhsIinLoop
              _valOsourcePos =
                  _lhsIsourcePos
              _casesOinLoop =
                  _lhsIinLoop
              _casesOsourcePos =
                  _lhsIsourcePos
              _elsOinLoop =
                  _lhsIinLoop
              _elsOsourcePos =
                  _lhsIsourcePos
              ( _valImessages,_valInodeType) =
                  (val_ _valOinLoop _valOsourcePos )
              ( _casesImessages,_casesInodeType) =
                  (cases_ _casesOinLoop _casesOsourcePos )
              ( _elsImessages,_elsInodeType) =
                  (els_ _elsOinLoop _elsOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Statement_ContinueStatement :: T_Statement 
sem_Statement_ContinueStatement  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  if not _lhsIinLoop
                    then [Error _lhsIsourcePos ContinueNotInLoop]
                    else []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Statement_Copy :: String ->
                      T_StringList  ->
                      T_CopySource  ->
                      T_Statement 
sem_Statement_Copy table_ targetCols_ source_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _targetColsOinLoop :: Bool
              _targetColsOsourcePos :: MySourcePos
              _sourceOinLoop :: Bool
              _sourceOsourcePos :: MySourcePos
              _targetColsImessages :: ([Message])
              _targetColsInodeType :: Type
              _sourceImessages :: ([Message])
              _sourceInodeType :: Type
              _lhsOmessages =
                  _targetColsImessages ++ _sourceImessages
              _lhsOnodeType =
                  _targetColsInodeType `setUnknown` _sourceInodeType
              _targetColsOinLoop =
                  _lhsIinLoop
              _targetColsOsourcePos =
                  _lhsIsourcePos
              _sourceOinLoop =
                  _lhsIinLoop
              _sourceOsourcePos =
                  _lhsIsourcePos
              ( _targetColsImessages,_targetColsInodeType) =
                  (targetCols_ _targetColsOinLoop _targetColsOsourcePos )
              ( _sourceImessages,_sourceInodeType) =
                  (source_ _sourceOinLoop _sourceOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Statement_CopyData :: String ->
                          T_Statement 
sem_Statement_CopyData insData_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Statement_CreateDomain :: String ->
                              T_TypeName  ->
                              (Maybe Expression) ->
                              T_Statement 
sem_Statement_CreateDomain name_ typ_ check_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _typOinLoop :: Bool
              _typOsourcePos :: MySourcePos
              _typImessages :: ([Message])
              _typInodeType :: Type
              _lhsOmessages =
                  _typImessages
              _lhsOnodeType =
                  _typInodeType
              _typOinLoop =
                  _lhsIinLoop
              _typOsourcePos =
                  _lhsIsourcePos
              ( _typImessages,_typInodeType) =
                  (typ_ _typOinLoop _typOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Statement_CreateFunction :: T_Language  ->
                                String ->
                                T_ParamDefList  ->
                                T_TypeName  ->
                                String ->
                                T_FnBody  ->
                                T_Volatility  ->
                                T_Statement 
sem_Statement_CreateFunction lang_ name_ params_ rettype_ bodyQuote_ body_ vol_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _bodyOinLoop :: Bool
              _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _langOinLoop :: Bool
              _langOsourcePos :: MySourcePos
              _paramsOinLoop :: Bool
              _paramsOsourcePos :: MySourcePos
              _rettypeOinLoop :: Bool
              _rettypeOsourcePos :: MySourcePos
              _bodyOsourcePos :: MySourcePos
              _volOinLoop :: Bool
              _volOsourcePos :: MySourcePos
              _langImessages :: ([Message])
              _langInodeType :: Type
              _paramsImessages :: ([Message])
              _paramsInodeType :: Type
              _rettypeImessages :: ([Message])
              _rettypeInodeType :: Type
              _bodyImessages :: ([Message])
              _bodyInodeType :: Type
              _volImessages :: ([Message])
              _volInodeType :: Type
              _bodyOinLoop =
                  False
              _lhsOmessages =
                  _langImessages ++ _paramsImessages ++ _rettypeImessages ++ _bodyImessages ++ _volImessages
              _lhsOnodeType =
                  _langInodeType `setUnknown` _paramsInodeType `setUnknown` _rettypeInodeType `setUnknown` _bodyInodeType `setUnknown` _volInodeType
              _langOinLoop =
                  _lhsIinLoop
              _langOsourcePos =
                  _lhsIsourcePos
              _paramsOinLoop =
                  _lhsIinLoop
              _paramsOsourcePos =
                  _lhsIsourcePos
              _rettypeOinLoop =
                  _lhsIinLoop
              _rettypeOsourcePos =
                  _lhsIsourcePos
              _bodyOsourcePos =
                  _lhsIsourcePos
              _volOinLoop =
                  _lhsIinLoop
              _volOsourcePos =
                  _lhsIsourcePos
              ( _langImessages,_langInodeType) =
                  (lang_ _langOinLoop _langOsourcePos )
              ( _paramsImessages,_paramsInodeType) =
                  (params_ _paramsOinLoop _paramsOsourcePos )
              ( _rettypeImessages,_rettypeInodeType) =
                  (rettype_ _rettypeOinLoop _rettypeOsourcePos )
              ( _bodyImessages,_bodyInodeType) =
                  (body_ _bodyOinLoop _bodyOsourcePos )
              ( _volImessages,_volInodeType) =
                  (vol_ _volOinLoop _volOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Statement_CreateTable :: String ->
                             T_AttributeDefList  ->
                             T_ConstraintList  ->
                             T_Statement 
sem_Statement_CreateTable name_ atts_ cons_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _attsOinLoop :: Bool
              _attsOsourcePos :: MySourcePos
              _consOinLoop :: Bool
              _consOsourcePos :: MySourcePos
              _attsImessages :: ([Message])
              _attsInodeType :: Type
              _consImessages :: ([Message])
              _consInodeType :: Type
              _lhsOmessages =
                  _attsImessages ++ _consImessages
              _lhsOnodeType =
                  _attsInodeType `setUnknown` _consInodeType
              _attsOinLoop =
                  _lhsIinLoop
              _attsOsourcePos =
                  _lhsIsourcePos
              _consOinLoop =
                  _lhsIinLoop
              _consOsourcePos =
                  _lhsIsourcePos
              ( _attsImessages,_attsInodeType) =
                  (atts_ _attsOinLoop _attsOsourcePos )
              ( _consImessages,_consInodeType) =
                  (cons_ _consOinLoop _consOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Statement_CreateTableAs :: String ->
                               T_SelectExpression  ->
                               T_Statement 
sem_Statement_CreateTableAs name_ expr_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _exprOinLoop :: Bool
              _exprOsourcePos :: MySourcePos
              _exprImessages :: ([Message])
              _exprInodeType :: Type
              _lhsOmessages =
                  _exprImessages
              _lhsOnodeType =
                  _exprInodeType
              _exprOinLoop =
                  _lhsIinLoop
              _exprOsourcePos =
                  _lhsIsourcePos
              ( _exprImessages,_exprInodeType) =
                  (expr_ _exprOinLoop _exprOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Statement_CreateType :: String ->
                            T_TypeAttributeDefList  ->
                            T_Statement 
sem_Statement_CreateType name_ atts_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _attsOinLoop :: Bool
              _attsOsourcePos :: MySourcePos
              _attsImessages :: ([Message])
              _attsInodeType :: Type
              _lhsOmessages =
                  _attsImessages
              _lhsOnodeType =
                  _attsInodeType
              _attsOinLoop =
                  _lhsIinLoop
              _attsOsourcePos =
                  _lhsIsourcePos
              ( _attsImessages,_attsInodeType) =
                  (atts_ _attsOinLoop _attsOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Statement_CreateView :: String ->
                            T_SelectExpression  ->
                            T_Statement 
sem_Statement_CreateView name_ expr_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _exprOinLoop :: Bool
              _exprOsourcePos :: MySourcePos
              _exprImessages :: ([Message])
              _exprInodeType :: Type
              _lhsOmessages =
                  _exprImessages
              _lhsOnodeType =
                  _exprInodeType
              _exprOinLoop =
                  _lhsIinLoop
              _exprOsourcePos =
                  _lhsIsourcePos
              ( _exprImessages,_exprInodeType) =
                  (expr_ _exprOinLoop _exprOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Statement_Delete :: String ->
                        (Maybe Expression) ->
                        (Maybe SelectList) ->
                        T_Statement 
sem_Statement_Delete table_ whr_ returning_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Statement_DropFunction :: T_IfExists  ->
                              T_StringStringListPairList  ->
                              T_Cascade  ->
                              T_Statement 
sem_Statement_DropFunction ifE_ sigs_ cascade_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _ifEOinLoop :: Bool
              _ifEOsourcePos :: MySourcePos
              _sigsOinLoop :: Bool
              _sigsOsourcePos :: MySourcePos
              _cascadeOinLoop :: Bool
              _cascadeOsourcePos :: MySourcePos
              _ifEImessages :: ([Message])
              _ifEInodeType :: Type
              _sigsImessages :: ([Message])
              _sigsInodeType :: Type
              _cascadeImessages :: ([Message])
              _cascadeInodeType :: Type
              _lhsOmessages =
                  _ifEImessages ++ _sigsImessages ++ _cascadeImessages
              _lhsOnodeType =
                  _ifEInodeType `setUnknown` _sigsInodeType `setUnknown` _cascadeInodeType
              _ifEOinLoop =
                  _lhsIinLoop
              _ifEOsourcePos =
                  _lhsIsourcePos
              _sigsOinLoop =
                  _lhsIinLoop
              _sigsOsourcePos =
                  _lhsIsourcePos
              _cascadeOinLoop =
                  _lhsIinLoop
              _cascadeOsourcePos =
                  _lhsIsourcePos
              ( _ifEImessages,_ifEInodeType) =
                  (ifE_ _ifEOinLoop _ifEOsourcePos )
              ( _sigsImessages,_sigsInodeType) =
                  (sigs_ _sigsOinLoop _sigsOsourcePos )
              ( _cascadeImessages,_cascadeInodeType) =
                  (cascade_ _cascadeOinLoop _cascadeOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Statement_DropSomething :: T_DropType  ->
                               T_IfExists  ->
                               T_StringList  ->
                               T_Cascade  ->
                               T_Statement 
sem_Statement_DropSomething dropType_ ifE_ names_ cascade_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _dropTypeOinLoop :: Bool
              _dropTypeOsourcePos :: MySourcePos
              _ifEOinLoop :: Bool
              _ifEOsourcePos :: MySourcePos
              _namesOinLoop :: Bool
              _namesOsourcePos :: MySourcePos
              _cascadeOinLoop :: Bool
              _cascadeOsourcePos :: MySourcePos
              _dropTypeImessages :: ([Message])
              _dropTypeInodeType :: Type
              _ifEImessages :: ([Message])
              _ifEInodeType :: Type
              _namesImessages :: ([Message])
              _namesInodeType :: Type
              _cascadeImessages :: ([Message])
              _cascadeInodeType :: Type
              _lhsOmessages =
                  _dropTypeImessages ++ _ifEImessages ++ _namesImessages ++ _cascadeImessages
              _lhsOnodeType =
                  _dropTypeInodeType `setUnknown` _ifEInodeType `setUnknown` _namesInodeType `setUnknown` _cascadeInodeType
              _dropTypeOinLoop =
                  _lhsIinLoop
              _dropTypeOsourcePos =
                  _lhsIsourcePos
              _ifEOinLoop =
                  _lhsIinLoop
              _ifEOsourcePos =
                  _lhsIsourcePos
              _namesOinLoop =
                  _lhsIinLoop
              _namesOsourcePos =
                  _lhsIsourcePos
              _cascadeOinLoop =
                  _lhsIinLoop
              _cascadeOsourcePos =
                  _lhsIsourcePos
              ( _dropTypeImessages,_dropTypeInodeType) =
                  (dropType_ _dropTypeOinLoop _dropTypeOsourcePos )
              ( _ifEImessages,_ifEInodeType) =
                  (ifE_ _ifEOinLoop _ifEOsourcePos )
              ( _namesImessages,_namesInodeType) =
                  (names_ _namesOinLoop _namesOsourcePos )
              ( _cascadeImessages,_cascadeInodeType) =
                  (cascade_ _cascadeOinLoop _cascadeOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Statement_Execute :: T_Expression  ->
                         T_Statement 
sem_Statement_Execute expr_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _exprOinLoop :: Bool
              _exprOsourcePos :: MySourcePos
              _exprImessages :: ([Message])
              _exprInodeType :: Type
              _lhsOmessages =
                  _exprImessages
              _lhsOnodeType =
                  _exprInodeType
              _exprOinLoop =
                  _lhsIinLoop
              _exprOsourcePos =
                  _lhsIsourcePos
              ( _exprImessages,_exprInodeType) =
                  (expr_ _exprOinLoop _exprOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Statement_ExecuteInto :: T_Expression  ->
                             T_StringList  ->
                             T_Statement 
sem_Statement_ExecuteInto expr_ targets_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _exprOinLoop :: Bool
              _exprOsourcePos :: MySourcePos
              _targetsOinLoop :: Bool
              _targetsOsourcePos :: MySourcePos
              _exprImessages :: ([Message])
              _exprInodeType :: Type
              _targetsImessages :: ([Message])
              _targetsInodeType :: Type
              _lhsOmessages =
                  _exprImessages ++ _targetsImessages
              _lhsOnodeType =
                  _exprInodeType `setUnknown` _targetsInodeType
              _exprOinLoop =
                  _lhsIinLoop
              _exprOsourcePos =
                  _lhsIsourcePos
              _targetsOinLoop =
                  _lhsIinLoop
              _targetsOsourcePos =
                  _lhsIsourcePos
              ( _exprImessages,_exprInodeType) =
                  (expr_ _exprOinLoop _exprOsourcePos )
              ( _targetsImessages,_targetsInodeType) =
                  (targets_ _targetsOinLoop _targetsOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Statement_ForIntegerStatement :: String ->
                                     T_Expression  ->
                                     T_Expression  ->
                                     T_StatementList  ->
                                     T_Statement 
sem_Statement_ForIntegerStatement var_ from_ to_ sts_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _stsOinLoop :: Bool
              _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _fromOinLoop :: Bool
              _fromOsourcePos :: MySourcePos
              _toOinLoop :: Bool
              _toOsourcePos :: MySourcePos
              _stsOsourcePos :: MySourcePos
              _fromImessages :: ([Message])
              _fromInodeType :: Type
              _toImessages :: ([Message])
              _toInodeType :: Type
              _stsImessages :: ([Message])
              _stsInodeType :: Type
              _stsOinLoop =
                  True
              _lhsOmessages =
                  _fromImessages ++ _toImessages ++ _stsImessages
              _lhsOnodeType =
                  _fromInodeType `setUnknown` _toInodeType `setUnknown` _stsInodeType
              _fromOinLoop =
                  _lhsIinLoop
              _fromOsourcePos =
                  _lhsIsourcePos
              _toOinLoop =
                  _lhsIinLoop
              _toOsourcePos =
                  _lhsIsourcePos
              _stsOsourcePos =
                  _lhsIsourcePos
              ( _fromImessages,_fromInodeType) =
                  (from_ _fromOinLoop _fromOsourcePos )
              ( _toImessages,_toInodeType) =
                  (to_ _toOinLoop _toOsourcePos )
              ( _stsImessages,_stsInodeType) =
                  (sts_ _stsOinLoop _stsOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Statement_ForSelectStatement :: String ->
                                    T_SelectExpression  ->
                                    T_StatementList  ->
                                    T_Statement 
sem_Statement_ForSelectStatement var_ sel_ sts_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _stsOinLoop :: Bool
              _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _selOinLoop :: Bool
              _selOsourcePos :: MySourcePos
              _stsOsourcePos :: MySourcePos
              _selImessages :: ([Message])
              _selInodeType :: Type
              _stsImessages :: ([Message])
              _stsInodeType :: Type
              _stsOinLoop =
                  True
              _lhsOmessages =
                  _selImessages ++ _stsImessages
              _lhsOnodeType =
                  _selInodeType `setUnknown` _stsInodeType
              _selOinLoop =
                  _lhsIinLoop
              _selOsourcePos =
                  _lhsIsourcePos
              _stsOsourcePos =
                  _lhsIsourcePos
              ( _selImessages,_selInodeType) =
                  (sel_ _selOinLoop _selOsourcePos )
              ( _stsImessages,_stsInodeType) =
                  (sts_ _stsOinLoop _stsOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Statement_If :: T_ExpressionStatementListPairList  ->
                    T_StatementList  ->
                    T_Statement 
sem_Statement_If cases_ els_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _casesOinLoop :: Bool
              _casesOsourcePos :: MySourcePos
              _elsOinLoop :: Bool
              _elsOsourcePos :: MySourcePos
              _casesImessages :: ([Message])
              _casesInodeType :: Type
              _elsImessages :: ([Message])
              _elsInodeType :: Type
              _lhsOmessages =
                  _casesImessages ++ _elsImessages
              _lhsOnodeType =
                  _casesInodeType `setUnknown` _elsInodeType
              _casesOinLoop =
                  _lhsIinLoop
              _casesOsourcePos =
                  _lhsIsourcePos
              _elsOinLoop =
                  _lhsIinLoop
              _elsOsourcePos =
                  _lhsIsourcePos
              ( _casesImessages,_casesInodeType) =
                  (cases_ _casesOinLoop _casesOsourcePos )
              ( _elsImessages,_elsInodeType) =
                  (els_ _elsOinLoop _elsOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Statement_Insert :: String ->
                        T_StringList  ->
                        T_SelectExpression  ->
                        (Maybe SelectList) ->
                        T_Statement 
sem_Statement_Insert table_ targetCols_ insData_ returning_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _targetColsOinLoop :: Bool
              _targetColsOsourcePos :: MySourcePos
              _insDataOinLoop :: Bool
              _insDataOsourcePos :: MySourcePos
              _targetColsImessages :: ([Message])
              _targetColsInodeType :: Type
              _insDataImessages :: ([Message])
              _insDataInodeType :: Type
              _lhsOmessages =
                  _targetColsImessages ++ _insDataImessages
              _lhsOnodeType =
                  _targetColsInodeType `setUnknown` _insDataInodeType
              _targetColsOinLoop =
                  _lhsIinLoop
              _targetColsOsourcePos =
                  _lhsIsourcePos
              _insDataOinLoop =
                  _lhsIinLoop
              _insDataOsourcePos =
                  _lhsIsourcePos
              ( _targetColsImessages,_targetColsInodeType) =
                  (targetCols_ _targetColsOinLoop _targetColsOsourcePos )
              ( _insDataImessages,_insDataInodeType) =
                  (insData_ _insDataOinLoop _insDataOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Statement_NullStatement :: T_Statement 
sem_Statement_NullStatement  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Statement_Perform :: T_Expression  ->
                         T_Statement 
sem_Statement_Perform expr_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _exprOinLoop :: Bool
              _exprOsourcePos :: MySourcePos
              _exprImessages :: ([Message])
              _exprInodeType :: Type
              _lhsOmessages =
                  _exprImessages
              _lhsOnodeType =
                  _exprInodeType
              _exprOinLoop =
                  _lhsIinLoop
              _exprOsourcePos =
                  _lhsIsourcePos
              ( _exprImessages,_exprInodeType) =
                  (expr_ _exprOinLoop _exprOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Statement_Raise :: T_RaiseType  ->
                       String ->
                       T_ExpressionList  ->
                       T_Statement 
sem_Statement_Raise level_ message_ args_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _levelOinLoop :: Bool
              _levelOsourcePos :: MySourcePos
              _argsOinLoop :: Bool
              _argsOsourcePos :: MySourcePos
              _levelImessages :: ([Message])
              _levelInodeType :: Type
              _argsImessages :: ([Message])
              _argsInodeType :: Type
              _lhsOmessages =
                  _levelImessages ++ _argsImessages
              _lhsOnodeType =
                  _levelInodeType `setUnknown` _argsInodeType
              _levelOinLoop =
                  _lhsIinLoop
              _levelOsourcePos =
                  _lhsIsourcePos
              _argsOinLoop =
                  _lhsIinLoop
              _argsOsourcePos =
                  _lhsIsourcePos
              ( _levelImessages,_levelInodeType) =
                  (level_ _levelOinLoop _levelOsourcePos )
              ( _argsImessages,_argsInodeType) =
                  (args_ _argsOinLoop _argsOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Statement_Return :: (Maybe Expression) ->
                        T_Statement 
sem_Statement_Return value_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Statement_ReturnNext :: T_Expression  ->
                            T_Statement 
sem_Statement_ReturnNext expr_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _exprOinLoop :: Bool
              _exprOsourcePos :: MySourcePos
              _exprImessages :: ([Message])
              _exprInodeType :: Type
              _lhsOmessages =
                  _exprImessages
              _lhsOnodeType =
                  _exprInodeType
              _exprOinLoop =
                  _lhsIinLoop
              _exprOsourcePos =
                  _lhsIsourcePos
              ( _exprImessages,_exprInodeType) =
                  (expr_ _exprOinLoop _exprOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Statement_ReturnQuery :: T_SelectExpression  ->
                             T_Statement 
sem_Statement_ReturnQuery sel_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _selOinLoop :: Bool
              _selOsourcePos :: MySourcePos
              _selImessages :: ([Message])
              _selInodeType :: Type
              _lhsOmessages =
                  _selImessages
              _lhsOnodeType =
                  _selInodeType
              _selOinLoop =
                  _lhsIinLoop
              _selOsourcePos =
                  _lhsIsourcePos
              ( _selImessages,_selInodeType) =
                  (sel_ _selOinLoop _selOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Statement_SelectStatement :: T_SelectExpression  ->
                                 T_Statement 
sem_Statement_SelectStatement selectExpression_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _selectExpressionOinLoop :: Bool
              _selectExpressionOsourcePos :: MySourcePos
              _selectExpressionImessages :: ([Message])
              _selectExpressionInodeType :: Type
              _lhsOmessages =
                  _selectExpressionImessages
              _lhsOnodeType =
                  _selectExpressionInodeType
              _selectExpressionOinLoop =
                  _lhsIinLoop
              _selectExpressionOsourcePos =
                  _lhsIsourcePos
              ( _selectExpressionImessages,_selectExpressionInodeType) =
                  (selectExpression_ _selectExpressionOinLoop _selectExpressionOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Statement_Truncate :: T_StringList  ->
                          T_RestartIdentity  ->
                          T_Cascade  ->
                          T_Statement 
sem_Statement_Truncate tables_ restartIdentity_ cascade_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _tablesOinLoop :: Bool
              _tablesOsourcePos :: MySourcePos
              _restartIdentityOinLoop :: Bool
              _restartIdentityOsourcePos :: MySourcePos
              _cascadeOinLoop :: Bool
              _cascadeOsourcePos :: MySourcePos
              _tablesImessages :: ([Message])
              _tablesInodeType :: Type
              _restartIdentityImessages :: ([Message])
              _restartIdentityInodeType :: Type
              _cascadeImessages :: ([Message])
              _cascadeInodeType :: Type
              _lhsOmessages =
                  _tablesImessages ++ _restartIdentityImessages ++ _cascadeImessages
              _lhsOnodeType =
                  _tablesInodeType `setUnknown` _restartIdentityInodeType `setUnknown` _cascadeInodeType
              _tablesOinLoop =
                  _lhsIinLoop
              _tablesOsourcePos =
                  _lhsIsourcePos
              _restartIdentityOinLoop =
                  _lhsIinLoop
              _restartIdentityOsourcePos =
                  _lhsIsourcePos
              _cascadeOinLoop =
                  _lhsIinLoop
              _cascadeOsourcePos =
                  _lhsIsourcePos
              ( _tablesImessages,_tablesInodeType) =
                  (tables_ _tablesOinLoop _tablesOsourcePos )
              ( _restartIdentityImessages,_restartIdentityInodeType) =
                  (restartIdentity_ _restartIdentityOinLoop _restartIdentityOsourcePos )
              ( _cascadeImessages,_cascadeInodeType) =
                  (cascade_ _cascadeOinLoop _cascadeOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Statement_Update :: String ->
                        T_SetClauseList  ->
                        (Maybe Expression) ->
                        (Maybe SelectList) ->
                        T_Statement 
sem_Statement_Update table_ assigns_ whr_ returning_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _assignsOinLoop :: Bool
              _assignsOsourcePos :: MySourcePos
              _assignsImessages :: ([Message])
              _assignsInodeType :: Type
              _lhsOmessages =
                  _assignsImessages
              _lhsOnodeType =
                  _assignsInodeType
              _assignsOinLoop =
                  _lhsIinLoop
              _assignsOsourcePos =
                  _lhsIsourcePos
              ( _assignsImessages,_assignsInodeType) =
                  (assigns_ _assignsOinLoop _assignsOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Statement_WhileStatement :: T_Expression  ->
                                T_StatementList  ->
                                T_Statement 
sem_Statement_WhileStatement expr_ sts_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _stsOinLoop :: Bool
              _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _exprOinLoop :: Bool
              _exprOsourcePos :: MySourcePos
              _stsOsourcePos :: MySourcePos
              _exprImessages :: ([Message])
              _exprInodeType :: Type
              _stsImessages :: ([Message])
              _stsInodeType :: Type
              _stsOinLoop =
                  True
              _lhsOmessages =
                  _exprImessages ++ _stsImessages
              _lhsOnodeType =
                  _exprInodeType `setUnknown` _stsInodeType
              _exprOinLoop =
                  _lhsIinLoop
              _exprOsourcePos =
                  _lhsIsourcePos
              _stsOsourcePos =
                  _lhsIsourcePos
              ( _exprImessages,_exprInodeType) =
                  (expr_ _exprOinLoop _exprOsourcePos )
              ( _stsImessages,_stsInodeType) =
                  (sts_ _stsOinLoop _stsOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
-- StatementList -----------------------------------------------
type StatementList  = [(SourcePosStatement)]
-- cata
sem_StatementList :: StatementList  ->
                     T_StatementList 
sem_StatementList list  =
    (Prelude.foldr sem_StatementList_Cons sem_StatementList_Nil (Prelude.map sem_SourcePosStatement list) )
-- semantic domain
type T_StatementList  = Bool ->
                        MySourcePos ->
                        ( ([Message]),Type)
data Inh_StatementList  = Inh_StatementList {inLoop_Inh_StatementList :: Bool,sourcePos_Inh_StatementList :: MySourcePos}
data Syn_StatementList  = Syn_StatementList {messages_Syn_StatementList :: [Message],nodeType_Syn_StatementList :: Type}
wrap_StatementList :: T_StatementList  ->
                      Inh_StatementList  ->
                      Syn_StatementList 
wrap_StatementList sem (Inh_StatementList _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_StatementList _lhsOmessages _lhsOnodeType ))
sem_StatementList_Cons :: T_SourcePosStatement  ->
                          T_StatementList  ->
                          T_StatementList 
sem_StatementList_Cons hd_ tl_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _hdOinLoop :: Bool
              _hdOsourcePos :: MySourcePos
              _tlOinLoop :: Bool
              _tlOsourcePos :: MySourcePos
              _hdImessages :: ([Message])
              _hdInodeType :: Type
              _tlImessages :: ([Message])
              _tlInodeType :: Type
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _lhsOnodeType =
                  _hdInodeType `appendTypeList` _tlInodeType
              _hdOinLoop =
                  _lhsIinLoop
              _hdOsourcePos =
                  _lhsIsourcePos
              _tlOinLoop =
                  _lhsIinLoop
              _tlOsourcePos =
                  _lhsIsourcePos
              ( _hdImessages,_hdInodeType) =
                  (hd_ _hdOinLoop _hdOsourcePos )
              ( _tlImessages,_tlInodeType) =
                  (tl_ _tlOinLoop _tlOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_StatementList_Nil :: T_StatementList 
sem_StatementList_Nil  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  TypeList []
          in  ( _lhsOmessages,_lhsOnodeType)))
-- StringList --------------------------------------------------
type StringList  = [(String)]
-- cata
sem_StringList :: StringList  ->
                  T_StringList 
sem_StringList list  =
    (Prelude.foldr sem_StringList_Cons sem_StringList_Nil list )
-- semantic domain
type T_StringList  = Bool ->
                     MySourcePos ->
                     ( ([Message]),Type)
data Inh_StringList  = Inh_StringList {inLoop_Inh_StringList :: Bool,sourcePos_Inh_StringList :: MySourcePos}
data Syn_StringList  = Syn_StringList {messages_Syn_StringList :: [Message],nodeType_Syn_StringList :: Type}
wrap_StringList :: T_StringList  ->
                   Inh_StringList  ->
                   Syn_StringList 
wrap_StringList sem (Inh_StringList _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_StringList _lhsOmessages _lhsOnodeType ))
sem_StringList_Cons :: String ->
                       T_StringList  ->
                       T_StringList 
sem_StringList_Cons hd_ tl_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _tlOinLoop :: Bool
              _tlOsourcePos :: MySourcePos
              _tlImessages :: ([Message])
              _tlInodeType :: Type
              _lhsOmessages =
                  _tlImessages
              _lhsOnodeType =
                  _tlInodeType
              _tlOinLoop =
                  _lhsIinLoop
              _tlOsourcePos =
                  _lhsIsourcePos
              ( _tlImessages,_tlInodeType) =
                  (tl_ _tlOinLoop _tlOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_StringList_Nil :: T_StringList 
sem_StringList_Nil  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  TypeList []
          in  ( _lhsOmessages,_lhsOnodeType)))
-- StringStringListPair ----------------------------------------
type StringStringListPair  = ( (String),(StringList))
-- cata
sem_StringStringListPair :: StringStringListPair  ->
                            T_StringStringListPair 
sem_StringStringListPair ( x1,x2)  =
    (sem_StringStringListPair_Tuple x1 (sem_StringList x2 ) )
-- semantic domain
type T_StringStringListPair  = Bool ->
                               MySourcePos ->
                               ( ([Message]),Type)
data Inh_StringStringListPair  = Inh_StringStringListPair {inLoop_Inh_StringStringListPair :: Bool,sourcePos_Inh_StringStringListPair :: MySourcePos}
data Syn_StringStringListPair  = Syn_StringStringListPair {messages_Syn_StringStringListPair :: [Message],nodeType_Syn_StringStringListPair :: Type}
wrap_StringStringListPair :: T_StringStringListPair  ->
                             Inh_StringStringListPair  ->
                             Syn_StringStringListPair 
wrap_StringStringListPair sem (Inh_StringStringListPair _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_StringStringListPair _lhsOmessages _lhsOnodeType ))
sem_StringStringListPair_Tuple :: String ->
                                  T_StringList  ->
                                  T_StringStringListPair 
sem_StringStringListPair_Tuple x1_ x2_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _x2OinLoop :: Bool
              _x2OsourcePos :: MySourcePos
              _x2Imessages :: ([Message])
              _x2InodeType :: Type
              _lhsOmessages =
                  _x2Imessages
              _lhsOnodeType =
                  _x2InodeType
              _x2OinLoop =
                  _lhsIinLoop
              _x2OsourcePos =
                  _lhsIsourcePos
              ( _x2Imessages,_x2InodeType) =
                  (x2_ _x2OinLoop _x2OsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
-- StringStringListPairList ------------------------------------
type StringStringListPairList  = [(StringStringListPair)]
-- cata
sem_StringStringListPairList :: StringStringListPairList  ->
                                T_StringStringListPairList 
sem_StringStringListPairList list  =
    (Prelude.foldr sem_StringStringListPairList_Cons sem_StringStringListPairList_Nil (Prelude.map sem_StringStringListPair list) )
-- semantic domain
type T_StringStringListPairList  = Bool ->
                                   MySourcePos ->
                                   ( ([Message]),Type)
data Inh_StringStringListPairList  = Inh_StringStringListPairList {inLoop_Inh_StringStringListPairList :: Bool,sourcePos_Inh_StringStringListPairList :: MySourcePos}
data Syn_StringStringListPairList  = Syn_StringStringListPairList {messages_Syn_StringStringListPairList :: [Message],nodeType_Syn_StringStringListPairList :: Type}
wrap_StringStringListPairList :: T_StringStringListPairList  ->
                                 Inh_StringStringListPairList  ->
                                 Syn_StringStringListPairList 
wrap_StringStringListPairList sem (Inh_StringStringListPairList _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_StringStringListPairList _lhsOmessages _lhsOnodeType ))
sem_StringStringListPairList_Cons :: T_StringStringListPair  ->
                                     T_StringStringListPairList  ->
                                     T_StringStringListPairList 
sem_StringStringListPairList_Cons hd_ tl_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _hdOinLoop :: Bool
              _hdOsourcePos :: MySourcePos
              _tlOinLoop :: Bool
              _tlOsourcePos :: MySourcePos
              _hdImessages :: ([Message])
              _hdInodeType :: Type
              _tlImessages :: ([Message])
              _tlInodeType :: Type
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _lhsOnodeType =
                  _hdInodeType `appendTypeList` _tlInodeType
              _hdOinLoop =
                  _lhsIinLoop
              _hdOsourcePos =
                  _lhsIsourcePos
              _tlOinLoop =
                  _lhsIinLoop
              _tlOsourcePos =
                  _lhsIsourcePos
              ( _hdImessages,_hdInodeType) =
                  (hd_ _hdOinLoop _hdOsourcePos )
              ( _tlImessages,_tlInodeType) =
                  (tl_ _tlOinLoop _tlOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_StringStringListPairList_Nil :: T_StringStringListPairList 
sem_StringStringListPairList_Nil  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  TypeList []
          in  ( _lhsOmessages,_lhsOnodeType)))
-- TableRef ----------------------------------------------------
data TableRef  = JoinedTref (TableRef) (Natural) (JoinType) (TableRef) (Maybe JoinExpression) 
               | SubTref (SelectExpression) (String) 
               | Tref (String) 
               | TrefAlias (String) (String) 
               | TrefFun (Expression) 
               | TrefFunAlias (Expression) (String) 
               deriving ( Eq,Show)
-- cata
sem_TableRef :: TableRef  ->
                T_TableRef 
sem_TableRef (JoinedTref _tref _nat _joinType _jtref _onExpr )  =
    (sem_TableRef_JoinedTref (sem_TableRef _tref ) (sem_Natural _nat ) (sem_JoinType _joinType ) (sem_TableRef _jtref ) _onExpr )
sem_TableRef (SubTref _sel _alias )  =
    (sem_TableRef_SubTref (sem_SelectExpression _sel ) _alias )
sem_TableRef (Tref _string )  =
    (sem_TableRef_Tref _string )
sem_TableRef (TrefAlias _tref _alias )  =
    (sem_TableRef_TrefAlias _tref _alias )
sem_TableRef (TrefFun _expression )  =
    (sem_TableRef_TrefFun (sem_Expression _expression ) )
sem_TableRef (TrefFunAlias _expression _string )  =
    (sem_TableRef_TrefFunAlias (sem_Expression _expression ) _string )
-- semantic domain
type T_TableRef  = Bool ->
                   MySourcePos ->
                   ( ([Message]),Type)
data Inh_TableRef  = Inh_TableRef {inLoop_Inh_TableRef :: Bool,sourcePos_Inh_TableRef :: MySourcePos}
data Syn_TableRef  = Syn_TableRef {messages_Syn_TableRef :: [Message],nodeType_Syn_TableRef :: Type}
wrap_TableRef :: T_TableRef  ->
                 Inh_TableRef  ->
                 Syn_TableRef 
wrap_TableRef sem (Inh_TableRef _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_TableRef _lhsOmessages _lhsOnodeType ))
sem_TableRef_JoinedTref :: T_TableRef  ->
                           T_Natural  ->
                           T_JoinType  ->
                           T_TableRef  ->
                           (Maybe JoinExpression) ->
                           T_TableRef 
sem_TableRef_JoinedTref tref_ nat_ joinType_ jtref_ onExpr_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _trefOinLoop :: Bool
              _trefOsourcePos :: MySourcePos
              _natOinLoop :: Bool
              _natOsourcePos :: MySourcePos
              _joinTypeOinLoop :: Bool
              _joinTypeOsourcePos :: MySourcePos
              _jtrefOinLoop :: Bool
              _jtrefOsourcePos :: MySourcePos
              _trefImessages :: ([Message])
              _trefInodeType :: Type
              _natImessages :: ([Message])
              _natInodeType :: Type
              _joinTypeImessages :: ([Message])
              _joinTypeInodeType :: Type
              _jtrefImessages :: ([Message])
              _jtrefInodeType :: Type
              _lhsOmessages =
                  _trefImessages ++ _natImessages ++ _joinTypeImessages ++ _jtrefImessages
              _lhsOnodeType =
                  _trefInodeType `setUnknown` _natInodeType `setUnknown` _joinTypeInodeType `setUnknown` _jtrefInodeType
              _trefOinLoop =
                  _lhsIinLoop
              _trefOsourcePos =
                  _lhsIsourcePos
              _natOinLoop =
                  _lhsIinLoop
              _natOsourcePos =
                  _lhsIsourcePos
              _joinTypeOinLoop =
                  _lhsIinLoop
              _joinTypeOsourcePos =
                  _lhsIsourcePos
              _jtrefOinLoop =
                  _lhsIinLoop
              _jtrefOsourcePos =
                  _lhsIsourcePos
              ( _trefImessages,_trefInodeType) =
                  (tref_ _trefOinLoop _trefOsourcePos )
              ( _natImessages,_natInodeType) =
                  (nat_ _natOinLoop _natOsourcePos )
              ( _joinTypeImessages,_joinTypeInodeType) =
                  (joinType_ _joinTypeOinLoop _joinTypeOsourcePos )
              ( _jtrefImessages,_jtrefInodeType) =
                  (jtref_ _jtrefOinLoop _jtrefOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_TableRef_SubTref :: T_SelectExpression  ->
                        String ->
                        T_TableRef 
sem_TableRef_SubTref sel_ alias_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _selOinLoop :: Bool
              _selOsourcePos :: MySourcePos
              _selImessages :: ([Message])
              _selInodeType :: Type
              _lhsOmessages =
                  _selImessages
              _lhsOnodeType =
                  _selInodeType
              _selOinLoop =
                  _lhsIinLoop
              _selOsourcePos =
                  _lhsIsourcePos
              ( _selImessages,_selInodeType) =
                  (sel_ _selOinLoop _selOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_TableRef_Tref :: String ->
                     T_TableRef 
sem_TableRef_Tref string_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_TableRef_TrefAlias :: String ->
                          String ->
                          T_TableRef 
sem_TableRef_TrefAlias tref_ alias_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_TableRef_TrefFun :: T_Expression  ->
                        T_TableRef 
sem_TableRef_TrefFun expression_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _expressionOinLoop :: Bool
              _expressionOsourcePos :: MySourcePos
              _expressionImessages :: ([Message])
              _expressionInodeType :: Type
              _lhsOmessages =
                  _expressionImessages
              _lhsOnodeType =
                  _expressionInodeType
              _expressionOinLoop =
                  _lhsIinLoop
              _expressionOsourcePos =
                  _lhsIsourcePos
              ( _expressionImessages,_expressionInodeType) =
                  (expression_ _expressionOinLoop _expressionOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_TableRef_TrefFunAlias :: T_Expression  ->
                             String ->
                             T_TableRef 
sem_TableRef_TrefFunAlias expression_ string_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _expressionOinLoop :: Bool
              _expressionOsourcePos :: MySourcePos
              _expressionImessages :: ([Message])
              _expressionInodeType :: Type
              _lhsOmessages =
                  _expressionImessages
              _lhsOnodeType =
                  _expressionInodeType
              _expressionOinLoop =
                  _lhsIinLoop
              _expressionOsourcePos =
                  _lhsIsourcePos
              ( _expressionImessages,_expressionInodeType) =
                  (expression_ _expressionOinLoop _expressionOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
-- Type --------------------------------------------------------
data Type  = ArrayType (Type) 
           | CompositeType (String) 
           | DomainType (String) (Type) 
           | Pseudo (PseudoType) 
           | Row ([Type]) 
           | ScalarType (String) 
           | SetOfType (Type) 
           | TypeError (MySourcePos) (TypeErrorInfo) 
           | TypeList ([Type]) 
           | UnknownType 
           deriving ( Eq,Show)
-- cata
sem_Type :: Type  ->
            T_Type 
sem_Type (ArrayType _type )  =
    (sem_Type_ArrayType (sem_Type _type ) )
sem_Type (CompositeType _name )  =
    (sem_Type_CompositeType _name )
sem_Type (DomainType _string _type )  =
    (sem_Type_DomainType _string (sem_Type _type ) )
sem_Type (Pseudo _pseudoType )  =
    (sem_Type_Pseudo (sem_PseudoType _pseudoType ) )
sem_Type (Row _attrs )  =
    (sem_Type_Row _attrs )
sem_Type (ScalarType _string )  =
    (sem_Type_ScalarType _string )
sem_Type (SetOfType _type )  =
    (sem_Type_SetOfType (sem_Type _type ) )
sem_Type (TypeError _mySourcePos _typeErrorInfo )  =
    (sem_Type_TypeError (sem_MySourcePos _mySourcePos ) (sem_TypeErrorInfo _typeErrorInfo ) )
sem_Type (TypeList _tps )  =
    (sem_Type_TypeList _tps )
sem_Type (UnknownType )  =
    (sem_Type_UnknownType )
-- semantic domain
type T_Type  = ( )
data Inh_Type  = Inh_Type {}
data Syn_Type  = Syn_Type {}
wrap_Type :: T_Type  ->
             Inh_Type  ->
             Syn_Type 
wrap_Type sem (Inh_Type )  =
    (let ( ) =
             (sem )
     in  (Syn_Type ))
sem_Type_ArrayType :: T_Type  ->
                      T_Type 
sem_Type_ArrayType type_  =
    (let 
     in  ( ))
sem_Type_CompositeType :: String ->
                          T_Type 
sem_Type_CompositeType name_  =
    (let 
     in  ( ))
sem_Type_DomainType :: String ->
                       T_Type  ->
                       T_Type 
sem_Type_DomainType string_ type_  =
    (let 
     in  ( ))
sem_Type_Pseudo :: T_PseudoType  ->
                   T_Type 
sem_Type_Pseudo pseudoType_  =
    (let 
     in  ( ))
sem_Type_Row :: ([Type]) ->
                T_Type 
sem_Type_Row attrs_  =
    (let 
     in  ( ))
sem_Type_ScalarType :: String ->
                       T_Type 
sem_Type_ScalarType string_  =
    (let 
     in  ( ))
sem_Type_SetOfType :: T_Type  ->
                      T_Type 
sem_Type_SetOfType type_  =
    (let 
     in  ( ))
sem_Type_TypeError :: T_MySourcePos  ->
                      T_TypeErrorInfo  ->
                      T_Type 
sem_Type_TypeError mySourcePos_ typeErrorInfo_  =
    (let _mySourcePosIval :: MySourcePos
         ( _mySourcePosIval) =
             (mySourcePos_ )
     in  ( ))
sem_Type_TypeList :: ([Type]) ->
                     T_Type 
sem_Type_TypeList tps_  =
    (let 
     in  ( ))
sem_Type_UnknownType :: T_Type 
sem_Type_UnknownType  =
    (let 
     in  ( ))
-- TypeAttributeDef --------------------------------------------
data TypeAttributeDef  = TypeAttDef (String) (TypeName) 
                       deriving ( Eq,Show)
-- cata
sem_TypeAttributeDef :: TypeAttributeDef  ->
                        T_TypeAttributeDef 
sem_TypeAttributeDef (TypeAttDef _name _typ )  =
    (sem_TypeAttributeDef_TypeAttDef _name (sem_TypeName _typ ) )
-- semantic domain
type T_TypeAttributeDef  = Bool ->
                           MySourcePos ->
                           ( ([Message]),Type)
data Inh_TypeAttributeDef  = Inh_TypeAttributeDef {inLoop_Inh_TypeAttributeDef :: Bool,sourcePos_Inh_TypeAttributeDef :: MySourcePos}
data Syn_TypeAttributeDef  = Syn_TypeAttributeDef {messages_Syn_TypeAttributeDef :: [Message],nodeType_Syn_TypeAttributeDef :: Type}
wrap_TypeAttributeDef :: T_TypeAttributeDef  ->
                         Inh_TypeAttributeDef  ->
                         Syn_TypeAttributeDef 
wrap_TypeAttributeDef sem (Inh_TypeAttributeDef _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_TypeAttributeDef _lhsOmessages _lhsOnodeType ))
sem_TypeAttributeDef_TypeAttDef :: String ->
                                   T_TypeName  ->
                                   T_TypeAttributeDef 
sem_TypeAttributeDef_TypeAttDef name_ typ_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _typOinLoop :: Bool
              _typOsourcePos :: MySourcePos
              _typImessages :: ([Message])
              _typInodeType :: Type
              _lhsOmessages =
                  _typImessages
              _lhsOnodeType =
                  _typInodeType
              _typOinLoop =
                  _lhsIinLoop
              _typOsourcePos =
                  _lhsIsourcePos
              ( _typImessages,_typInodeType) =
                  (typ_ _typOinLoop _typOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
-- TypeAttributeDefList ----------------------------------------
type TypeAttributeDefList  = [(TypeAttributeDef)]
-- cata
sem_TypeAttributeDefList :: TypeAttributeDefList  ->
                            T_TypeAttributeDefList 
sem_TypeAttributeDefList list  =
    (Prelude.foldr sem_TypeAttributeDefList_Cons sem_TypeAttributeDefList_Nil (Prelude.map sem_TypeAttributeDef list) )
-- semantic domain
type T_TypeAttributeDefList  = Bool ->
                               MySourcePos ->
                               ( ([Message]),Type)
data Inh_TypeAttributeDefList  = Inh_TypeAttributeDefList {inLoop_Inh_TypeAttributeDefList :: Bool,sourcePos_Inh_TypeAttributeDefList :: MySourcePos}
data Syn_TypeAttributeDefList  = Syn_TypeAttributeDefList {messages_Syn_TypeAttributeDefList :: [Message],nodeType_Syn_TypeAttributeDefList :: Type}
wrap_TypeAttributeDefList :: T_TypeAttributeDefList  ->
                             Inh_TypeAttributeDefList  ->
                             Syn_TypeAttributeDefList 
wrap_TypeAttributeDefList sem (Inh_TypeAttributeDefList _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_TypeAttributeDefList _lhsOmessages _lhsOnodeType ))
sem_TypeAttributeDefList_Cons :: T_TypeAttributeDef  ->
                                 T_TypeAttributeDefList  ->
                                 T_TypeAttributeDefList 
sem_TypeAttributeDefList_Cons hd_ tl_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _hdOinLoop :: Bool
              _hdOsourcePos :: MySourcePos
              _tlOinLoop :: Bool
              _tlOsourcePos :: MySourcePos
              _hdImessages :: ([Message])
              _hdInodeType :: Type
              _tlImessages :: ([Message])
              _tlInodeType :: Type
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _lhsOnodeType =
                  _hdInodeType `appendTypeList` _tlInodeType
              _hdOinLoop =
                  _lhsIinLoop
              _hdOsourcePos =
                  _lhsIsourcePos
              _tlOinLoop =
                  _lhsIinLoop
              _tlOsourcePos =
                  _lhsIsourcePos
              ( _hdImessages,_hdInodeType) =
                  (hd_ _hdOinLoop _hdOsourcePos )
              ( _tlImessages,_tlInodeType) =
                  (tl_ _tlOinLoop _tlOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_TypeAttributeDefList_Nil :: T_TypeAttributeDefList 
sem_TypeAttributeDefList_Nil  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  TypeList []
          in  ( _lhsOmessages,_lhsOnodeType)))
-- TypeErrorInfo -----------------------------------------------
data TypeErrorInfo  = MultipleMatchingKOperators (KeywordOperator) ([Type]) 
                    | MultipleMatchingOperators (String) ([Type]) 
                    | NeedOneOrMoreArgs 
                    | NoMatchingKOperator (KeywordOperator) ([Type]) 
                    | NoMatchingOperator (String) ([Type]) 
                    | NotArrayType (Type) 
                    | OperatorNeeds1Or2Args (Int) 
                    | OtherTypeError (String) 
                    | UnknownTypeError (String) 
                    | WrongNumArgs (Int) (Int) 
                    | WrongType (Type) (Type) 
                    | WrongTypeList ([Type]) ([Type]) 
                    | WrongTypes (Type) ([Type]) 
                    deriving ( Eq,Show)
-- cata
sem_TypeErrorInfo :: TypeErrorInfo  ->
                     T_TypeErrorInfo 
sem_TypeErrorInfo (MultipleMatchingKOperators _o _t )  =
    (sem_TypeErrorInfo_MultipleMatchingKOperators (sem_KeywordOperator _o ) _t )
sem_TypeErrorInfo (MultipleMatchingOperators _o _t )  =
    (sem_TypeErrorInfo_MultipleMatchingOperators _o _t )
sem_TypeErrorInfo (NeedOneOrMoreArgs )  =
    (sem_TypeErrorInfo_NeedOneOrMoreArgs )
sem_TypeErrorInfo (NoMatchingKOperator _o _t )  =
    (sem_TypeErrorInfo_NoMatchingKOperator (sem_KeywordOperator _o ) _t )
sem_TypeErrorInfo (NoMatchingOperator _o _t )  =
    (sem_TypeErrorInfo_NoMatchingOperator _o _t )
sem_TypeErrorInfo (NotArrayType _got )  =
    (sem_TypeErrorInfo_NotArrayType (sem_Type _got ) )
sem_TypeErrorInfo (OperatorNeeds1Or2Args _got )  =
    (sem_TypeErrorInfo_OperatorNeeds1Or2Args _got )
sem_TypeErrorInfo (OtherTypeError _string )  =
    (sem_TypeErrorInfo_OtherTypeError _string )
sem_TypeErrorInfo (UnknownTypeError _string )  =
    (sem_TypeErrorInfo_UnknownTypeError _string )
sem_TypeErrorInfo (WrongNumArgs _expected _got )  =
    (sem_TypeErrorInfo_WrongNumArgs _expected _got )
sem_TypeErrorInfo (WrongType _expected _got )  =
    (sem_TypeErrorInfo_WrongType (sem_Type _expected ) (sem_Type _got ) )
sem_TypeErrorInfo (WrongTypeList _expected _got )  =
    (sem_TypeErrorInfo_WrongTypeList _expected _got )
sem_TypeErrorInfo (WrongTypes _expected _got )  =
    (sem_TypeErrorInfo_WrongTypes (sem_Type _expected ) _got )
-- semantic domain
type T_TypeErrorInfo  = ( )
data Inh_TypeErrorInfo  = Inh_TypeErrorInfo {}
data Syn_TypeErrorInfo  = Syn_TypeErrorInfo {}
wrap_TypeErrorInfo :: T_TypeErrorInfo  ->
                      Inh_TypeErrorInfo  ->
                      Syn_TypeErrorInfo 
wrap_TypeErrorInfo sem (Inh_TypeErrorInfo )  =
    (let ( ) =
             (sem )
     in  (Syn_TypeErrorInfo ))
sem_TypeErrorInfo_MultipleMatchingKOperators :: T_KeywordOperator  ->
                                                ([Type]) ->
                                                T_TypeErrorInfo 
sem_TypeErrorInfo_MultipleMatchingKOperators o_ t_  =
    (let _oOinLoop :: Bool
         _oOsourcePos :: MySourcePos
         _oImessages :: ([Message])
         _oIval :: KeywordOperator
         _oOinLoop =
             error "missing rule: TypeErrorInfo.MultipleMatchingKOperators.o.inLoop"
         _oOsourcePos =
             error "missing rule: TypeErrorInfo.MultipleMatchingKOperators.o.sourcePos"
         ( _oImessages,_oIval) =
             (o_ _oOinLoop _oOsourcePos )
     in  ( ))
sem_TypeErrorInfo_MultipleMatchingOperators :: String ->
                                               ([Type]) ->
                                               T_TypeErrorInfo 
sem_TypeErrorInfo_MultipleMatchingOperators o_ t_  =
    (let 
     in  ( ))
sem_TypeErrorInfo_NeedOneOrMoreArgs :: T_TypeErrorInfo 
sem_TypeErrorInfo_NeedOneOrMoreArgs  =
    (let 
     in  ( ))
sem_TypeErrorInfo_NoMatchingKOperator :: T_KeywordOperator  ->
                                         ([Type]) ->
                                         T_TypeErrorInfo 
sem_TypeErrorInfo_NoMatchingKOperator o_ t_  =
    (let _oOinLoop :: Bool
         _oOsourcePos :: MySourcePos
         _oImessages :: ([Message])
         _oIval :: KeywordOperator
         _oOinLoop =
             error "missing rule: TypeErrorInfo.NoMatchingKOperator.o.inLoop"
         _oOsourcePos =
             error "missing rule: TypeErrorInfo.NoMatchingKOperator.o.sourcePos"
         ( _oImessages,_oIval) =
             (o_ _oOinLoop _oOsourcePos )
     in  ( ))
sem_TypeErrorInfo_NoMatchingOperator :: String ->
                                        ([Type]) ->
                                        T_TypeErrorInfo 
sem_TypeErrorInfo_NoMatchingOperator o_ t_  =
    (let 
     in  ( ))
sem_TypeErrorInfo_NotArrayType :: T_Type  ->
                                  T_TypeErrorInfo 
sem_TypeErrorInfo_NotArrayType got_  =
    (let 
     in  ( ))
sem_TypeErrorInfo_OperatorNeeds1Or2Args :: Int ->
                                           T_TypeErrorInfo 
sem_TypeErrorInfo_OperatorNeeds1Or2Args got_  =
    (let 
     in  ( ))
sem_TypeErrorInfo_OtherTypeError :: String ->
                                    T_TypeErrorInfo 
sem_TypeErrorInfo_OtherTypeError string_  =
    (let 
     in  ( ))
sem_TypeErrorInfo_UnknownTypeError :: String ->
                                      T_TypeErrorInfo 
sem_TypeErrorInfo_UnknownTypeError string_  =
    (let 
     in  ( ))
sem_TypeErrorInfo_WrongNumArgs :: Int ->
                                  Int ->
                                  T_TypeErrorInfo 
sem_TypeErrorInfo_WrongNumArgs expected_ got_  =
    (let 
     in  ( ))
sem_TypeErrorInfo_WrongType :: T_Type  ->
                               T_Type  ->
                               T_TypeErrorInfo 
sem_TypeErrorInfo_WrongType expected_ got_  =
    (let 
     in  ( ))
sem_TypeErrorInfo_WrongTypeList :: ([Type]) ->
                                   ([Type]) ->
                                   T_TypeErrorInfo 
sem_TypeErrorInfo_WrongTypeList expected_ got_  =
    (let 
     in  ( ))
sem_TypeErrorInfo_WrongTypes :: T_Type  ->
                                ([Type]) ->
                                T_TypeErrorInfo 
sem_TypeErrorInfo_WrongTypes expected_ got_  =
    (let 
     in  ( ))
-- TypeName ----------------------------------------------------
data TypeName  = ArrayTypeName (TypeName) 
               | PrecTypeName (String) (Integer) 
               | SetOfTypeName (TypeName) 
               | SimpleTypeName (String) 
               deriving ( Eq,Show)
-- cata
sem_TypeName :: TypeName  ->
                T_TypeName 
sem_TypeName (ArrayTypeName _typ )  =
    (sem_TypeName_ArrayTypeName (sem_TypeName _typ ) )
sem_TypeName (PrecTypeName _tn _prec )  =
    (sem_TypeName_PrecTypeName _tn _prec )
sem_TypeName (SetOfTypeName _typ )  =
    (sem_TypeName_SetOfTypeName (sem_TypeName _typ ) )
sem_TypeName (SimpleTypeName _tn )  =
    (sem_TypeName_SimpleTypeName _tn )
-- semantic domain
type T_TypeName  = Bool ->
                   MySourcePos ->
                   ( ([Message]),Type)
data Inh_TypeName  = Inh_TypeName {inLoop_Inh_TypeName :: Bool,sourcePos_Inh_TypeName :: MySourcePos}
data Syn_TypeName  = Syn_TypeName {messages_Syn_TypeName :: [Message],nodeType_Syn_TypeName :: Type}
wrap_TypeName :: T_TypeName  ->
                 Inh_TypeName  ->
                 Syn_TypeName 
wrap_TypeName sem (Inh_TypeName _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_TypeName _lhsOmessages _lhsOnodeType ))
sem_TypeName_ArrayTypeName :: T_TypeName  ->
                              T_TypeName 
sem_TypeName_ArrayTypeName typ_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOmessages :: ([Message])
              _typOinLoop :: Bool
              _typOsourcePos :: MySourcePos
              _typImessages :: ([Message])
              _typInodeType :: Type
              _lhsOnodeType =
                  propagateUnknownError _typInodeType
                    (ArrayType _typInodeType)
              _lhsOmessages =
                  _typImessages
              _typOinLoop =
                  _lhsIinLoop
              _typOsourcePos =
                  _lhsIsourcePos
              ( _typImessages,_typInodeType) =
                  (typ_ _typOinLoop _typOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_TypeName_PrecTypeName :: String ->
                             Integer ->
                             T_TypeName 
sem_TypeName_PrecTypeName tn_ prec_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_TypeName_SetOfTypeName :: T_TypeName  ->
                              T_TypeName 
sem_TypeName_SetOfTypeName typ_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOmessages :: ([Message])
              _typOinLoop :: Bool
              _typOsourcePos :: MySourcePos
              _typImessages :: ([Message])
              _typInodeType :: Type
              _lhsOnodeType =
                  propagateUnknownError _typInodeType
                    (SetOfType _typInodeType)
              _lhsOmessages =
                  _typImessages
              _typOinLoop =
                  _lhsIinLoop
              _typOsourcePos =
                  _lhsIsourcePos
              ( _typImessages,_typInodeType) =
                  (typ_ _typOinLoop _typOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_TypeName_SimpleTypeName :: String ->
                               T_TypeName 
sem_TypeName_SimpleTypeName tn_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOnodeType :: Type
              _lhsOmessages :: ([Message])
              _lhsOnodeType =
                  let st = canonicalizeType $ ScalarType tn_
                  in if st `elem` defaultTypeNames
                       then st
                       else TypeError _lhsIsourcePos
                                (UnknownTypeError tn_)
              _lhsOmessages =
                  []
          in  ( _lhsOmessages,_lhsOnodeType)))
-- VarDef ------------------------------------------------------
data VarDef  = VarDef (String) (TypeName) (Maybe Expression) 
             deriving ( Eq,Show)
-- cata
sem_VarDef :: VarDef  ->
              T_VarDef 
sem_VarDef (VarDef _name _typ _value )  =
    (sem_VarDef_VarDef _name (sem_TypeName _typ ) _value )
-- semantic domain
type T_VarDef  = Bool ->
                 MySourcePos ->
                 ( ([Message]),Type)
data Inh_VarDef  = Inh_VarDef {inLoop_Inh_VarDef :: Bool,sourcePos_Inh_VarDef :: MySourcePos}
data Syn_VarDef  = Syn_VarDef {messages_Syn_VarDef :: [Message],nodeType_Syn_VarDef :: Type}
wrap_VarDef :: T_VarDef  ->
               Inh_VarDef  ->
               Syn_VarDef 
wrap_VarDef sem (Inh_VarDef _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_VarDef _lhsOmessages _lhsOnodeType ))
sem_VarDef_VarDef :: String ->
                     T_TypeName  ->
                     (Maybe Expression) ->
                     T_VarDef 
sem_VarDef_VarDef name_ typ_ value_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _typOinLoop :: Bool
              _typOsourcePos :: MySourcePos
              _typImessages :: ([Message])
              _typInodeType :: Type
              _lhsOmessages =
                  _typImessages
              _lhsOnodeType =
                  _typInodeType
              _typOinLoop =
                  _lhsIinLoop
              _typOsourcePos =
                  _lhsIsourcePos
              ( _typImessages,_typInodeType) =
                  (typ_ _typOinLoop _typOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
-- VarDefList --------------------------------------------------
type VarDefList  = [(VarDef)]
-- cata
sem_VarDefList :: VarDefList  ->
                  T_VarDefList 
sem_VarDefList list  =
    (Prelude.foldr sem_VarDefList_Cons sem_VarDefList_Nil (Prelude.map sem_VarDef list) )
-- semantic domain
type T_VarDefList  = Bool ->
                     MySourcePos ->
                     ( ([Message]),Type)
data Inh_VarDefList  = Inh_VarDefList {inLoop_Inh_VarDefList :: Bool,sourcePos_Inh_VarDefList :: MySourcePos}
data Syn_VarDefList  = Syn_VarDefList {messages_Syn_VarDefList :: [Message],nodeType_Syn_VarDefList :: Type}
wrap_VarDefList :: T_VarDefList  ->
                   Inh_VarDefList  ->
                   Syn_VarDefList 
wrap_VarDefList sem (Inh_VarDefList _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_VarDefList _lhsOmessages _lhsOnodeType ))
sem_VarDefList_Cons :: T_VarDef  ->
                       T_VarDefList  ->
                       T_VarDefList 
sem_VarDefList_Cons hd_ tl_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _hdOinLoop :: Bool
              _hdOsourcePos :: MySourcePos
              _tlOinLoop :: Bool
              _tlOsourcePos :: MySourcePos
              _hdImessages :: ([Message])
              _hdInodeType :: Type
              _tlImessages :: ([Message])
              _tlInodeType :: Type
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _lhsOnodeType =
                  _hdInodeType `appendTypeList` _tlInodeType
              _hdOinLoop =
                  _lhsIinLoop
              _hdOsourcePos =
                  _lhsIsourcePos
              _tlOinLoop =
                  _lhsIinLoop
              _tlOsourcePos =
                  _lhsIsourcePos
              ( _hdImessages,_hdInodeType) =
                  (hd_ _hdOinLoop _hdOsourcePos )
              ( _tlImessages,_tlInodeType) =
                  (tl_ _tlOinLoop _tlOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_VarDefList_Nil :: T_VarDefList 
sem_VarDefList_Nil  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  TypeList []
          in  ( _lhsOmessages,_lhsOnodeType)))
-- Volatility --------------------------------------------------
data Volatility  = Immutable 
                 | Stable 
                 | Volatile 
                 deriving ( Eq,Show)
-- cata
sem_Volatility :: Volatility  ->
                  T_Volatility 
sem_Volatility (Immutable )  =
    (sem_Volatility_Immutable )
sem_Volatility (Stable )  =
    (sem_Volatility_Stable )
sem_Volatility (Volatile )  =
    (sem_Volatility_Volatile )
-- semantic domain
type T_Volatility  = Bool ->
                     MySourcePos ->
                     ( ([Message]),Type)
data Inh_Volatility  = Inh_Volatility {inLoop_Inh_Volatility :: Bool,sourcePos_Inh_Volatility :: MySourcePos}
data Syn_Volatility  = Syn_Volatility {messages_Syn_Volatility :: [Message],nodeType_Syn_Volatility :: Type}
wrap_Volatility :: T_Volatility  ->
                   Inh_Volatility  ->
                   Syn_Volatility 
wrap_Volatility sem (Inh_Volatility _lhsIinLoop _lhsIsourcePos )  =
    (let ( _lhsOmessages,_lhsOnodeType) =
             (sem _lhsIinLoop _lhsIsourcePos )
     in  (Syn_Volatility _lhsOmessages _lhsOnodeType ))
sem_Volatility_Immutable :: T_Volatility 
sem_Volatility_Immutable  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Volatility_Stable :: T_Volatility 
sem_Volatility_Stable  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Volatility_Volatile :: T_Volatility 
sem_Volatility_Volatile  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
          in  ( _lhsOmessages,_lhsOnodeType)))