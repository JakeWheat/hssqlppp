

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
   ,TypeErrorInfo (..)
   --fns
   ,checkAst
   ,getExpressionType
   ,resetSps
   ,resetSp
   ,resetSp'
   ,resetSps'
   ,nsp
) where



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
isArrayType (AnyArray) = True
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



defaultTypes = [
 "abstime",
 "aclitem",
 "\"any\"",
 "anyarray",
 "anyelement",
 "anyenum",
 "anynonarray",
 "bigint",
 "bit",
 "bit varying",
 "boolean",
 "box",
 "bytea",
 "\"char\"",
 "character",
 "character varying",
 "cid",
 "cidr",
 "circle",
 "cstring",
 "date",
 "double precision",
 "gtsvector",
 "inet",
 "int2vector",
 "integer",
 "internal",
 "interval",
 "language_handler",
 "line",
 "lseg",
 "macaddr",
 "money",
 "name",
 "numeric",
 "oid",
 "oidvector",
 "opaque",
 "path",
 "point",
 "polygon",
 "real",
 "record",
 "refcursor",
 "regclass",
 "regconfig",
 "regdictionary",
 "regoper",
 "regoperator",
 "regproc",
 "regprocedure",
 "regtype",
 "reltime",
 "smallint",
 "smgr",
 "text",
 "tid",
 "timestamp without time zone",
 "timestamp with time zone",
 "time without time zone",
 "time with time zone",
 "tinterval",
 "trigger",
 "tsquery",
 "tsvector",
 "txid_snapshot",
 "unknown",
 "uuid",
 "void",
 "xid",
 "xml"]


allOpsAndFns = binaryOperatorTypes
               ++ prefixOperatorTypes
               ++ postfixOperatorTypes
               ++ functionTypes

--add the keyword operators here

keywordBinaryOperatorTypes = [
 (And, [ScalarType "boolean", ScalarType "boolean"], ScalarType "boolean"),
 (Or, [ScalarType "boolean", ScalarType "boolean"], ScalarType "boolean"),
 (Like, [ScalarType "text", ScalarType "text"], ScalarType "boolean")]
keywordUnaryOperatorTypes = [
 (Not, [ScalarType "boolean"], ScalarType "boolean"),
 (IsNull, [ScalarType "any"], ScalarType "boolean"),
 (IsNotNull, [ScalarType "any"], ScalarType "boolean")]

allKeywordOps = keywordBinaryOperatorTypes ++ keywordUnaryOperatorTypes



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


binaryOperatorTypes = [
    ("!~",[ScalarType "character",ScalarType "text"],ScalarType "boolean"),
    ("!~",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("!~",[ScalarType "name",ScalarType "text"],ScalarType "boolean"),
    ("!~*",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("!~*",[ScalarType "name",ScalarType "text"],ScalarType "boolean"),
    ("!~*",[ScalarType "character",ScalarType "text"],ScalarType "boolean"),
    ("!~~",[ScalarType "character",ScalarType "text"],ScalarType "boolean"),
    ("!~~",[ScalarType "bytea",ScalarType "bytea"],ScalarType "boolean"),
    ("!~~",[ScalarType "name",ScalarType "text"],ScalarType "boolean"),
    ("!~~",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("!~~*",[ScalarType "name",ScalarType "text"],ScalarType "boolean"),
    ("!~~*",[ScalarType "character",ScalarType "text"],ScalarType "boolean"),
    ("!~~*",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("#",[ScalarType "bigint",ScalarType "bigint"],ScalarType "bigint"),
    ("#",[ScalarType "bit",ScalarType "bit"],ScalarType "bit"),
    ("#",[ScalarType "line",ScalarType "line"],ScalarType "point"),
    ("#",[ScalarType "box",ScalarType "box"],ScalarType "box"),
    ("#",[ScalarType "smallint",ScalarType "smallint"],ScalarType "smallint"),
    ("#",[ScalarType "lseg",ScalarType "lseg"],ScalarType "point"),
    ("#",[ScalarType "integer",ScalarType "integer"],ScalarType "integer"),
    ("##",[ScalarType "lseg",ScalarType "box"],ScalarType "point"),
    ("##",[ScalarType "point",ScalarType "lseg"],ScalarType "point"),
    ("##",[ScalarType "point",ScalarType "box"],ScalarType "point"),
    ("##",[ScalarType "lseg",ScalarType "lseg"],ScalarType "point"),
    ("##",[ScalarType "point",ScalarType "line"],ScalarType "point"),
    ("##",[ScalarType "lseg",ScalarType "line"],ScalarType "point"),
    ("##",[ScalarType "line",ScalarType "box"],ScalarType "point"),
    ("##",[ScalarType "line",ScalarType "lseg"],ScalarType "point"),
    ("#<",[ScalarType "tinterval",ScalarType "reltime"],ScalarType "boolean"),
    ("#<=",[ScalarType "tinterval",ScalarType "reltime"],ScalarType "boolean"),
    ("#<>",[ScalarType "tinterval",ScalarType "reltime"],ScalarType "boolean"),
    ("#=",[ScalarType "tinterval",ScalarType "reltime"],ScalarType "boolean"),
    ("#>",[ScalarType "tinterval",ScalarType "reltime"],ScalarType "boolean"),
    ("#>=",[ScalarType "tinterval",ScalarType "reltime"],ScalarType "boolean"),
    ("%",[ScalarType "bigint",ScalarType "bigint"],ScalarType "bigint"),
    ("%",[ScalarType "integer",ScalarType "integer"],ScalarType "integer"),
    ("%",[ScalarType "smallint",ScalarType "smallint"],ScalarType "smallint"),
    ("%",[ScalarType "numeric",ScalarType "numeric"],ScalarType "numeric"),
    ("&",[ScalarType "smallint",ScalarType "smallint"],ScalarType "smallint"),
    ("&",[ScalarType "integer",ScalarType "integer"],ScalarType "integer"),
    ("&",[ScalarType "bigint",ScalarType "bigint"],ScalarType "bigint"),
    ("&",[ScalarType "bit",ScalarType "bit"],ScalarType "bit"),
    ("&",[ScalarType "inet",ScalarType "inet"],ScalarType "inet"),
    ("&&",[ScalarType "circle",ScalarType "circle"],ScalarType "boolean"),
    ("&&",[ScalarType "tinterval",ScalarType "tinterval"],ScalarType "boolean"),
    ("&&",[ScalarType "polygon",ScalarType "polygon"],ScalarType "boolean"),
    ("&&",[ScalarType "tsquery",ScalarType "tsquery"],ScalarType "tsquery"),
    ("&&",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    ("&&",[ScalarType "anyarray",ScalarType "anyarray"],ScalarType "boolean"),
    ("&<",[ScalarType "circle",ScalarType "circle"],ScalarType "boolean"),
    ("&<",[ScalarType "polygon",ScalarType "polygon"],ScalarType "boolean"),
    ("&<",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    ("&<|",[ScalarType "circle",ScalarType "circle"],ScalarType "boolean"),
    ("&<|",[ScalarType "polygon",ScalarType "polygon"],ScalarType "boolean"),
    ("&<|",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    ("&>",[ScalarType "circle",ScalarType "circle"],ScalarType "boolean"),
    ("&>",[ScalarType "polygon",ScalarType "polygon"],ScalarType "boolean"),
    ("&>",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    ("*",[ScalarType "integer",ScalarType "smallint"],ScalarType "integer"),
    ("*",[ScalarType "bigint",ScalarType "bigint"],ScalarType "bigint"),
    ("*",[ScalarType "money",ScalarType "real"],ScalarType "money"),
    ("*",[ScalarType "smallint",ScalarType "bigint"],ScalarType "bigint"),
    ("*",[ScalarType "integer",ScalarType "money"],ScalarType "money"),
    ("*",[ScalarType "double precision",ScalarType "real"],ScalarType "double precision"),
    ("*",[ScalarType "path",ScalarType "point"],ScalarType "path"),
    ("*",[ScalarType "money",ScalarType "integer"],ScalarType "money"),
    ("*",[ScalarType "double precision",ScalarType "money"],ScalarType "money"),
    ("*",[ScalarType "double precision",ScalarType "interval"],ScalarType "interval"),
    ("*",[ScalarType "integer",ScalarType "integer"],ScalarType "integer"),
    ("*",[ScalarType "real",ScalarType "double precision"],ScalarType "double precision"),
    ("*",[ScalarType "real",ScalarType "real"],ScalarType "real"),
    ("*",[ScalarType "bigint",ScalarType "integer"],ScalarType "bigint"),
    ("*",[ScalarType "smallint",ScalarType "money"],ScalarType "money"),
    ("*",[ScalarType "money",ScalarType "smallint"],ScalarType "money"),
    ("*",[ScalarType "point",ScalarType "point"],ScalarType "point"),
    ("*",[ScalarType "circle",ScalarType "point"],ScalarType "circle"),
    ("*",[ScalarType "integer",ScalarType "bigint"],ScalarType "bigint"),
    ("*",[ScalarType "smallint",ScalarType "smallint"],ScalarType "smallint"),
    ("*",[ScalarType "interval",ScalarType "double precision"],ScalarType "interval"),
    ("*",[ScalarType "real",ScalarType "money"],ScalarType "money"),
    ("*",[ScalarType "box",ScalarType "point"],ScalarType "box"),
    ("*",[ScalarType "smallint",ScalarType "integer"],ScalarType "integer"),
    ("*",[ScalarType "double precision",ScalarType "double precision"],ScalarType "double precision"),
    ("*",[ScalarType "numeric",ScalarType "numeric"],ScalarType "numeric"),
    ("*",[ScalarType "bigint",ScalarType "smallint"],ScalarType "bigint"),
    ("*",[ScalarType "money",ScalarType "double precision"],ScalarType "money"),
    ("+",[ScalarType "real",ScalarType "real"],ScalarType "real"),
    ("+",[ScalarType "path",ScalarType "point"],ScalarType "path"),
    ("+",[ScalarType "path",ScalarType "path"],ScalarType "path"),
    ("+",[ScalarType "point",ScalarType "point"],ScalarType "point"),
    ("+",[ScalarType "bigint",ScalarType "bigint"],ScalarType "bigint"),
    ("+",[ScalarType "bigint",ScalarType "integer"],ScalarType "bigint"),
    ("+",[ScalarType "integer",ScalarType "bigint"],ScalarType "bigint"),
    ("+",[ScalarType "bigint",ScalarType "smallint"],ScalarType "bigint"),
    ("+",[ScalarType "smallint",ScalarType "bigint"],ScalarType "bigint"),
    ("+",[ScalarType "date",ScalarType "time with time zone"],ScalarType "timestamp with time zone"),
    ("+",[ScalarType "date",ScalarType "time without time zone"],ScalarType "timestamp without time zone"),
    ("+",[ScalarType "inet",ScalarType "bigint"],ScalarType "inet"),
    ("+",[ScalarType "interval",ScalarType "interval"],ScalarType "interval"),
    ("+",[ScalarType "timestamp with time zone",ScalarType "interval"],ScalarType "timestamp with time zone"),
    ("+",[ScalarType "bigint",ScalarType "inet"],ScalarType "inet"),
    ("+",[ScalarType "numeric",ScalarType "numeric"],ScalarType "numeric"),
    ("+",[ScalarType "double precision",ScalarType "real"],ScalarType "double precision"),
    ("+",[ScalarType "integer",ScalarType "date"],ScalarType "date"),
    ("+",[ScalarType "interval",ScalarType "timestamp with time zone"],ScalarType "timestamp with time zone"),
    ("+",[ScalarType "real",ScalarType "double precision"],ScalarType "double precision"),
    ("+",[ScalarType "interval",ScalarType "timestamp without time zone"],ScalarType "timestamp without time zone"),
    ("+",[ScalarType "interval",ScalarType "time with time zone"],ScalarType "time with time zone"),
    ("+",[ScalarType "time without time zone",ScalarType "interval"],ScalarType "time without time zone"),
    ("+",[ScalarType "interval",ScalarType "date"],ScalarType "timestamp without time zone"),
    ("+",[ScalarType "time with time zone",ScalarType "interval"],ScalarType "time with time zone"),
    ("+",[ScalarType "date",ScalarType "integer"],ScalarType "date"),
    ("+",[ScalarType "smallint",ScalarType "smallint"],ScalarType "smallint"),
    ("+",[ScalarType "integer",ScalarType "integer"],ScalarType "integer"),
    ("+",[ScalarType "smallint",ScalarType "integer"],ScalarType "integer"),
    ("+",[ScalarType "integer",ScalarType "smallint"],ScalarType "integer"),
    ("+",[ScalarType "date",ScalarType "interval"],ScalarType "timestamp without time zone"),
    ("+",[ScalarType "circle",ScalarType "point"],ScalarType "circle"),
    ("+",[ScalarType "interval",ScalarType "time without time zone"],ScalarType "time without time zone"),
    ("+",[ScalarType "time with time zone",ScalarType "date"],ScalarType "timestamp with time zone"),
    ("+",[ScalarType "aclitem[]",ScalarType "aclitem"],ScalarType "aclitem[]"),
    ("+",[ScalarType "abstime",ScalarType "reltime"],ScalarType "abstime"),
    ("+",[ScalarType "timestamp without time zone",ScalarType "interval"],ScalarType "timestamp without time zone"),
    ("+",[ScalarType "double precision",ScalarType "double precision"],ScalarType "double precision"),
    ("+",[ScalarType "time without time zone",ScalarType "date"],ScalarType "timestamp without time zone"),
    ("+",[ScalarType "money",ScalarType "money"],ScalarType "money"),
    ("+",[ScalarType "box",ScalarType "point"],ScalarType "box"),
    ("-",[ScalarType "integer",ScalarType "integer"],ScalarType "integer"),
    ("-",[ScalarType "double precision",ScalarType "double precision"],ScalarType "double precision"),
    ("-",[ScalarType "real",ScalarType "double precision"],ScalarType "double precision"),
    ("-",[ScalarType "double precision",ScalarType "real"],ScalarType "double precision"),
    ("-",[ScalarType "money",ScalarType "money"],ScalarType "money"),
    ("-",[ScalarType "inet",ScalarType "bigint"],ScalarType "inet"),
    ("-",[ScalarType "path",ScalarType "point"],ScalarType "path"),
    ("-",[ScalarType "numeric",ScalarType "numeric"],ScalarType "numeric"),
    ("-",[ScalarType "timestamp with time zone",ScalarType "timestamp with time zone"],ScalarType "interval"),
    ("-",[ScalarType "timestamp with time zone",ScalarType "interval"],ScalarType "timestamp with time zone"),
    ("-",[ScalarType "interval",ScalarType "interval"],ScalarType "interval"),
    ("-",[ScalarType "smallint",ScalarType "bigint"],ScalarType "bigint"),
    ("-",[ScalarType "box",ScalarType "point"],ScalarType "box"),
    ("-",[ScalarType "timestamp without time zone",ScalarType "timestamp without time zone"],ScalarType "interval"),
    ("-",[ScalarType "bigint",ScalarType "smallint"],ScalarType "bigint"),
    ("-",[ScalarType "integer",ScalarType "bigint"],ScalarType "bigint"),
    ("-",[ScalarType "bigint",ScalarType "integer"],ScalarType "bigint"),
    ("-",[ScalarType "date",ScalarType "interval"],ScalarType "timestamp without time zone"),
    ("-",[ScalarType "integer",ScalarType "smallint"],ScalarType "integer"),
    ("-",[ScalarType "time without time zone",ScalarType "time without time zone"],ScalarType "interval"),
    ("-",[ScalarType "aclitem[]",ScalarType "aclitem"],ScalarType "aclitem[]"),
    ("-",[ScalarType "bigint",ScalarType "bigint"],ScalarType "bigint"),
    ("-",[ScalarType "smallint",ScalarType "integer"],ScalarType "integer"),
    ("-",[ScalarType "smallint",ScalarType "smallint"],ScalarType "smallint"),
    ("-",[ScalarType "time with time zone",ScalarType "interval"],ScalarType "time with time zone"),
    ("-",[ScalarType "date",ScalarType "date"],ScalarType "integer"),
    ("-",[ScalarType "date",ScalarType "integer"],ScalarType "date"),
    ("-",[ScalarType "time without time zone",ScalarType "interval"],ScalarType "time without time zone"),
    ("-",[ScalarType "point",ScalarType "point"],ScalarType "point"),
    ("-",[ScalarType "abstime",ScalarType "reltime"],ScalarType "abstime"),
    ("-",[ScalarType "circle",ScalarType "point"],ScalarType "circle"),
    ("-",[ScalarType "real",ScalarType "real"],ScalarType "real"),
    ("-",[ScalarType "inet",ScalarType "inet"],ScalarType "bigint"),
    ("-",[ScalarType "timestamp without time zone",ScalarType "interval"],ScalarType "timestamp without time zone"),
    ("/",[ScalarType "integer",ScalarType "smallint"],ScalarType "integer"),
    ("/",[ScalarType "smallint",ScalarType "integer"],ScalarType "integer"),
    ("/",[ScalarType "circle",ScalarType "point"],ScalarType "circle"),
    ("/",[ScalarType "money",ScalarType "double precision"],ScalarType "money"),
    ("/",[ScalarType "double precision",ScalarType "real"],ScalarType "double precision"),
    ("/",[ScalarType "numeric",ScalarType "numeric"],ScalarType "numeric"),
    ("/",[ScalarType "box",ScalarType "point"],ScalarType "box"),
    ("/",[ScalarType "smallint",ScalarType "bigint"],ScalarType "bigint"),
    ("/",[ScalarType "integer",ScalarType "bigint"],ScalarType "bigint"),
    ("/",[ScalarType "interval",ScalarType "double precision"],ScalarType "interval"),
    ("/",[ScalarType "path",ScalarType "point"],ScalarType "path"),
    ("/",[ScalarType "real",ScalarType "real"],ScalarType "real"),
    ("/",[ScalarType "money",ScalarType "real"],ScalarType "money"),
    ("/",[ScalarType "bigint",ScalarType "integer"],ScalarType "bigint"),
    ("/",[ScalarType "bigint",ScalarType "smallint"],ScalarType "bigint"),
    ("/",[ScalarType "double precision",ScalarType "double precision"],ScalarType "double precision"),
    ("/",[ScalarType "integer",ScalarType "integer"],ScalarType "integer"),
    ("/",[ScalarType "money",ScalarType "smallint"],ScalarType "money"),
    ("/",[ScalarType "smallint",ScalarType "smallint"],ScalarType "smallint"),
    ("/",[ScalarType "bigint",ScalarType "bigint"],ScalarType "bigint"),
    ("/",[ScalarType "point",ScalarType "point"],ScalarType "point"),
    ("/",[ScalarType "real",ScalarType "double precision"],ScalarType "double precision"),
    ("/",[ScalarType "money",ScalarType "integer"],ScalarType "money"),
    ("<",[ScalarType "anyarray",ScalarType "anyarray"],ScalarType "boolean"),
    ("<",[ScalarType "tsvector",ScalarType "tsvector"],ScalarType "boolean"),
    ("<",[ScalarType "inet",ScalarType "inet"],ScalarType "boolean"),
    ("<",[ScalarType "lseg",ScalarType "lseg"],ScalarType "boolean"),
    ("<",[ScalarType "macaddr",ScalarType "macaddr"],ScalarType "boolean"),
    ("<",[ScalarType "bigint",ScalarType "integer"],ScalarType "boolean"),
    ("<",[ScalarType "anyenum",ScalarType "anyenum"],ScalarType "boolean"),
    ("<",[ScalarType "uuid",ScalarType "uuid"],ScalarType "boolean"),
    ("<",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    ("<",[ScalarType "smallint",ScalarType "smallint"],ScalarType "boolean"),
    ("<",[ScalarType "record",ScalarType "record"],ScalarType "boolean"),
    ("<",[ScalarType "smallint",ScalarType "integer"],ScalarType "boolean"),
    ("<",[ScalarType "integer",ScalarType "smallint"],ScalarType "boolean"),
    ("<",[ScalarType "timestamp with time zone",ScalarType "timestamp without time zone"],ScalarType "boolean"),
    ("<",[ScalarType "integer",ScalarType "integer"],ScalarType "boolean"),
    ("<",[ScalarType "abstime",ScalarType "abstime"],ScalarType "boolean"),
    ("<",[ScalarType "boolean",ScalarType "boolean"],ScalarType "boolean"),
    ("<",[ScalarType "reltime",ScalarType "reltime"],ScalarType "boolean"),
    ("<",[ScalarType "timestamp without time zone",ScalarType "timestamp with time zone"],ScalarType "boolean"),
    ("<",[ScalarType "oid",ScalarType "oid"],ScalarType "boolean"),
    ("<",[ScalarType "timestamp with time zone",ScalarType "date"],ScalarType "boolean"),
    ("<",[ScalarType "oidvector",ScalarType "oidvector"],ScalarType "boolean"),
    ("<",[ScalarType "timestamp without time zone",ScalarType "date"],ScalarType "boolean"),
    ("<",[ScalarType "real",ScalarType "real"],ScalarType "boolean"),
    ("<",[ScalarType "\"char\"",ScalarType "\"char\""],ScalarType "boolean"),
    ("<",[ScalarType "date",ScalarType "timestamp with time zone"],ScalarType "boolean"),
    ("<",[ScalarType "date",ScalarType "timestamp without time zone"],ScalarType "boolean"),
    ("<",[ScalarType "name",ScalarType "name"],ScalarType "boolean"),
    ("<",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("<",[ScalarType "timestamp without time zone",ScalarType "timestamp without time zone"],ScalarType "boolean"),
    ("<",[ScalarType "double precision",ScalarType "double precision"],ScalarType "boolean"),
    ("<",[ScalarType "bytea",ScalarType "bytea"],ScalarType "boolean"),
    ("<",[ScalarType "path",ScalarType "path"],ScalarType "boolean"),
    ("<",[ScalarType "tinterval",ScalarType "tinterval"],ScalarType "boolean"),
    ("<",[ScalarType "money",ScalarType "money"],ScalarType "boolean"),
    ("<",[ScalarType "bigint",ScalarType "smallint"],ScalarType "boolean"),
    ("<",[ScalarType "smallint",ScalarType "bigint"],ScalarType "boolean"),
    ("<",[ScalarType "character",ScalarType "character"],ScalarType "boolean"),
    ("<",[ScalarType "tid",ScalarType "tid"],ScalarType "boolean"),
    ("<",[ScalarType "bit varying",ScalarType "bit varying"],ScalarType "boolean"),
    ("<",[ScalarType "tsquery",ScalarType "tsquery"],ScalarType "boolean"),
    ("<",[ScalarType "date",ScalarType "date"],ScalarType "boolean"),
    ("<",[ScalarType "time without time zone",ScalarType "time without time zone"],ScalarType "boolean"),
    ("<",[ScalarType "time with time zone",ScalarType "time with time zone"],ScalarType "boolean"),
    ("<",[ScalarType "integer",ScalarType "bigint"],ScalarType "boolean"),
    ("<",[ScalarType "real",ScalarType "double precision"],ScalarType "boolean"),
    ("<",[ScalarType "double precision",ScalarType "real"],ScalarType "boolean"),
    ("<",[ScalarType "bit",ScalarType "bit"],ScalarType "boolean"),
    ("<",[ScalarType "bigint",ScalarType "bigint"],ScalarType "boolean"),
    ("<",[ScalarType "timestamp with time zone",ScalarType "timestamp with time zone"],ScalarType "boolean"),
    ("<",[ScalarType "numeric",ScalarType "numeric"],ScalarType "boolean"),
    ("<",[ScalarType "interval",ScalarType "interval"],ScalarType "boolean"),
    ("<",[ScalarType "circle",ScalarType "circle"],ScalarType "boolean"),
    ("<#>",[ScalarType "abstime",ScalarType "abstime"],ScalarType "tinterval"),
    ("<->",[ScalarType "point",ScalarType "box"],ScalarType "double precision"),
    ("<->",[ScalarType "lseg",ScalarType "line"],ScalarType "double precision"),
    ("<->",[ScalarType "line",ScalarType "line"],ScalarType "double precision"),
    ("<->",[ScalarType "polygon",ScalarType "polygon"],ScalarType "double precision"),
    ("<->",[ScalarType "path",ScalarType "path"],ScalarType "double precision"),
    ("<->",[ScalarType "box",ScalarType "box"],ScalarType "double precision"),
    ("<->",[ScalarType "circle",ScalarType "circle"],ScalarType "double precision"),
    ("<->",[ScalarType "point",ScalarType "circle"],ScalarType "double precision"),
    ("<->",[ScalarType "circle",ScalarType "polygon"],ScalarType "double precision"),
    ("<->",[ScalarType "line",ScalarType "box"],ScalarType "double precision"),
    ("<->",[ScalarType "point",ScalarType "point"],ScalarType "double precision"),
    ("<->",[ScalarType "point",ScalarType "path"],ScalarType "double precision"),
    ("<->",[ScalarType "lseg",ScalarType "lseg"],ScalarType "double precision"),
    ("<->",[ScalarType "point",ScalarType "line"],ScalarType "double precision"),
    ("<->",[ScalarType "lseg",ScalarType "box"],ScalarType "double precision"),
    ("<->",[ScalarType "point",ScalarType "lseg"],ScalarType "double precision"),
    ("<<",[ScalarType "tinterval",ScalarType "tinterval"],ScalarType "boolean"),
    ("<<",[ScalarType "integer",ScalarType "integer"],ScalarType "integer"),
    ("<<",[ScalarType "smallint",ScalarType "integer"],ScalarType "smallint"),
    ("<<",[ScalarType "bigint",ScalarType "integer"],ScalarType "bigint"),
    ("<<",[ScalarType "circle",ScalarType "circle"],ScalarType "boolean"),
    ("<<",[ScalarType "bit",ScalarType "integer"],ScalarType "bit"),
    ("<<",[ScalarType "inet",ScalarType "inet"],ScalarType "boolean"),
    ("<<",[ScalarType "point",ScalarType "point"],ScalarType "boolean"),
    ("<<",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    ("<<",[ScalarType "polygon",ScalarType "polygon"],ScalarType "boolean"),
    ("<<=",[ScalarType "inet",ScalarType "inet"],ScalarType "boolean"),
    ("<<|",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    ("<<|",[ScalarType "circle",ScalarType "circle"],ScalarType "boolean"),
    ("<<|",[ScalarType "polygon",ScalarType "polygon"],ScalarType "boolean"),
    ("<=",[ScalarType "anyenum",ScalarType "anyenum"],ScalarType "boolean"),
    ("<=",[ScalarType "oidvector",ScalarType "oidvector"],ScalarType "boolean"),
    ("<=",[ScalarType "timestamp without time zone",ScalarType "date"],ScalarType "boolean"),
    ("<=",[ScalarType "oid",ScalarType "oid"],ScalarType "boolean"),
    ("<=",[ScalarType "bytea",ScalarType "bytea"],ScalarType "boolean"),
    ("<=",[ScalarType "timestamp with time zone",ScalarType "date"],ScalarType "boolean"),
    ("<=",[ScalarType "reltime",ScalarType "reltime"],ScalarType "boolean"),
    ("<=",[ScalarType "smallint",ScalarType "bigint"],ScalarType "boolean"),
    ("<=",[ScalarType "bigint",ScalarType "bigint"],ScalarType "boolean"),
    ("<=",[ScalarType "character",ScalarType "character"],ScalarType "boolean"),
    ("<=",[ScalarType "timestamp without time zone",ScalarType "timestamp with time zone"],ScalarType "boolean"),
    ("<=",[ScalarType "abstime",ScalarType "abstime"],ScalarType "boolean"),
    ("<=",[ScalarType "bit varying",ScalarType "bit varying"],ScalarType "boolean"),
    ("<=",[ScalarType "anyarray",ScalarType "anyarray"],ScalarType "boolean"),
    ("<=",[ScalarType "circle",ScalarType "circle"],ScalarType "boolean"),
    ("<=",[ScalarType "date",ScalarType "date"],ScalarType "boolean"),
    ("<=",[ScalarType "timestamp with time zone",ScalarType "timestamp without time zone"],ScalarType "boolean"),
    ("<=",[ScalarType "integer",ScalarType "smallint"],ScalarType "boolean"),
    ("<=",[ScalarType "smallint",ScalarType "integer"],ScalarType "boolean"),
    ("<=",[ScalarType "tid",ScalarType "tid"],ScalarType "boolean"),
    ("<=",[ScalarType "time without time zone",ScalarType "time without time zone"],ScalarType "boolean"),
    ("<=",[ScalarType "time with time zone",ScalarType "time with time zone"],ScalarType "boolean"),
    ("<=",[ScalarType "macaddr",ScalarType "macaddr"],ScalarType "boolean"),
    ("<=",[ScalarType "integer",ScalarType "integer"],ScalarType "boolean"),
    ("<=",[ScalarType "smallint",ScalarType "smallint"],ScalarType "boolean"),
    ("<=",[ScalarType "inet",ScalarType "inet"],ScalarType "boolean"),
    ("<=",[ScalarType "tsquery",ScalarType "tsquery"],ScalarType "boolean"),
    ("<=",[ScalarType "real",ScalarType "double precision"],ScalarType "boolean"),
    ("<=",[ScalarType "bit",ScalarType "bit"],ScalarType "boolean"),
    ("<=",[ScalarType "double precision",ScalarType "real"],ScalarType "boolean"),
    ("<=",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    ("<=",[ScalarType "integer",ScalarType "bigint"],ScalarType "boolean"),
    ("<=",[ScalarType "uuid",ScalarType "uuid"],ScalarType "boolean"),
    ("<=",[ScalarType "lseg",ScalarType "lseg"],ScalarType "boolean"),
    ("<=",[ScalarType "timestamp with time zone",ScalarType "timestamp with time zone"],ScalarType "boolean"),
    ("<=",[ScalarType "numeric",ScalarType "numeric"],ScalarType "boolean"),
    ("<=",[ScalarType "real",ScalarType "real"],ScalarType "boolean"),
    ("<=",[ScalarType "bigint",ScalarType "integer"],ScalarType "boolean"),
    ("<=",[ScalarType "money",ScalarType "money"],ScalarType "boolean"),
    ("<=",[ScalarType "tsvector",ScalarType "tsvector"],ScalarType "boolean"),
    ("<=",[ScalarType "date",ScalarType "timestamp with time zone"],ScalarType "boolean"),
    ("<=",[ScalarType "interval",ScalarType "interval"],ScalarType "boolean"),
    ("<=",[ScalarType "record",ScalarType "record"],ScalarType "boolean"),
    ("<=",[ScalarType "\"char\"",ScalarType "\"char\""],ScalarType "boolean"),
    ("<=",[ScalarType "path",ScalarType "path"],ScalarType "boolean"),
    ("<=",[ScalarType "double precision",ScalarType "double precision"],ScalarType "boolean"),
    ("<=",[ScalarType "timestamp without time zone",ScalarType "timestamp without time zone"],ScalarType "boolean"),
    ("<=",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("<=",[ScalarType "name",ScalarType "name"],ScalarType "boolean"),
    ("<=",[ScalarType "date",ScalarType "timestamp without time zone"],ScalarType "boolean"),
    ("<=",[ScalarType "tinterval",ScalarType "tinterval"],ScalarType "boolean"),
    ("<=",[ScalarType "boolean",ScalarType "boolean"],ScalarType "boolean"),
    ("<=",[ScalarType "bigint",ScalarType "smallint"],ScalarType "boolean"),
    ("<>",[ScalarType "double precision",ScalarType "double precision"],ScalarType "boolean"),
    ("<>",[ScalarType "integer",ScalarType "integer"],ScalarType "boolean"),
    ("<>",[ScalarType "interval",ScalarType "interval"],ScalarType "boolean"),
    ("<>",[ScalarType "real",ScalarType "double precision"],ScalarType "boolean"),
    ("<>",[ScalarType "timestamp with time zone",ScalarType "timestamp with time zone"],ScalarType "boolean"),
    ("<>",[ScalarType "date",ScalarType "timestamp with time zone"],ScalarType "boolean"),
    ("<>",[ScalarType "date",ScalarType "timestamp without time zone"],ScalarType "boolean"),
    ("<>",[ScalarType "integer",ScalarType "smallint"],ScalarType "boolean"),
    ("<>",[ScalarType "name",ScalarType "name"],ScalarType "boolean"),
    ("<>",[ScalarType "time without time zone",ScalarType "time without time zone"],ScalarType "boolean"),
    ("<>",[ScalarType "inet",ScalarType "inet"],ScalarType "boolean"),
    ("<>",[ScalarType "point",ScalarType "point"],ScalarType "boolean"),
    ("<>",[ScalarType "smallint",ScalarType "integer"],ScalarType "boolean"),
    ("<>",[ScalarType "timestamp with time zone",ScalarType "timestamp without time zone"],ScalarType "boolean"),
    ("<>",[ScalarType "tsvector",ScalarType "tsvector"],ScalarType "boolean"),
    ("<>",[ScalarType "time with time zone",ScalarType "time with time zone"],ScalarType "boolean"),
    ("<>",[ScalarType "timestamp without time zone",ScalarType "timestamp without time zone"],ScalarType "boolean"),
    ("<>",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("<>",[ScalarType "bigint",ScalarType "integer"],ScalarType "boolean"),
    ("<>",[ScalarType "smallint",ScalarType "smallint"],ScalarType "boolean"),
    ("<>",[ScalarType "record",ScalarType "record"],ScalarType "boolean"),
    ("<>",[ScalarType "money",ScalarType "money"],ScalarType "boolean"),
    ("<>",[ScalarType "oidvector",ScalarType "oidvector"],ScalarType "boolean"),
    ("<>",[ScalarType "bytea",ScalarType "bytea"],ScalarType "boolean"),
    ("<>",[ScalarType "bigint",ScalarType "bigint"],ScalarType "boolean"),
    ("<>",[ScalarType "double precision",ScalarType "real"],ScalarType "boolean"),
    ("<>",[ScalarType "tsquery",ScalarType "tsquery"],ScalarType "boolean"),
    ("<>",[ScalarType "numeric",ScalarType "numeric"],ScalarType "boolean"),
    ("<>",[ScalarType "lseg",ScalarType "lseg"],ScalarType "boolean"),
    ("<>",[ScalarType "bit",ScalarType "bit"],ScalarType "boolean"),
    ("<>",[ScalarType "oid",ScalarType "oid"],ScalarType "boolean"),
    ("<>",[ScalarType "integer",ScalarType "bigint"],ScalarType "boolean"),
    ("<>",[ScalarType "macaddr",ScalarType "macaddr"],ScalarType "boolean"),
    ("<>",[ScalarType "bigint",ScalarType "smallint"],ScalarType "boolean"),
    ("<>",[ScalarType "timestamp with time zone",ScalarType "date"],ScalarType "boolean"),
    ("<>",[ScalarType "\"char\"",ScalarType "\"char\""],ScalarType "boolean"),
    ("<>",[ScalarType "tid",ScalarType "tid"],ScalarType "boolean"),
    ("<>",[ScalarType "reltime",ScalarType "reltime"],ScalarType "boolean"),
    ("<>",[ScalarType "character",ScalarType "character"],ScalarType "boolean"),
    ("<>",[ScalarType "uuid",ScalarType "uuid"],ScalarType "boolean"),
    ("<>",[ScalarType "smallint",ScalarType "bigint"],ScalarType "boolean"),
    ("<>",[ScalarType "boolean",ScalarType "boolean"],ScalarType "boolean"),
    ("<>",[ScalarType "circle",ScalarType "circle"],ScalarType "boolean"),
    ("<>",[ScalarType "anyarray",ScalarType "anyarray"],ScalarType "boolean"),
    ("<>",[ScalarType "anyenum",ScalarType "anyenum"],ScalarType "boolean"),
    ("<>",[ScalarType "timestamp without time zone",ScalarType "date"],ScalarType "boolean"),
    ("<>",[ScalarType "timestamp without time zone",ScalarType "timestamp with time zone"],ScalarType "boolean"),
    ("<>",[ScalarType "abstime",ScalarType "abstime"],ScalarType "boolean"),
    ("<>",[ScalarType "date",ScalarType "date"],ScalarType "boolean"),
    ("<>",[ScalarType "tinterval",ScalarType "tinterval"],ScalarType "boolean"),
    ("<>",[ScalarType "real",ScalarType "real"],ScalarType "boolean"),
    ("<>",[ScalarType "bit varying",ScalarType "bit varying"],ScalarType "boolean"),
    ("<?>",[ScalarType "abstime",ScalarType "tinterval"],ScalarType "boolean"),
    ("<@",[ScalarType "point",ScalarType "line"],ScalarType "boolean"),
    ("<@",[ScalarType "point",ScalarType "path"],ScalarType "boolean"),
    ("<@",[ScalarType "point",ScalarType "box"],ScalarType "boolean"),
    ("<@",[ScalarType "lseg",ScalarType "line"],ScalarType "boolean"),
    ("<@",[ScalarType "point",ScalarType "lseg"],ScalarType "boolean"),
    ("<@",[ScalarType "lseg",ScalarType "box"],ScalarType "boolean"),
    ("<@",[ScalarType "point",ScalarType "circle"],ScalarType "boolean"),
    ("<@",[ScalarType "point",ScalarType "polygon"],ScalarType "boolean"),
    ("<@",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    ("<@",[ScalarType "polygon",ScalarType "polygon"],ScalarType "boolean"),
    ("<@",[ScalarType "anyarray",ScalarType "anyarray"],ScalarType "boolean"),
    ("<@",[ScalarType "tsquery",ScalarType "tsquery"],ScalarType "boolean"),
    ("<@",[ScalarType "circle",ScalarType "circle"],ScalarType "boolean"),
    ("<^",[ScalarType "point",ScalarType "point"],ScalarType "boolean"),
    ("<^",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    ("=",[ScalarType "integer",ScalarType "bigint"],ScalarType "boolean"),
    ("=",[ScalarType "boolean",ScalarType "boolean"],ScalarType "boolean"),
    ("=",[ScalarType "\"char\"",ScalarType "\"char\""],ScalarType "boolean"),
    ("=",[ScalarType "name",ScalarType "name"],ScalarType "boolean"),
    ("=",[ScalarType "smallint",ScalarType "smallint"],ScalarType "boolean"),
    ("=",[ScalarType "integer",ScalarType "integer"],ScalarType "boolean"),
    ("=",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("=",[ScalarType "xid",ScalarType "xid"],ScalarType "boolean"),
    ("=",[ScalarType "xid",ScalarType "integer"],ScalarType "boolean"),
    ("=",[ScalarType "cid",ScalarType "cid"],ScalarType "boolean"),
    ("=",[ScalarType "int2vector",ScalarType "int2vector"],ScalarType "boolean"),
    ("=",[ScalarType "tid",ScalarType "tid"],ScalarType "boolean"),
    ("=",[ScalarType "bigint",ScalarType "bigint"],ScalarType "boolean"),
    ("=",[ScalarType "bigint",ScalarType "integer"],ScalarType "boolean"),
    ("=",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    ("=",[ScalarType "smallint",ScalarType "integer"],ScalarType "boolean"),
    ("=",[ScalarType "integer",ScalarType "smallint"],ScalarType "boolean"),
    ("=",[ScalarType "abstime",ScalarType "abstime"],ScalarType "boolean"),
    ("=",[ScalarType "reltime",ScalarType "reltime"],ScalarType "boolean"),
    ("=",[ScalarType "oid",ScalarType "oid"],ScalarType "boolean"),
    ("=",[ScalarType "oidvector",ScalarType "oidvector"],ScalarType "boolean"),
    ("=",[ScalarType "real",ScalarType "real"],ScalarType "boolean"),
    ("=",[ScalarType "double precision",ScalarType "double precision"],ScalarType "boolean"),
    ("=",[ScalarType "path",ScalarType "path"],ScalarType "boolean"),
    ("=",[ScalarType "tinterval",ScalarType "tinterval"],ScalarType "boolean"),
    ("=",[ScalarType "money",ScalarType "money"],ScalarType "boolean"),
    ("=",[ScalarType "aclitem",ScalarType "aclitem"],ScalarType "boolean"),
    ("=",[ScalarType "character",ScalarType "character"],ScalarType "boolean"),
    ("=",[ScalarType "anyarray",ScalarType "anyarray"],ScalarType "boolean"),
    ("=",[ScalarType "date",ScalarType "date"],ScalarType "boolean"),
    ("=",[ScalarType "time without time zone",ScalarType "time without time zone"],ScalarType "boolean"),
    ("=",[ScalarType "time with time zone",ScalarType "time with time zone"],ScalarType "boolean"),
    ("=",[ScalarType "real",ScalarType "double precision"],ScalarType "boolean"),
    ("=",[ScalarType "double precision",ScalarType "real"],ScalarType "boolean"),
    ("=",[ScalarType "timestamp with time zone",ScalarType "timestamp with time zone"],ScalarType "boolean"),
    ("=",[ScalarType "interval",ScalarType "interval"],ScalarType "boolean"),
    ("=",[ScalarType "circle",ScalarType "circle"],ScalarType "boolean"),
    ("=",[ScalarType "lseg",ScalarType "lseg"],ScalarType "boolean"),
    ("=",[ScalarType "line",ScalarType "line"],ScalarType "boolean"),
    ("=",[ScalarType "macaddr",ScalarType "macaddr"],ScalarType "boolean"),
    ("=",[ScalarType "inet",ScalarType "inet"],ScalarType "boolean"),
    ("=",[ScalarType "numeric",ScalarType "numeric"],ScalarType "boolean"),
    ("=",[ScalarType "bit",ScalarType "bit"],ScalarType "boolean"),
    ("=",[ScalarType "bit varying",ScalarType "bit varying"],ScalarType "boolean"),
    ("=",[ScalarType "smallint",ScalarType "bigint"],ScalarType "boolean"),
    ("=",[ScalarType "bigint",ScalarType "smallint"],ScalarType "boolean"),
    ("=",[ScalarType "bytea",ScalarType "bytea"],ScalarType "boolean"),
    ("=",[ScalarType "timestamp without time zone",ScalarType "timestamp without time zone"],ScalarType "boolean"),
    ("=",[ScalarType "date",ScalarType "timestamp without time zone"],ScalarType "boolean"),
    ("=",[ScalarType "date",ScalarType "timestamp with time zone"],ScalarType "boolean"),
    ("=",[ScalarType "timestamp without time zone",ScalarType "date"],ScalarType "boolean"),
    ("=",[ScalarType "timestamp with time zone",ScalarType "date"],ScalarType "boolean"),
    ("=",[ScalarType "timestamp without time zone",ScalarType "timestamp with time zone"],ScalarType "boolean"),
    ("=",[ScalarType "timestamp with time zone",ScalarType "timestamp without time zone"],ScalarType "boolean"),
    ("=",[ScalarType "uuid",ScalarType "uuid"],ScalarType "boolean"),
    ("=",[ScalarType "anyenum",ScalarType "anyenum"],ScalarType "boolean"),
    ("=",[ScalarType "tsvector",ScalarType "tsvector"],ScalarType "boolean"),
    ("=",[ScalarType "tsquery",ScalarType "tsquery"],ScalarType "boolean"),
    ("=",[ScalarType "record",ScalarType "record"],ScalarType "boolean"),
    (">",[ScalarType "inet",ScalarType "inet"],ScalarType "boolean"),
    (">",[ScalarType "bigint",ScalarType "smallint"],ScalarType "boolean"),
    (">",[ScalarType "money",ScalarType "money"],ScalarType "boolean"),
    (">",[ScalarType "reltime",ScalarType "reltime"],ScalarType "boolean"),
    (">",[ScalarType "timestamp with time zone",ScalarType "date"],ScalarType "boolean"),
    (">",[ScalarType "tsvector",ScalarType "tsvector"],ScalarType "boolean"),
    (">",[ScalarType "tinterval",ScalarType "tinterval"],ScalarType "boolean"),
    (">",[ScalarType "date",ScalarType "timestamp without time zone"],ScalarType "boolean"),
    (">",[ScalarType "boolean",ScalarType "boolean"],ScalarType "boolean"),
    (">",[ScalarType "uuid",ScalarType "uuid"],ScalarType "boolean"),
    (">",[ScalarType "oid",ScalarType "oid"],ScalarType "boolean"),
    (">",[ScalarType "anyenum",ScalarType "anyenum"],ScalarType "boolean"),
    (">",[ScalarType "bigint",ScalarType "integer"],ScalarType "boolean"),
    (">",[ScalarType "name",ScalarType "name"],ScalarType "boolean"),
    (">",[ScalarType "timestamp without time zone",ScalarType "timestamp without time zone"],ScalarType "boolean"),
    (">",[ScalarType "smallint",ScalarType "smallint"],ScalarType "boolean"),
    (">",[ScalarType "macaddr",ScalarType "macaddr"],ScalarType "boolean"),
    (">",[ScalarType "path",ScalarType "path"],ScalarType "boolean"),
    (">",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    (">",[ScalarType "tsquery",ScalarType "tsquery"],ScalarType "boolean"),
    (">",[ScalarType "double precision",ScalarType "double precision"],ScalarType "boolean"),
    (">",[ScalarType "bigint",ScalarType "bigint"],ScalarType "boolean"),
    (">",[ScalarType "integer",ScalarType "smallint"],ScalarType "boolean"),
    (">",[ScalarType "bytea",ScalarType "bytea"],ScalarType "boolean"),
    (">",[ScalarType "bit",ScalarType "bit"],ScalarType "boolean"),
    (">",[ScalarType "record",ScalarType "record"],ScalarType "boolean"),
    (">",[ScalarType "double precision",ScalarType "real"],ScalarType "boolean"),
    (">",[ScalarType "circle",ScalarType "circle"],ScalarType "boolean"),
    (">",[ScalarType "timestamp with time zone",ScalarType "timestamp without time zone"],ScalarType "boolean"),
    (">",[ScalarType "lseg",ScalarType "lseg"],ScalarType "boolean"),
    (">",[ScalarType "real",ScalarType "double precision"],ScalarType "boolean"),
    (">",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    (">",[ScalarType "timestamp without time zone",ScalarType "date"],ScalarType "boolean"),
    (">",[ScalarType "tid",ScalarType "tid"],ScalarType "boolean"),
    (">",[ScalarType "time with time zone",ScalarType "time with time zone"],ScalarType "boolean"),
    (">",[ScalarType "interval",ScalarType "interval"],ScalarType "boolean"),
    (">",[ScalarType "time without time zone",ScalarType "time without time zone"],ScalarType "boolean"),
    (">",[ScalarType "integer",ScalarType "integer"],ScalarType "boolean"),
    (">",[ScalarType "integer",ScalarType "bigint"],ScalarType "boolean"),
    (">",[ScalarType "date",ScalarType "date"],ScalarType "boolean"),
    (">",[ScalarType "timestamp with time zone",ScalarType "timestamp with time zone"],ScalarType "boolean"),
    (">",[ScalarType "bit varying",ScalarType "bit varying"],ScalarType "boolean"),
    (">",[ScalarType "anyarray",ScalarType "anyarray"],ScalarType "boolean"),
    (">",[ScalarType "numeric",ScalarType "numeric"],ScalarType "boolean"),
    (">",[ScalarType "smallint",ScalarType "integer"],ScalarType "boolean"),
    (">",[ScalarType "oidvector",ScalarType "oidvector"],ScalarType "boolean"),
    (">",[ScalarType "character",ScalarType "character"],ScalarType "boolean"),
    (">",[ScalarType "real",ScalarType "real"],ScalarType "boolean"),
    (">",[ScalarType "smallint",ScalarType "bigint"],ScalarType "boolean"),
    (">",[ScalarType "date",ScalarType "timestamp with time zone"],ScalarType "boolean"),
    (">",[ScalarType "abstime",ScalarType "abstime"],ScalarType "boolean"),
    (">",[ScalarType "timestamp without time zone",ScalarType "timestamp with time zone"],ScalarType "boolean"),
    (">",[ScalarType "\"char\"",ScalarType "\"char\""],ScalarType "boolean"),
    (">=",[ScalarType "inet",ScalarType "inet"],ScalarType "boolean"),
    (">=",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    (">=",[ScalarType "interval",ScalarType "interval"],ScalarType "boolean"),
    (">=",[ScalarType "timestamp with time zone",ScalarType "timestamp with time zone"],ScalarType "boolean"),
    (">=",[ScalarType "numeric",ScalarType "numeric"],ScalarType "boolean"),
    (">=",[ScalarType "macaddr",ScalarType "macaddr"],ScalarType "boolean"),
    (">=",[ScalarType "double precision",ScalarType "real"],ScalarType "boolean"),
    (">=",[ScalarType "bit",ScalarType "bit"],ScalarType "boolean"),
    (">=",[ScalarType "real",ScalarType "double precision"],ScalarType "boolean"),
    (">=",[ScalarType "time with time zone",ScalarType "time with time zone"],ScalarType "boolean"),
    (">=",[ScalarType "lseg",ScalarType "lseg"],ScalarType "boolean"),
    (">=",[ScalarType "time without time zone",ScalarType "time without time zone"],ScalarType "boolean"),
    (">=",[ScalarType "date",ScalarType "date"],ScalarType "boolean"),
    (">=",[ScalarType "anyarray",ScalarType "anyarray"],ScalarType "boolean"),
    (">=",[ScalarType "bit varying",ScalarType "bit varying"],ScalarType "boolean"),
    (">=",[ScalarType "character",ScalarType "character"],ScalarType "boolean"),
    (">=",[ScalarType "smallint",ScalarType "bigint"],ScalarType "boolean"),
    (">=",[ScalarType "tsquery",ScalarType "tsquery"],ScalarType "boolean"),
    (">=",[ScalarType "money",ScalarType "money"],ScalarType "boolean"),
    (">=",[ScalarType "bigint",ScalarType "smallint"],ScalarType "boolean"),
    (">=",[ScalarType "tinterval",ScalarType "tinterval"],ScalarType "boolean"),
    (">=",[ScalarType "tsvector",ScalarType "tsvector"],ScalarType "boolean"),
    (">=",[ScalarType "tid",ScalarType "tid"],ScalarType "boolean"),
    (">=",[ScalarType "path",ScalarType "path"],ScalarType "boolean"),
    (">=",[ScalarType "circle",ScalarType "circle"],ScalarType "boolean"),
    (">=",[ScalarType "bytea",ScalarType "bytea"],ScalarType "boolean"),
    (">=",[ScalarType "double precision",ScalarType "double precision"],ScalarType "boolean"),
    (">=",[ScalarType "bigint",ScalarType "bigint"],ScalarType "boolean"),
    (">=",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    (">=",[ScalarType "timestamp without time zone",ScalarType "timestamp without time zone"],ScalarType "boolean"),
    (">=",[ScalarType "name",ScalarType "name"],ScalarType "boolean"),
    (">=",[ScalarType "anyenum",ScalarType "anyenum"],ScalarType "boolean"),
    (">=",[ScalarType "bigint",ScalarType "integer"],ScalarType "boolean"),
    (">=",[ScalarType "integer",ScalarType "bigint"],ScalarType "boolean"),
    (">=",[ScalarType "uuid",ScalarType "uuid"],ScalarType "boolean"),
    (">=",[ScalarType "date",ScalarType "timestamp without time zone"],ScalarType "boolean"),
    (">=",[ScalarType "\"char\"",ScalarType "\"char\""],ScalarType "boolean"),
    (">=",[ScalarType "real",ScalarType "real"],ScalarType "boolean"),
    (">=",[ScalarType "date",ScalarType "timestamp with time zone"],ScalarType "boolean"),
    (">=",[ScalarType "oidvector",ScalarType "oidvector"],ScalarType "boolean"),
    (">=",[ScalarType "boolean",ScalarType "boolean"],ScalarType "boolean"),
    (">=",[ScalarType "timestamp without time zone",ScalarType "date"],ScalarType "boolean"),
    (">=",[ScalarType "oid",ScalarType "oid"],ScalarType "boolean"),
    (">=",[ScalarType "timestamp with time zone",ScalarType "date"],ScalarType "boolean"),
    (">=",[ScalarType "reltime",ScalarType "reltime"],ScalarType "boolean"),
    (">=",[ScalarType "abstime",ScalarType "abstime"],ScalarType "boolean"),
    (">=",[ScalarType "timestamp without time zone",ScalarType "timestamp with time zone"],ScalarType "boolean"),
    (">=",[ScalarType "integer",ScalarType "smallint"],ScalarType "boolean"),
    (">=",[ScalarType "smallint",ScalarType "integer"],ScalarType "boolean"),
    (">=",[ScalarType "record",ScalarType "record"],ScalarType "boolean"),
    (">=",[ScalarType "timestamp with time zone",ScalarType "timestamp without time zone"],ScalarType "boolean"),
    (">=",[ScalarType "integer",ScalarType "integer"],ScalarType "boolean"),
    (">=",[ScalarType "smallint",ScalarType "smallint"],ScalarType "boolean"),
    (">>",[ScalarType "polygon",ScalarType "polygon"],ScalarType "boolean"),
    (">>",[ScalarType "bigint",ScalarType "integer"],ScalarType "bigint"),
    (">>",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    (">>",[ScalarType "smallint",ScalarType "integer"],ScalarType "smallint"),
    (">>",[ScalarType "point",ScalarType "point"],ScalarType "boolean"),
    (">>",[ScalarType "integer",ScalarType "integer"],ScalarType "integer"),
    (">>",[ScalarType "circle",ScalarType "circle"],ScalarType "boolean"),
    (">>",[ScalarType "bit",ScalarType "integer"],ScalarType "bit"),
    (">>",[ScalarType "inet",ScalarType "inet"],ScalarType "boolean"),
    (">>=",[ScalarType "inet",ScalarType "inet"],ScalarType "boolean"),
    (">^",[ScalarType "point",ScalarType "point"],ScalarType "boolean"),
    (">^",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    ("?#",[ScalarType "lseg",ScalarType "line"],ScalarType "boolean"),
    ("?#",[ScalarType "line",ScalarType "box"],ScalarType "boolean"),
    ("?#",[ScalarType "path",ScalarType "path"],ScalarType "boolean"),
    ("?#",[ScalarType "line",ScalarType "line"],ScalarType "boolean"),
    ("?#",[ScalarType "lseg",ScalarType "box"],ScalarType "boolean"),
    ("?#",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    ("?#",[ScalarType "lseg",ScalarType "lseg"],ScalarType "boolean"),
    ("?-",[ScalarType "point",ScalarType "point"],ScalarType "boolean"),
    ("?-|",[ScalarType "lseg",ScalarType "lseg"],ScalarType "boolean"),
    ("?-|",[ScalarType "line",ScalarType "line"],ScalarType "boolean"),
    ("?|",[ScalarType "point",ScalarType "point"],ScalarType "boolean"),
    ("?||",[ScalarType "line",ScalarType "line"],ScalarType "boolean"),
    ("?||",[ScalarType "lseg",ScalarType "lseg"],ScalarType "boolean"),
    -- ("@",[ScalarType "circle",ScalarType "circle"],ScalarType "boolean"),
    -- ("@",[ScalarType "point",ScalarType "box"],ScalarType "boolean"),
    -- ("@",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    -- ("@",[ScalarType "point",ScalarType "path"],ScalarType "boolean"),
    -- ("@",[ScalarType "point",ScalarType "line"],ScalarType "boolean"),
    -- ("@",[ScalarType "point",ScalarType "circle"],ScalarType "boolean"),
    -- ("@",[ScalarType "polygon",ScalarType "polygon"],ScalarType "boolean"),
    -- ("@",[ScalarType "lseg",ScalarType "box"],ScalarType "boolean"),
    -- ("@",[ScalarType "lseg",ScalarType "line"],ScalarType "boolean"),
    -- ("@",[ScalarType "point",ScalarType "polygon"],ScalarType "boolean"),
    -- ("@",[ScalarType "point",ScalarType "lseg"],ScalarType "boolean"),
    ("@>",[ScalarType "tsquery",ScalarType "tsquery"],ScalarType "boolean"),
    ("@>",[ScalarType "circle",ScalarType "circle"],ScalarType "boolean"),
    ("@>",[ScalarType "aclitem[]",ScalarType "aclitem"],ScalarType "boolean"),
    ("@>",[ScalarType "circle",ScalarType "point"],ScalarType "boolean"),
    ("@>",[ScalarType "polygon",ScalarType "point"],ScalarType "boolean"),
    ("@>",[ScalarType "path",ScalarType "point"],ScalarType "boolean"),
    ("@>",[ScalarType "anyarray",ScalarType "anyarray"],ScalarType "boolean"),
    ("@>",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    ("@>",[ScalarType "polygon",ScalarType "polygon"],ScalarType "boolean"),
    ("@@",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("@@",[ScalarType "tsquery",ScalarType "tsvector"],ScalarType "boolean"),
    ("@@",[ScalarType "text",ScalarType "tsquery"],ScalarType "boolean"),
    ("@@",[ScalarType "tsvector",ScalarType "tsquery"],ScalarType "boolean"),
    ("@@@",[ScalarType "tsvector",ScalarType "tsquery"],ScalarType "boolean"),
    ("@@@",[ScalarType "tsquery",ScalarType "tsvector"],ScalarType "boolean"),
    ("^",[ScalarType "double precision",ScalarType "double precision"],ScalarType "double precision"),
    ("^",[ScalarType "numeric",ScalarType "numeric"],ScalarType "numeric"),
    ("|",[ScalarType "bigint",ScalarType "bigint"],ScalarType "bigint"),
    ("|",[ScalarType "integer",ScalarType "integer"],ScalarType "integer"),
    ("|",[ScalarType "bit",ScalarType "bit"],ScalarType "bit"),
    ("|",[ScalarType "inet",ScalarType "inet"],ScalarType "inet"),
    ("|",[ScalarType "smallint",ScalarType "smallint"],ScalarType "smallint"),
    ("|&>",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    ("|&>",[ScalarType "polygon",ScalarType "polygon"],ScalarType "boolean"),
    ("|&>",[ScalarType "circle",ScalarType "circle"],ScalarType "boolean"),
    ("|>>",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    ("|>>",[ScalarType "circle",ScalarType "circle"],ScalarType "boolean"),
    ("|>>",[ScalarType "polygon",ScalarType "polygon"],ScalarType "boolean"),
    ("||",[ScalarType "tsvector",ScalarType "tsvector"],ScalarType "tsvector"),
    ("||",[ScalarType "text",ScalarType "anynonarray"],ScalarType "text"),
    ("||",[ScalarType "anynonarray",ScalarType "text"],ScalarType "text"),
    ("||",[ScalarType "tsquery",ScalarType "tsquery"],ScalarType "tsquery"),
    ("||",[ScalarType "anyarray",ScalarType "anyarray"],ScalarType "anyarray"),
    ("||",[ScalarType "bit varying",ScalarType "bit varying"],ScalarType "bit varying"),
    ("||",[ScalarType "anyelement",ScalarType "anyarray"],ScalarType "anyarray"),
    ("||",[ScalarType "text",ScalarType "text"],ScalarType "text"),
    ("||",[ScalarType "anyarray",ScalarType "anyelement"],ScalarType "anyarray"),
    ("||",[ScalarType "bytea",ScalarType "bytea"],ScalarType "bytea"),
    ("~",[ScalarType "character",ScalarType "text"],ScalarType "boolean"),
    ("~",[ScalarType "circle",ScalarType "point"],ScalarType "boolean"),
    ("~",[ScalarType "polygon",ScalarType "point"],ScalarType "boolean"),
    ("~",[ScalarType "circle",ScalarType "circle"],ScalarType "boolean"),
    ("~",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    ("~",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("~",[ScalarType "aclitem[]",ScalarType "aclitem"],ScalarType "boolean"),
    ("~",[ScalarType "polygon",ScalarType "polygon"],ScalarType "boolean"),
    ("~",[ScalarType "path",ScalarType "point"],ScalarType "boolean"),
    ("~",[ScalarType "name",ScalarType "text"],ScalarType "boolean"),
    ("~*",[ScalarType "name",ScalarType "text"],ScalarType "boolean"),
    ("~*",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("~*",[ScalarType "character",ScalarType "text"],ScalarType "boolean"),
    ("~<=~",[ScalarType "character",ScalarType "character"],ScalarType "boolean"),
    ("~<=~",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("~<~",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("~<~",[ScalarType "character",ScalarType "character"],ScalarType "boolean"),
    ("~=",[ScalarType "tinterval",ScalarType "tinterval"],ScalarType "boolean"),
    ("~=",[ScalarType "circle",ScalarType "circle"],ScalarType "boolean"),
    ("~=",[ScalarType "point",ScalarType "point"],ScalarType "boolean"),
    ("~=",[ScalarType "polygon",ScalarType "polygon"],ScalarType "boolean"),
    ("~=",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    ("~>=~",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("~>=~",[ScalarType "character",ScalarType "character"],ScalarType "boolean"),
    ("~>~",[ScalarType "character",ScalarType "character"],ScalarType "boolean"),
    ("~>~",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("~~",[ScalarType "character",ScalarType "text"],ScalarType "boolean"),
    ("~~",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("~~",[ScalarType "name",ScalarType "text"],ScalarType "boolean"),
    ("~~",[ScalarType "bytea",ScalarType "bytea"],ScalarType "boolean"),
    ("~~*",[ScalarType "name",ScalarType "text"],ScalarType "boolean"),
    ("~~*",[ScalarType "character",ScalarType "text"],ScalarType "boolean"),
    ("~~*",[ScalarType "text",ScalarType "text"],ScalarType "boolean")
    ]
prefixOperatorTypes = [
    ("!!",[ScalarType "tsquery"],ScalarType "tsquery"),
    ("!!",[ScalarType "bigint"],ScalarType "numeric"),
    ("#",[ScalarType "polygon"],ScalarType "integer"),
    ("#",[ScalarType "path"],ScalarType "integer"),
    ("+",[ScalarType "double precision"],ScalarType "double precision"),
    ("+",[ScalarType "integer"],ScalarType "integer"),
    ("+",[ScalarType "real"],ScalarType "real"),
    ("+",[ScalarType "smallint"],ScalarType "smallint"),
    ("+",[ScalarType "bigint"],ScalarType "bigint"),
    ("+",[ScalarType "numeric"],ScalarType "numeric"),
    ("-",[ScalarType "smallint"],ScalarType "smallint"),
    ("-",[ScalarType "bigint"],ScalarType "bigint"),
    ("-",[ScalarType "integer"],ScalarType "integer"),
    ("-",[ScalarType "real"],ScalarType "real"),
    ("-",[ScalarType "double precision"],ScalarType "double precision"),
    ("-",[ScalarType "interval"],ScalarType "interval"),
    ("-",[ScalarType "numeric"],ScalarType "numeric"),
    ("?-",[ScalarType "lseg"],ScalarType "boolean"),
    ("?-",[ScalarType "line"],ScalarType "boolean"),
    ("?|",[ScalarType "line"],ScalarType "boolean"),
    ("?|",[ScalarType "lseg"],ScalarType "boolean"),
    ("@",[ScalarType "numeric"],ScalarType "numeric"),
    ("@",[ScalarType "smallint"],ScalarType "smallint"),
    ("@",[ScalarType "real"],ScalarType "real"),
    ("@",[ScalarType "bigint"],ScalarType "bigint"),
    ("@",[ScalarType "integer"],ScalarType "integer"),
    ("@",[ScalarType "double precision"],ScalarType "double precision"),
    ("@-@",[ScalarType "path"],ScalarType "double precision"),
    ("@-@",[ScalarType "lseg"],ScalarType "double precision"),
    ("@@",[ScalarType "box"],ScalarType "point"),
    ("@@",[ScalarType "circle"],ScalarType "point"),
    ("@@",[ScalarType "lseg"],ScalarType "point"),
    ("@@",[ScalarType "path"],ScalarType "point"),
    ("@@",[ScalarType "polygon"],ScalarType "point"),
    ("|",[ScalarType "tinterval"],ScalarType "abstime"),
    ("|/",[ScalarType "double precision"],ScalarType "double precision"),
    ("||/",[ScalarType "double precision"],ScalarType "double precision"),
    ("~",[ScalarType "integer"],ScalarType "integer"),
    ("~",[ScalarType "smallint"],ScalarType "smallint"),
    ("~",[ScalarType "bit"],ScalarType "bit"),
    ("~",[ScalarType "bigint"],ScalarType "bigint"),
    ("~",[ScalarType "inet"],ScalarType "inet")
    ]
postfixOperatorTypes = [
    ("!",[ScalarType "bigint"],ScalarType "numeric")
    ]
functionTypes = [
    ("abbrev",[ScalarType "cidr"],ScalarType "text"),
    ("abbrev",[ScalarType "inet"],ScalarType "text"),
    ("abs",[ScalarType "numeric"],ScalarType "numeric"),
    ("abs",[ScalarType "double precision"],ScalarType "double precision"),
    ("abs",[ScalarType "integer"],ScalarType "integer"),
    ("abs",[ScalarType "smallint"],ScalarType "smallint"),
    ("abs",[ScalarType "real"],ScalarType "real"),
    ("abs",[ScalarType "bigint"],ScalarType "bigint"),
    ("abstime",[ScalarType "timestamp without time zone"],ScalarType "abstime"),
    ("abstime",[ScalarType "timestamp with time zone"],ScalarType "abstime"),
    ("abstimeeq",[ScalarType "abstime",ScalarType "abstime"],ScalarType "boolean"),
    ("abstimege",[ScalarType "abstime",ScalarType "abstime"],ScalarType "boolean"),
    ("abstimegt",[ScalarType "abstime",ScalarType "abstime"],ScalarType "boolean"),
    ("abstimein",[ScalarType "cstring"],ScalarType "abstime"),
    ("abstimele",[ScalarType "abstime",ScalarType "abstime"],ScalarType "boolean"),
    ("abstimelt",[ScalarType "abstime",ScalarType "abstime"],ScalarType "boolean"),
    ("abstimene",[ScalarType "abstime",ScalarType "abstime"],ScalarType "boolean"),
    ("abstimeout",[ScalarType "abstime"],ScalarType "cstring"),
    ("abstimerecv",[ScalarType "internal"],ScalarType "abstime"),
    ("abstimesend",[ScalarType "abstime"],ScalarType "bytea"),
    ("aclcontains",[ScalarType "aclitem[]",ScalarType "aclitem"],ScalarType "boolean"),
    ("aclinsert",[ScalarType "aclitem[]",ScalarType "aclitem"],ScalarType "aclitem[]"),
    ("aclitemeq",[ScalarType "aclitem",ScalarType "aclitem"],ScalarType "boolean"),
    ("aclitemin",[ScalarType "cstring"],ScalarType "aclitem"),
    ("aclitemout",[ScalarType "aclitem"],ScalarType "cstring"),
    ("aclremove",[ScalarType "aclitem[]",ScalarType "aclitem"],ScalarType "aclitem[]"),
    ("acos",[ScalarType "double precision"],ScalarType "double precision"),
    ("age",[ScalarType "timestamp with time zone",ScalarType "timestamp with time zone"],ScalarType "interval"),
    ("age",[ScalarType "timestamp with time zone"],ScalarType "interval"),
    ("age",[ScalarType "timestamp without time zone",ScalarType "timestamp without time zone"],ScalarType "interval"),
    ("age",[ScalarType "timestamp without time zone"],ScalarType "interval"),
    ("age",[ScalarType "xid"],ScalarType "integer"),
    ("any_in",[ScalarType "cstring"],ScalarType "\"any\""),
    ("any_out",[ScalarType "\"any\""],ScalarType "cstring"),
    ("anyarray_in",[ScalarType "cstring"],ScalarType "anyarray"),
    ("anyarray_out",[ScalarType "anyarray"],ScalarType "cstring"),
    ("anyarray_recv",[ScalarType "internal"],ScalarType "anyarray"),
    ("anyarray_send",[ScalarType "anyarray"],ScalarType "bytea"),
    ("anyelement_in",[ScalarType "cstring"],ScalarType "anyelement"),
    ("anyelement_out",[ScalarType "anyelement"],ScalarType "cstring"),
    ("anyenum_in",[ScalarType "cstring"],ScalarType "anyenum"),
    ("anyenum_out",[ScalarType "anyenum"],ScalarType "cstring"),
    ("anynonarray_in",[ScalarType "cstring"],ScalarType "anynonarray"),
    ("anynonarray_out",[ScalarType "anynonarray"],ScalarType "cstring"),
    ("anytextcat",[ScalarType "anynonarray",ScalarType "text"],ScalarType "text"),
    ("area",[ScalarType "path"],ScalarType "double precision"),
    ("area",[ScalarType "box"],ScalarType "double precision"),
    ("area",[ScalarType "circle"],ScalarType "double precision"),
    ("areajoinsel",[ScalarType "internal",ScalarType "oid",ScalarType "internal",ScalarType "smallint",ScalarType "internal"],ScalarType "double precision"),
    ("areasel",[ScalarType "internal",ScalarType "oid",ScalarType "internal",ScalarType "integer"],ScalarType "double precision"),
    ("array_agg_finalfn",[ScalarType "internal"],ScalarType "anyarray"),
    ("array_agg_transfn",[ScalarType "internal",ScalarType "anyelement"],ScalarType "internal"),
    ("array_append",[ScalarType "anyarray",ScalarType "anyelement"],ScalarType "anyarray"),
    ("array_cat",[ScalarType "anyarray",ScalarType "anyarray"],ScalarType "anyarray"),
    ("array_dims",[ScalarType "anyarray"],ScalarType "text"),
    ("array_eq",[ScalarType "anyarray",ScalarType "anyarray"],ScalarType "boolean"),
    ("array_fill",[ScalarType "anyelement",ScalarType "integer[]"],ScalarType "anyarray"),
    ("array_fill",[ScalarType "anyelement",ScalarType "integer[]",ScalarType "integer[]"],ScalarType "anyarray"),
    ("array_ge",[ScalarType "anyarray",ScalarType "anyarray"],ScalarType "boolean"),
    ("array_gt",[ScalarType "anyarray",ScalarType "anyarray"],ScalarType "boolean"),
    ("array_in",[ScalarType "cstring",ScalarType "oid",ScalarType "integer"],ScalarType "anyarray"),
    ("array_larger",[ScalarType "anyarray",ScalarType "anyarray"],ScalarType "anyarray"),
    ("array_le",[ScalarType "anyarray",ScalarType "anyarray"],ScalarType "boolean"),
    ("array_length",[ScalarType "anyarray",ScalarType "integer"],ScalarType "integer"),
    ("array_lower",[ScalarType "anyarray",ScalarType "integer"],ScalarType "integer"),
    ("array_lt",[ScalarType "anyarray",ScalarType "anyarray"],ScalarType "boolean"),
    ("array_ndims",[ScalarType "anyarray"],ScalarType "integer"),
    ("array_ne",[ScalarType "anyarray",ScalarType "anyarray"],ScalarType "boolean"),
    ("array_out",[ScalarType "anyarray"],ScalarType "cstring"),
    ("array_prepend",[ScalarType "anyelement",ScalarType "anyarray"],ScalarType "anyarray"),
    ("array_recv",[ScalarType "internal",ScalarType "oid",ScalarType "integer"],ScalarType "anyarray"),
    ("array_send",[ScalarType "anyarray"],ScalarType "bytea"),
    ("array_smaller",[ScalarType "anyarray",ScalarType "anyarray"],ScalarType "anyarray"),
    ("array_to_string",[ScalarType "anyarray",ScalarType "text"],ScalarType "text"),
    ("array_upper",[ScalarType "anyarray",ScalarType "integer"],ScalarType "integer"),
    ("arraycontained",[ScalarType "anyarray",ScalarType "anyarray"],ScalarType "boolean"),
    ("arraycontains",[ScalarType "anyarray",ScalarType "anyarray"],ScalarType "boolean"),
    ("arrayoverlap",[ScalarType "anyarray",ScalarType "anyarray"],ScalarType "boolean"),
    ("ascii",[ScalarType "text"],ScalarType "integer"),
    ("ascii_to_mic",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("ascii_to_utf8",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("asin",[ScalarType "double precision"],ScalarType "double precision"),
    ("atan",[ScalarType "double precision"],ScalarType "double precision"),
    ("atan2",[ScalarType "double precision",ScalarType "double precision"],ScalarType "double precision"),
    ("big5_to_euc_tw",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("big5_to_mic",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("big5_to_utf8",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("bit",[ScalarType "bit",ScalarType "integer",ScalarType "boolean"],ScalarType "bit"),
    ("bit",[ScalarType "bigint",ScalarType "integer"],ScalarType "bit"),
    ("bit",[ScalarType "integer",ScalarType "integer"],ScalarType "bit"),
    ("bit_in",[ScalarType "cstring",ScalarType "oid",ScalarType "integer"],ScalarType "bit"),
    ("bit_length",[ScalarType "bytea"],ScalarType "integer"),
    ("bit_length",[ScalarType "bit"],ScalarType "integer"),
    ("bit_length",[ScalarType "text"],ScalarType "integer"),
    ("bit_out",[ScalarType "bit"],ScalarType "cstring"),
    ("bit_recv",[ScalarType "internal",ScalarType "oid",ScalarType "integer"],ScalarType "bit"),
    ("bit_send",[ScalarType "bit"],ScalarType "bytea"),
    ("bitand",[ScalarType "bit",ScalarType "bit"],ScalarType "bit"),
    ("bitcat",[ScalarType "bit varying",ScalarType "bit varying"],ScalarType "bit varying"),
    ("bitcmp",[ScalarType "bit",ScalarType "bit"],ScalarType "integer"),
    ("biteq",[ScalarType "bit",ScalarType "bit"],ScalarType "boolean"),
    ("bitge",[ScalarType "bit",ScalarType "bit"],ScalarType "boolean"),
    ("bitgt",[ScalarType "bit",ScalarType "bit"],ScalarType "boolean"),
    ("bitle",[ScalarType "bit",ScalarType "bit"],ScalarType "boolean"),
    ("bitlt",[ScalarType "bit",ScalarType "bit"],ScalarType "boolean"),
    ("bitne",[ScalarType "bit",ScalarType "bit"],ScalarType "boolean"),
    ("bitnot",[ScalarType "bit"],ScalarType "bit"),
    ("bitor",[ScalarType "bit",ScalarType "bit"],ScalarType "bit"),
    ("bitshiftleft",[ScalarType "bit",ScalarType "integer"],ScalarType "bit"),
    ("bitshiftright",[ScalarType "bit",ScalarType "integer"],ScalarType "bit"),
    ("bittypmodin",[ScalarType "cstring[]"],ScalarType "integer"),
    ("bittypmodout",[ScalarType "integer"],ScalarType "cstring"),
    ("bitxor",[ScalarType "bit",ScalarType "bit"],ScalarType "bit"),
    ("bool",[ScalarType "integer"],ScalarType "boolean"),
    ("booland_statefunc",[ScalarType "boolean",ScalarType "boolean"],ScalarType "boolean"),
    ("booleq",[ScalarType "boolean",ScalarType "boolean"],ScalarType "boolean"),
    ("boolge",[ScalarType "boolean",ScalarType "boolean"],ScalarType "boolean"),
    ("boolgt",[ScalarType "boolean",ScalarType "boolean"],ScalarType "boolean"),
    ("boolin",[ScalarType "cstring"],ScalarType "boolean"),
    ("boolle",[ScalarType "boolean",ScalarType "boolean"],ScalarType "boolean"),
    ("boollt",[ScalarType "boolean",ScalarType "boolean"],ScalarType "boolean"),
    ("boolne",[ScalarType "boolean",ScalarType "boolean"],ScalarType "boolean"),
    ("boolor_statefunc",[ScalarType "boolean",ScalarType "boolean"],ScalarType "boolean"),
    ("boolout",[ScalarType "boolean"],ScalarType "cstring"),
    ("boolrecv",[ScalarType "internal"],ScalarType "boolean"),
    ("boolsend",[ScalarType "boolean"],ScalarType "bytea"),
    ("box",[ScalarType "circle"],ScalarType "box"),
    ("box",[ScalarType "point",ScalarType "point"],ScalarType "box"),
    ("box",[ScalarType "polygon"],ScalarType "box"),
    ("box_above",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    ("box_above_eq",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    ("box_add",[ScalarType "box",ScalarType "point"],ScalarType "box"),
    ("box_below",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    ("box_below_eq",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    ("box_center",[ScalarType "box"],ScalarType "point"),
    ("box_contain",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    ("box_contained",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    ("box_distance",[ScalarType "box",ScalarType "box"],ScalarType "double precision"),
    ("box_div",[ScalarType "box",ScalarType "point"],ScalarType "box"),
    ("box_eq",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    ("box_ge",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    ("box_gt",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    ("box_in",[ScalarType "cstring"],ScalarType "box"),
    ("box_intersect",[ScalarType "box",ScalarType "box"],ScalarType "box"),
    ("box_le",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    ("box_left",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    ("box_lt",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    ("box_mul",[ScalarType "box",ScalarType "point"],ScalarType "box"),
    ("box_out",[ScalarType "box"],ScalarType "cstring"),
    ("box_overabove",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    ("box_overbelow",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    ("box_overlap",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    ("box_overleft",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    ("box_overright",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    ("box_recv",[ScalarType "internal"],ScalarType "box"),
    ("box_right",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    ("box_same",[ScalarType "box",ScalarType "box"],ScalarType "boolean"),
    ("box_send",[ScalarType "box"],ScalarType "bytea"),
    ("box_sub",[ScalarType "box",ScalarType "point"],ScalarType "box"),
    ("bpchar",[ScalarType "\"char\""],ScalarType "character"),
    ("bpchar",[ScalarType "character",ScalarType "integer",ScalarType "boolean"],ScalarType "character"),
    ("bpchar",[ScalarType "name"],ScalarType "character"),
    ("bpchar_larger",[ScalarType "character",ScalarType "character"],ScalarType "character"),
    ("bpchar_pattern_ge",[ScalarType "character",ScalarType "character"],ScalarType "boolean"),
    ("bpchar_pattern_gt",[ScalarType "character",ScalarType "character"],ScalarType "boolean"),
    ("bpchar_pattern_le",[ScalarType "character",ScalarType "character"],ScalarType "boolean"),
    ("bpchar_pattern_lt",[ScalarType "character",ScalarType "character"],ScalarType "boolean"),
    ("bpchar_smaller",[ScalarType "character",ScalarType "character"],ScalarType "character"),
    ("bpcharcmp",[ScalarType "character",ScalarType "character"],ScalarType "integer"),
    ("bpchareq",[ScalarType "character",ScalarType "character"],ScalarType "boolean"),
    ("bpcharge",[ScalarType "character",ScalarType "character"],ScalarType "boolean"),
    ("bpchargt",[ScalarType "character",ScalarType "character"],ScalarType "boolean"),
    ("bpchariclike",[ScalarType "character",ScalarType "text"],ScalarType "boolean"),
    ("bpcharicnlike",[ScalarType "character",ScalarType "text"],ScalarType "boolean"),
    ("bpcharicregexeq",[ScalarType "character",ScalarType "text"],ScalarType "boolean"),
    ("bpcharicregexne",[ScalarType "character",ScalarType "text"],ScalarType "boolean"),
    ("bpcharin",[ScalarType "cstring",ScalarType "oid",ScalarType "integer"],ScalarType "character"),
    ("bpcharle",[ScalarType "character",ScalarType "character"],ScalarType "boolean"),
    ("bpcharlike",[ScalarType "character",ScalarType "text"],ScalarType "boolean"),
    ("bpcharlt",[ScalarType "character",ScalarType "character"],ScalarType "boolean"),
    ("bpcharne",[ScalarType "character",ScalarType "character"],ScalarType "boolean"),
    ("bpcharnlike",[ScalarType "character",ScalarType "text"],ScalarType "boolean"),
    ("bpcharout",[ScalarType "character"],ScalarType "cstring"),
    ("bpcharrecv",[ScalarType "internal",ScalarType "oid",ScalarType "integer"],ScalarType "character"),
    ("bpcharregexeq",[ScalarType "character",ScalarType "text"],ScalarType "boolean"),
    ("bpcharregexne",[ScalarType "character",ScalarType "text"],ScalarType "boolean"),
    ("bpcharsend",[ScalarType "character"],ScalarType "bytea"),
    ("bpchartypmodin",[ScalarType "cstring[]"],ScalarType "integer"),
    ("bpchartypmodout",[ScalarType "integer"],ScalarType "cstring"),
    ("broadcast",[ScalarType "inet"],ScalarType "inet"),
    ("btabstimecmp",[ScalarType "abstime",ScalarType "abstime"],ScalarType "integer"),
    ("btarraycmp",[ScalarType "anyarray",ScalarType "anyarray"],ScalarType "integer"),
    ("btbeginscan",[ScalarType "internal",ScalarType "internal",ScalarType "internal"],ScalarType "internal"),
    ("btboolcmp",[ScalarType "boolean",ScalarType "boolean"],ScalarType "integer"),
    ("btbpchar_pattern_cmp",[ScalarType "character",ScalarType "character"],ScalarType "integer"),
    ("btbuild",[ScalarType "internal",ScalarType "internal",ScalarType "internal"],ScalarType "internal"),
    ("btbulkdelete",[ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal"],ScalarType "internal"),
    ("btcharcmp",[ScalarType "\"char\"",ScalarType "\"char\""],ScalarType "integer"),
    ("btcostestimate",[ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal"],ScalarType "void"),
    ("btendscan",[ScalarType "internal"],ScalarType "void"),
    ("btfloat48cmp",[ScalarType "real",ScalarType "double precision"],ScalarType "integer"),
    ("btfloat4cmp",[ScalarType "real",ScalarType "real"],ScalarType "integer"),
    ("btfloat84cmp",[ScalarType "double precision",ScalarType "real"],ScalarType "integer"),
    ("btfloat8cmp",[ScalarType "double precision",ScalarType "double precision"],ScalarType "integer"),
    ("btgetbitmap",[ScalarType "internal",ScalarType "internal"],ScalarType "bigint"),
    ("btgettuple",[ScalarType "internal",ScalarType "internal"],ScalarType "boolean"),
    ("btinsert",[ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal"],ScalarType "boolean"),
    ("btint24cmp",[ScalarType "smallint",ScalarType "integer"],ScalarType "integer"),
    ("btint28cmp",[ScalarType "smallint",ScalarType "bigint"],ScalarType "integer"),
    ("btint2cmp",[ScalarType "smallint",ScalarType "smallint"],ScalarType "integer"),
    ("btint42cmp",[ScalarType "integer",ScalarType "smallint"],ScalarType "integer"),
    ("btint48cmp",[ScalarType "integer",ScalarType "bigint"],ScalarType "integer"),
    ("btint4cmp",[ScalarType "integer",ScalarType "integer"],ScalarType "integer"),
    ("btint82cmp",[ScalarType "bigint",ScalarType "smallint"],ScalarType "integer"),
    ("btint84cmp",[ScalarType "bigint",ScalarType "integer"],ScalarType "integer"),
    ("btint8cmp",[ScalarType "bigint",ScalarType "bigint"],ScalarType "integer"),
    ("btmarkpos",[ScalarType "internal"],ScalarType "void"),
    ("btnamecmp",[ScalarType "name",ScalarType "name"],ScalarType "integer"),
    ("btoidcmp",[ScalarType "oid",ScalarType "oid"],ScalarType "integer"),
    ("btoidvectorcmp",[ScalarType "oidvector",ScalarType "oidvector"],ScalarType "integer"),
    ("btoptions",[ScalarType "text[]",ScalarType "boolean"],ScalarType "bytea"),
    ("btrecordcmp",[ScalarType "record",ScalarType "record"],ScalarType "integer"),
    ("btreltimecmp",[ScalarType "reltime",ScalarType "reltime"],ScalarType "integer"),
    ("btrescan",[ScalarType "internal",ScalarType "internal"],ScalarType "void"),
    ("btrestrpos",[ScalarType "internal"],ScalarType "void"),
    ("btrim",[ScalarType "text"],ScalarType "text"),
    ("btrim",[ScalarType "bytea",ScalarType "bytea"],ScalarType "bytea"),
    ("btrim",[ScalarType "text",ScalarType "text"],ScalarType "text"),
    ("bttext_pattern_cmp",[ScalarType "text",ScalarType "text"],ScalarType "integer"),
    ("bttextcmp",[ScalarType "text",ScalarType "text"],ScalarType "integer"),
    ("bttidcmp",[ScalarType "tid",ScalarType "tid"],ScalarType "integer"),
    ("bttintervalcmp",[ScalarType "tinterval",ScalarType "tinterval"],ScalarType "integer"),
    ("btvacuumcleanup",[ScalarType "internal",ScalarType "internal"],ScalarType "internal"),
    ("byteacat",[ScalarType "bytea",ScalarType "bytea"],ScalarType "bytea"),
    ("byteacmp",[ScalarType "bytea",ScalarType "bytea"],ScalarType "integer"),
    ("byteaeq",[ScalarType "bytea",ScalarType "bytea"],ScalarType "boolean"),
    ("byteage",[ScalarType "bytea",ScalarType "bytea"],ScalarType "boolean"),
    ("byteagt",[ScalarType "bytea",ScalarType "bytea"],ScalarType "boolean"),
    ("byteain",[ScalarType "cstring"],ScalarType "bytea"),
    ("byteale",[ScalarType "bytea",ScalarType "bytea"],ScalarType "boolean"),
    ("bytealike",[ScalarType "bytea",ScalarType "bytea"],ScalarType "boolean"),
    ("bytealt",[ScalarType "bytea",ScalarType "bytea"],ScalarType "boolean"),
    ("byteane",[ScalarType "bytea",ScalarType "bytea"],ScalarType "boolean"),
    ("byteanlike",[ScalarType "bytea",ScalarType "bytea"],ScalarType "boolean"),
    ("byteaout",[ScalarType "bytea"],ScalarType "cstring"),
    ("bytearecv",[ScalarType "internal"],ScalarType "bytea"),
    ("byteasend",[ScalarType "bytea"],ScalarType "bytea"),
    ("cash_cmp",[ScalarType "money",ScalarType "money"],ScalarType "integer"),
    ("cash_div_flt4",[ScalarType "money",ScalarType "real"],ScalarType "money"),
    ("cash_div_flt8",[ScalarType "money",ScalarType "double precision"],ScalarType "money"),
    ("cash_div_int2",[ScalarType "money",ScalarType "smallint"],ScalarType "money"),
    ("cash_div_int4",[ScalarType "money",ScalarType "integer"],ScalarType "money"),
    ("cash_eq",[ScalarType "money",ScalarType "money"],ScalarType "boolean"),
    ("cash_ge",[ScalarType "money",ScalarType "money"],ScalarType "boolean"),
    ("cash_gt",[ScalarType "money",ScalarType "money"],ScalarType "boolean"),
    ("cash_in",[ScalarType "cstring"],ScalarType "money"),
    ("cash_le",[ScalarType "money",ScalarType "money"],ScalarType "boolean"),
    ("cash_lt",[ScalarType "money",ScalarType "money"],ScalarType "boolean"),
    ("cash_mi",[ScalarType "money",ScalarType "money"],ScalarType "money"),
    ("cash_mul_flt4",[ScalarType "money",ScalarType "real"],ScalarType "money"),
    ("cash_mul_flt8",[ScalarType "money",ScalarType "double precision"],ScalarType "money"),
    ("cash_mul_int2",[ScalarType "money",ScalarType "smallint"],ScalarType "money"),
    ("cash_mul_int4",[ScalarType "money",ScalarType "integer"],ScalarType "money"),
    ("cash_ne",[ScalarType "money",ScalarType "money"],ScalarType "boolean"),
    ("cash_out",[ScalarType "money"],ScalarType "cstring"),
    ("cash_pl",[ScalarType "money",ScalarType "money"],ScalarType "money"),
    ("cash_recv",[ScalarType "internal"],ScalarType "money"),
    ("cash_send",[ScalarType "money"],ScalarType "bytea"),
    ("cash_words",[ScalarType "money"],ScalarType "text"),
    ("cashlarger",[ScalarType "money",ScalarType "money"],ScalarType "money"),
    ("cashsmaller",[ScalarType "money",ScalarType "money"],ScalarType "money"),
    ("cbrt",[ScalarType "double precision"],ScalarType "double precision"),
    ("ceil",[ScalarType "double precision"],ScalarType "double precision"),
    ("ceil",[ScalarType "numeric"],ScalarType "numeric"),
    ("ceiling",[ScalarType "double precision"],ScalarType "double precision"),
    ("ceiling",[ScalarType "numeric"],ScalarType "numeric"),
    ("center",[ScalarType "circle"],ScalarType "point"),
    ("center",[ScalarType "box"],ScalarType "point"),
    ("char",[ScalarType "integer"],ScalarType "\"char\""),
    ("char",[ScalarType "text"],ScalarType "\"char\""),
    ("char_length",[ScalarType "text"],ScalarType "integer"),
    ("char_length",[ScalarType "character"],ScalarType "integer"),
    ("character_length",[ScalarType "character"],ScalarType "integer"),
    ("character_length",[ScalarType "text"],ScalarType "integer"),
    ("chareq",[ScalarType "\"char\"",ScalarType "\"char\""],ScalarType "boolean"),
    ("charge",[ScalarType "\"char\"",ScalarType "\"char\""],ScalarType "boolean"),
    ("chargt",[ScalarType "\"char\"",ScalarType "\"char\""],ScalarType "boolean"),
    ("charin",[ScalarType "cstring"],ScalarType "\"char\""),
    ("charle",[ScalarType "\"char\"",ScalarType "\"char\""],ScalarType "boolean"),
    ("charlt",[ScalarType "\"char\"",ScalarType "\"char\""],ScalarType "boolean"),
    ("charne",[ScalarType "\"char\"",ScalarType "\"char\""],ScalarType "boolean"),
    ("charout",[ScalarType "\"char\""],ScalarType "cstring"),
    ("charrecv",[ScalarType "internal"],ScalarType "\"char\""),
    ("charsend",[ScalarType "\"char\""],ScalarType "bytea"),
    ("chr",[ScalarType "integer"],ScalarType "text"),
    ("cideq",[ScalarType "cid",ScalarType "cid"],ScalarType "boolean"),
    ("cidin",[ScalarType "cstring"],ScalarType "cid"),
    ("cidout",[ScalarType "cid"],ScalarType "cstring"),
    ("cidr",[ScalarType "inet"],ScalarType "cidr"),
    ("cidr_in",[ScalarType "cstring"],ScalarType "cidr"),
    ("cidr_out",[ScalarType "cidr"],ScalarType "cstring"),
    ("cidr_recv",[ScalarType "internal"],ScalarType "cidr"),
    ("cidr_send",[ScalarType "cidr"],ScalarType "bytea"),
    ("cidrecv",[ScalarType "internal"],ScalarType "cid"),
    ("cidsend",[ScalarType "cid"],ScalarType "bytea"),
    ("circle",[ScalarType "polygon"],ScalarType "circle"),
    ("circle",[ScalarType "point",ScalarType "double precision"],ScalarType "circle"),
    ("circle",[ScalarType "box"],ScalarType "circle"),
    ("circle_above",[ScalarType "circle",ScalarType "circle"],ScalarType "boolean"),
    ("circle_add_pt",[ScalarType "circle",ScalarType "point"],ScalarType "circle"),
    ("circle_below",[ScalarType "circle",ScalarType "circle"],ScalarType "boolean"),
    ("circle_center",[ScalarType "circle"],ScalarType "point"),
    ("circle_contain",[ScalarType "circle",ScalarType "circle"],ScalarType "boolean"),
    ("circle_contain_pt",[ScalarType "circle",ScalarType "point"],ScalarType "boolean"),
    ("circle_contained",[ScalarType "circle",ScalarType "circle"],ScalarType "boolean"),
    ("circle_distance",[ScalarType "circle",ScalarType "circle"],ScalarType "double precision"),
    ("circle_div_pt",[ScalarType "circle",ScalarType "point"],ScalarType "circle"),
    ("circle_eq",[ScalarType "circle",ScalarType "circle"],ScalarType "boolean"),
    ("circle_ge",[ScalarType "circle",ScalarType "circle"],ScalarType "boolean"),
    ("circle_gt",[ScalarType "circle",ScalarType "circle"],ScalarType "boolean"),
    ("circle_in",[ScalarType "cstring"],ScalarType "circle"),
    ("circle_le",[ScalarType "circle",ScalarType "circle"],ScalarType "boolean"),
    ("circle_left",[ScalarType "circle",ScalarType "circle"],ScalarType "boolean"),
    ("circle_lt",[ScalarType "circle",ScalarType "circle"],ScalarType "boolean"),
    ("circle_mul_pt",[ScalarType "circle",ScalarType "point"],ScalarType "circle"),
    ("circle_ne",[ScalarType "circle",ScalarType "circle"],ScalarType "boolean"),
    ("circle_out",[ScalarType "circle"],ScalarType "cstring"),
    ("circle_overabove",[ScalarType "circle",ScalarType "circle"],ScalarType "boolean"),
    ("circle_overbelow",[ScalarType "circle",ScalarType "circle"],ScalarType "boolean"),
    ("circle_overlap",[ScalarType "circle",ScalarType "circle"],ScalarType "boolean"),
    ("circle_overleft",[ScalarType "circle",ScalarType "circle"],ScalarType "boolean"),
    ("circle_overright",[ScalarType "circle",ScalarType "circle"],ScalarType "boolean"),
    ("circle_recv",[ScalarType "internal"],ScalarType "circle"),
    ("circle_right",[ScalarType "circle",ScalarType "circle"],ScalarType "boolean"),
    ("circle_same",[ScalarType "circle",ScalarType "circle"],ScalarType "boolean"),
    ("circle_send",[ScalarType "circle"],ScalarType "bytea"),
    ("circle_sub_pt",[ScalarType "circle",ScalarType "point"],ScalarType "circle"),
    ("clock_timestamp",[],ScalarType "timestamp with time zone"),
    ("close_lb",[ScalarType "line",ScalarType "box"],ScalarType "point"),
    ("close_ls",[ScalarType "line",ScalarType "lseg"],ScalarType "point"),
    ("close_lseg",[ScalarType "lseg",ScalarType "lseg"],ScalarType "point"),
    ("close_pb",[ScalarType "point",ScalarType "box"],ScalarType "point"),
    ("close_pl",[ScalarType "point",ScalarType "line"],ScalarType "point"),
    ("close_ps",[ScalarType "point",ScalarType "lseg"],ScalarType "point"),
    ("close_sb",[ScalarType "lseg",ScalarType "box"],ScalarType "point"),
    ("close_sl",[ScalarType "lseg",ScalarType "line"],ScalarType "point"),
    ("col_description",[ScalarType "oid",ScalarType "integer"],ScalarType "text"),
    ("contjoinsel",[ScalarType "internal",ScalarType "oid",ScalarType "internal",ScalarType "smallint",ScalarType "internal"],ScalarType "double precision"),
    ("contsel",[ScalarType "internal",ScalarType "oid",ScalarType "internal",ScalarType "integer"],ScalarType "double precision"),
    ("convert",[ScalarType "bytea",ScalarType "name",ScalarType "name"],ScalarType "bytea"),
    ("convert_from",[ScalarType "bytea",ScalarType "name"],ScalarType "text"),
    ("convert_to",[ScalarType "text",ScalarType "name"],ScalarType "bytea"),
    ("cos",[ScalarType "double precision"],ScalarType "double precision"),
    ("cot",[ScalarType "double precision"],ScalarType "double precision"),
    ("cstring_in",[ScalarType "cstring"],ScalarType "cstring"),
    ("cstring_out",[ScalarType "cstring"],ScalarType "cstring"),
    ("cstring_recv",[ScalarType "internal"],ScalarType "cstring"),
    ("cstring_send",[ScalarType "cstring"],ScalarType "bytea"),
    ("current_database",[],ScalarType "name"),
    ("current_query",[],ScalarType "text"),
    ("current_schema",[],ScalarType "name"),
    ("current_schemas",[ScalarType "boolean"],ScalarType "name[]"),
    ("current_setting",[ScalarType "text"],ScalarType "text"),
    ("current_user",[],ScalarType "name"),
    ("currtid",[ScalarType "oid",ScalarType "tid"],ScalarType "tid"),
    ("currtid2",[ScalarType "text",ScalarType "tid"],ScalarType "tid"),
    ("currval",[ScalarType "regclass"],ScalarType "bigint"),
    ("cursor_to_xml",[ScalarType "cursor refcursor",ScalarType "count integer",ScalarType "nulls boolean",ScalarType "tableforest boolean",ScalarType "targetns text"],ScalarType "xml"),
    ("cursor_to_xmlschema",[ScalarType "cursor refcursor",ScalarType "nulls boolean",ScalarType "tableforest boolean",ScalarType "targetns text"],ScalarType "xml"),
    ("database_to_xml",[ScalarType "nulls boolean",ScalarType "tableforest boolean",ScalarType "targetns text"],ScalarType "xml"),
    ("database_to_xml_and_xmlschema",[ScalarType "nulls boolean",ScalarType "tableforest boolean",ScalarType "targetns text"],ScalarType "xml"),
    ("database_to_xmlschema",[ScalarType "nulls boolean",ScalarType "tableforest boolean",ScalarType "targetns text"],ScalarType "xml"),
    ("date",[ScalarType "timestamp without time zone"],ScalarType "date"),
    ("date",[ScalarType "abstime"],ScalarType "date"),
    ("date",[ScalarType "timestamp with time zone"],ScalarType "date"),
    ("date_cmp",[ScalarType "date",ScalarType "date"],ScalarType "integer"),
    ("date_cmp_timestamp",[ScalarType "date",ScalarType "timestamp without time zone"],ScalarType "integer"),
    ("date_cmp_timestamptz",[ScalarType "date",ScalarType "timestamp with time zone"],ScalarType "integer"),
    ("date_eq",[ScalarType "date",ScalarType "date"],ScalarType "boolean"),
    ("date_eq_timestamp",[ScalarType "date",ScalarType "timestamp without time zone"],ScalarType "boolean"),
    ("date_eq_timestamptz",[ScalarType "date",ScalarType "timestamp with time zone"],ScalarType "boolean"),
    ("date_ge",[ScalarType "date",ScalarType "date"],ScalarType "boolean"),
    ("date_ge_timestamp",[ScalarType "date",ScalarType "timestamp without time zone"],ScalarType "boolean"),
    ("date_ge_timestamptz",[ScalarType "date",ScalarType "timestamp with time zone"],ScalarType "boolean"),
    ("date_gt",[ScalarType "date",ScalarType "date"],ScalarType "boolean"),
    ("date_gt_timestamp",[ScalarType "date",ScalarType "timestamp without time zone"],ScalarType "boolean"),
    ("date_gt_timestamptz",[ScalarType "date",ScalarType "timestamp with time zone"],ScalarType "boolean"),
    ("date_in",[ScalarType "cstring"],ScalarType "date"),
    ("date_larger",[ScalarType "date",ScalarType "date"],ScalarType "date"),
    ("date_le",[ScalarType "date",ScalarType "date"],ScalarType "boolean"),
    ("date_le_timestamp",[ScalarType "date",ScalarType "timestamp without time zone"],ScalarType "boolean"),
    ("date_le_timestamptz",[ScalarType "date",ScalarType "timestamp with time zone"],ScalarType "boolean"),
    ("date_lt",[ScalarType "date",ScalarType "date"],ScalarType "boolean"),
    ("date_lt_timestamp",[ScalarType "date",ScalarType "timestamp without time zone"],ScalarType "boolean"),
    ("date_lt_timestamptz",[ScalarType "date",ScalarType "timestamp with time zone"],ScalarType "boolean"),
    ("date_mi",[ScalarType "date",ScalarType "date"],ScalarType "integer"),
    ("date_mi_interval",[ScalarType "date",ScalarType "interval"],ScalarType "timestamp without time zone"),
    ("date_mii",[ScalarType "date",ScalarType "integer"],ScalarType "date"),
    ("date_ne",[ScalarType "date",ScalarType "date"],ScalarType "boolean"),
    ("date_ne_timestamp",[ScalarType "date",ScalarType "timestamp without time zone"],ScalarType "boolean"),
    ("date_ne_timestamptz",[ScalarType "date",ScalarType "timestamp with time zone"],ScalarType "boolean"),
    ("date_out",[ScalarType "date"],ScalarType "cstring"),
    ("date_part",[ScalarType "text",ScalarType "time without time zone"],ScalarType "double precision"),
    ("date_part",[ScalarType "text",ScalarType "timestamp without time zone"],ScalarType "double precision"),
    ("date_part",[ScalarType "text",ScalarType "date"],ScalarType "double precision"),
    ("date_part",[ScalarType "text",ScalarType "abstime"],ScalarType "double precision"),
    ("date_part",[ScalarType "text",ScalarType "timestamp with time zone"],ScalarType "double precision"),
    ("date_part",[ScalarType "text",ScalarType "reltime"],ScalarType "double precision"),
    ("date_part",[ScalarType "text",ScalarType "interval"],ScalarType "double precision"),
    ("date_part",[ScalarType "text",ScalarType "time with time zone"],ScalarType "double precision"),
    ("date_pl_interval",[ScalarType "date",ScalarType "interval"],ScalarType "timestamp without time zone"),
    ("date_pli",[ScalarType "date",ScalarType "integer"],ScalarType "date"),
    ("date_recv",[ScalarType "internal"],ScalarType "date"),
    ("date_send",[ScalarType "date"],ScalarType "bytea"),
    ("date_smaller",[ScalarType "date",ScalarType "date"],ScalarType "date"),
    ("date_trunc",[ScalarType "text",ScalarType "timestamp with time zone"],ScalarType "timestamp with time zone"),
    ("date_trunc",[ScalarType "text",ScalarType "timestamp without time zone"],ScalarType "timestamp without time zone"),
    ("date_trunc",[ScalarType "text",ScalarType "interval"],ScalarType "interval"),
    ("datetime_pl",[ScalarType "date",ScalarType "time without time zone"],ScalarType "timestamp without time zone"),
    ("datetimetz_pl",[ScalarType "date",ScalarType "time with time zone"],ScalarType "timestamp with time zone"),
    ("dcbrt",[ScalarType "double precision"],ScalarType "double precision"),
    ("decode",[ScalarType "text",ScalarType "text"],ScalarType "bytea"),
    ("degrees",[ScalarType "double precision"],ScalarType "double precision"),
    ("dexp",[ScalarType "double precision"],ScalarType "double precision"),
    ("diagonal",[ScalarType "box"],ScalarType "lseg"),
    ("diameter",[ScalarType "circle"],ScalarType "double precision"),
    ("dispell_init",[ScalarType "internal"],ScalarType "internal"),
    ("dispell_lexize",[ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal"],ScalarType "internal"),
    ("dist_cpoly",[ScalarType "circle",ScalarType "polygon"],ScalarType "double precision"),
    ("dist_lb",[ScalarType "line",ScalarType "box"],ScalarType "double precision"),
    ("dist_pb",[ScalarType "point",ScalarType "box"],ScalarType "double precision"),
    ("dist_pc",[ScalarType "point",ScalarType "circle"],ScalarType "double precision"),
    ("dist_pl",[ScalarType "point",ScalarType "line"],ScalarType "double precision"),
    ("dist_ppath",[ScalarType "point",ScalarType "path"],ScalarType "double precision"),
    ("dist_ps",[ScalarType "point",ScalarType "lseg"],ScalarType "double precision"),
    ("dist_sb",[ScalarType "lseg",ScalarType "box"],ScalarType "double precision"),
    ("dist_sl",[ScalarType "lseg",ScalarType "line"],ScalarType "double precision"),
    ("div",[ScalarType "numeric",ScalarType "numeric"],ScalarType "numeric"),
    ("dlog1",[ScalarType "double precision"],ScalarType "double precision"),
    ("dlog10",[ScalarType "double precision"],ScalarType "double precision"),
    ("domain_in",[ScalarType "cstring",ScalarType "oid",ScalarType "integer"],ScalarType "\"any\""),
    ("domain_recv",[ScalarType "internal",ScalarType "oid",ScalarType "integer"],ScalarType "\"any\""),
    ("dpow",[ScalarType "double precision",ScalarType "double precision"],ScalarType "double precision"),
    ("dround",[ScalarType "double precision"],ScalarType "double precision"),
    ("dsimple_init",[ScalarType "internal"],ScalarType "internal"),
    ("dsimple_lexize",[ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal"],ScalarType "internal"),
    ("dsnowball_init",[ScalarType "internal"],ScalarType "internal"),
    ("dsnowball_lexize",[ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal"],ScalarType "internal"),
    ("dsqrt",[ScalarType "double precision"],ScalarType "double precision"),
    ("dsynonym_init",[ScalarType "internal"],ScalarType "internal"),
    ("dsynonym_lexize",[ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal"],ScalarType "internal"),
    ("dtrunc",[ScalarType "double precision"],ScalarType "double precision"),
    ("encode",[ScalarType "bytea",ScalarType "text"],ScalarType "text"),
    ("enum_cmp",[ScalarType "anyenum",ScalarType "anyenum"],ScalarType "integer"),
    ("enum_eq",[ScalarType "anyenum",ScalarType "anyenum"],ScalarType "boolean"),
    ("enum_first",[ScalarType "anyenum"],ScalarType "anyenum"),
    ("enum_ge",[ScalarType "anyenum",ScalarType "anyenum"],ScalarType "boolean"),
    ("enum_gt",[ScalarType "anyenum",ScalarType "anyenum"],ScalarType "boolean"),
    ("enum_in",[ScalarType "cstring",ScalarType "oid"],ScalarType "anyenum"),
    ("enum_larger",[ScalarType "anyenum",ScalarType "anyenum"],ScalarType "anyenum"),
    ("enum_last",[ScalarType "anyenum"],ScalarType "anyenum"),
    ("enum_le",[ScalarType "anyenum",ScalarType "anyenum"],ScalarType "boolean"),
    ("enum_lt",[ScalarType "anyenum",ScalarType "anyenum"],ScalarType "boolean"),
    ("enum_ne",[ScalarType "anyenum",ScalarType "anyenum"],ScalarType "boolean"),
    ("enum_out",[ScalarType "anyenum"],ScalarType "cstring"),
    ("enum_range",[ScalarType "anyenum",ScalarType "anyenum"],ScalarType "anyarray"),
    ("enum_range",[ScalarType "anyenum"],ScalarType "anyarray"),
    ("enum_recv",[ScalarType "cstring",ScalarType "oid"],ScalarType "anyenum"),
    ("enum_send",[ScalarType "anyenum"],ScalarType "bytea"),
    ("enum_smaller",[ScalarType "anyenum",ScalarType "anyenum"],ScalarType "anyenum"),
    ("eqjoinsel",[ScalarType "internal",ScalarType "oid",ScalarType "internal",ScalarType "smallint",ScalarType "internal"],ScalarType "double precision"),
    ("eqsel",[ScalarType "internal",ScalarType "oid",ScalarType "internal",ScalarType "integer"],ScalarType "double precision"),
    ("euc_cn_to_mic",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("euc_cn_to_utf8",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("euc_jis_2004_to_shift_jis_2004",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("euc_jis_2004_to_utf8",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("euc_jp_to_mic",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("euc_jp_to_sjis",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("euc_jp_to_utf8",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("euc_kr_to_mic",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("euc_kr_to_utf8",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("euc_tw_to_big5",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("euc_tw_to_mic",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("euc_tw_to_utf8",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("exp",[ScalarType "double precision"],ScalarType "double precision"),
    ("exp",[ScalarType "numeric"],ScalarType "numeric"),
    ("factorial",[ScalarType "bigint"],ScalarType "numeric"),
    ("family",[ScalarType "inet"],ScalarType "integer"),
    ("float4",[ScalarType "double precision"],ScalarType "real"),
    ("float4",[ScalarType "integer"],ScalarType "real"),
    ("float4",[ScalarType "smallint"],ScalarType "real"),
    ("float4",[ScalarType "numeric"],ScalarType "real"),
    ("float4",[ScalarType "bigint"],ScalarType "real"),
    ("float48div",[ScalarType "real",ScalarType "double precision"],ScalarType "double precision"),
    ("float48eq",[ScalarType "real",ScalarType "double precision"],ScalarType "boolean"),
    ("float48ge",[ScalarType "real",ScalarType "double precision"],ScalarType "boolean"),
    ("float48gt",[ScalarType "real",ScalarType "double precision"],ScalarType "boolean"),
    ("float48le",[ScalarType "real",ScalarType "double precision"],ScalarType "boolean"),
    ("float48lt",[ScalarType "real",ScalarType "double precision"],ScalarType "boolean"),
    ("float48mi",[ScalarType "real",ScalarType "double precision"],ScalarType "double precision"),
    ("float48mul",[ScalarType "real",ScalarType "double precision"],ScalarType "double precision"),
    ("float48ne",[ScalarType "real",ScalarType "double precision"],ScalarType "boolean"),
    ("float48pl",[ScalarType "real",ScalarType "double precision"],ScalarType "double precision"),
    ("float4_accum",[ScalarType "double precision[]",ScalarType "real"],ScalarType "double precision[]"),
    ("float4abs",[ScalarType "real"],ScalarType "real"),
    ("float4div",[ScalarType "real",ScalarType "real"],ScalarType "real"),
    ("float4eq",[ScalarType "real",ScalarType "real"],ScalarType "boolean"),
    ("float4ge",[ScalarType "real",ScalarType "real"],ScalarType "boolean"),
    ("float4gt",[ScalarType "real",ScalarType "real"],ScalarType "boolean"),
    ("float4in",[ScalarType "cstring"],ScalarType "real"),
    ("float4larger",[ScalarType "real",ScalarType "real"],ScalarType "real"),
    ("float4le",[ScalarType "real",ScalarType "real"],ScalarType "boolean"),
    ("float4lt",[ScalarType "real",ScalarType "real"],ScalarType "boolean"),
    ("float4mi",[ScalarType "real",ScalarType "real"],ScalarType "real"),
    ("float4mul",[ScalarType "real",ScalarType "real"],ScalarType "real"),
    ("float4ne",[ScalarType "real",ScalarType "real"],ScalarType "boolean"),
    ("float4out",[ScalarType "real"],ScalarType "cstring"),
    ("float4pl",[ScalarType "real",ScalarType "real"],ScalarType "real"),
    ("float4recv",[ScalarType "internal"],ScalarType "real"),
    ("float4send",[ScalarType "real"],ScalarType "bytea"),
    ("float4smaller",[ScalarType "real",ScalarType "real"],ScalarType "real"),
    ("float4um",[ScalarType "real"],ScalarType "real"),
    ("float4up",[ScalarType "real"],ScalarType "real"),
    ("float8",[ScalarType "real"],ScalarType "double precision"),
    ("float8",[ScalarType "integer"],ScalarType "double precision"),
    ("float8",[ScalarType "bigint"],ScalarType "double precision"),
    ("float8",[ScalarType "smallint"],ScalarType "double precision"),
    ("float8",[ScalarType "numeric"],ScalarType "double precision"),
    ("float84div",[ScalarType "double precision",ScalarType "real"],ScalarType "double precision"),
    ("float84eq",[ScalarType "double precision",ScalarType "real"],ScalarType "boolean"),
    ("float84ge",[ScalarType "double precision",ScalarType "real"],ScalarType "boolean"),
    ("float84gt",[ScalarType "double precision",ScalarType "real"],ScalarType "boolean"),
    ("float84le",[ScalarType "double precision",ScalarType "real"],ScalarType "boolean"),
    ("float84lt",[ScalarType "double precision",ScalarType "real"],ScalarType "boolean"),
    ("float84mi",[ScalarType "double precision",ScalarType "real"],ScalarType "double precision"),
    ("float84mul",[ScalarType "double precision",ScalarType "real"],ScalarType "double precision"),
    ("float84ne",[ScalarType "double precision",ScalarType "real"],ScalarType "boolean"),
    ("float84pl",[ScalarType "double precision",ScalarType "real"],ScalarType "double precision"),
    ("float8_accum",[ScalarType "double precision[]",ScalarType "double precision"],ScalarType "double precision[]"),
    ("float8_avg",[ScalarType "double precision[]"],ScalarType "double precision"),
    ("float8_corr",[ScalarType "double precision[]"],ScalarType "double precision"),
    ("float8_covar_pop",[ScalarType "double precision[]"],ScalarType "double precision"),
    ("float8_covar_samp",[ScalarType "double precision[]"],ScalarType "double precision"),
    ("float8_regr_accum",[ScalarType "double precision[]",ScalarType "double precision",ScalarType "double precision"],ScalarType "double precision[]"),
    ("float8_regr_avgx",[ScalarType "double precision[]"],ScalarType "double precision"),
    ("float8_regr_avgy",[ScalarType "double precision[]"],ScalarType "double precision"),
    ("float8_regr_intercept",[ScalarType "double precision[]"],ScalarType "double precision"),
    ("float8_regr_r2",[ScalarType "double precision[]"],ScalarType "double precision"),
    ("float8_regr_slope",[ScalarType "double precision[]"],ScalarType "double precision"),
    ("float8_regr_sxx",[ScalarType "double precision[]"],ScalarType "double precision"),
    ("float8_regr_sxy",[ScalarType "double precision[]"],ScalarType "double precision"),
    ("float8_regr_syy",[ScalarType "double precision[]"],ScalarType "double precision"),
    ("float8_stddev_pop",[ScalarType "double precision[]"],ScalarType "double precision"),
    ("float8_stddev_samp",[ScalarType "double precision[]"],ScalarType "double precision"),
    ("float8_var_pop",[ScalarType "double precision[]"],ScalarType "double precision"),
    ("float8_var_samp",[ScalarType "double precision[]"],ScalarType "double precision"),
    ("float8abs",[ScalarType "double precision"],ScalarType "double precision"),
    ("float8div",[ScalarType "double precision",ScalarType "double precision"],ScalarType "double precision"),
    ("float8eq",[ScalarType "double precision",ScalarType "double precision"],ScalarType "boolean"),
    ("float8ge",[ScalarType "double precision",ScalarType "double precision"],ScalarType "boolean"),
    ("float8gt",[ScalarType "double precision",ScalarType "double precision"],ScalarType "boolean"),
    ("float8in",[ScalarType "cstring"],ScalarType "double precision"),
    ("float8larger",[ScalarType "double precision",ScalarType "double precision"],ScalarType "double precision"),
    ("float8le",[ScalarType "double precision",ScalarType "double precision"],ScalarType "boolean"),
    ("float8lt",[ScalarType "double precision",ScalarType "double precision"],ScalarType "boolean"),
    ("float8mi",[ScalarType "double precision",ScalarType "double precision"],ScalarType "double precision"),
    ("float8mul",[ScalarType "double precision",ScalarType "double precision"],ScalarType "double precision"),
    ("float8ne",[ScalarType "double precision",ScalarType "double precision"],ScalarType "boolean"),
    ("float8out",[ScalarType "double precision"],ScalarType "cstring"),
    ("float8pl",[ScalarType "double precision",ScalarType "double precision"],ScalarType "double precision"),
    ("float8recv",[ScalarType "internal"],ScalarType "double precision"),
    ("float8send",[ScalarType "double precision"],ScalarType "bytea"),
    ("float8smaller",[ScalarType "double precision",ScalarType "double precision"],ScalarType "double precision"),
    ("float8um",[ScalarType "double precision"],ScalarType "double precision"),
    ("float8up",[ScalarType "double precision"],ScalarType "double precision"),
    ("floor",[ScalarType "double precision"],ScalarType "double precision"),
    ("floor",[ScalarType "numeric"],ScalarType "numeric"),
    ("flt4_mul_cash",[ScalarType "real",ScalarType "money"],ScalarType "money"),
    ("flt8_mul_cash",[ScalarType "double precision",ScalarType "money"],ScalarType "money"),
    ("fmgr_c_validator",[ScalarType "oid"],ScalarType "void"),
    ("fmgr_internal_validator",[ScalarType "oid"],ScalarType "void"),
    ("fmgr_sql_validator",[ScalarType "oid"],ScalarType "void"),
    ("format_type",[ScalarType "oid",ScalarType "integer"],ScalarType "text"),
    ("gb18030_to_utf8",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("gbk_to_utf8",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("generate_series",[ScalarType "integer",ScalarType "integer"],ScalarType "SETOF integer"),
    ("generate_series",[ScalarType "integer",ScalarType "integer",ScalarType "integer"],ScalarType "SETOF integer"),
    ("generate_series",[ScalarType "timestamp without time zone",ScalarType "timestamp without time zone",ScalarType "interval"],ScalarType "SETOF timestamp without time zone"),
    ("generate_series",[ScalarType "bigint",ScalarType "bigint",ScalarType "bigint"],ScalarType "SETOF bigint"),
    ("generate_series",[ScalarType "timestamp with time zone",ScalarType "timestamp with time zone",ScalarType "interval"],ScalarType "SETOF timestamp with time zone"),
    ("generate_series",[ScalarType "bigint",ScalarType "bigint"],ScalarType "SETOF bigint"),
    ("generate_subscripts",[ScalarType "anyarray",ScalarType "integer"],ScalarType "SETOF integer"),
    ("generate_subscripts",[ScalarType "anyarray",ScalarType "integer",ScalarType "boolean"],ScalarType "SETOF integer"),
    ("get_bit",[ScalarType "bytea",ScalarType "integer"],ScalarType "integer"),
    ("get_byte",[ScalarType "bytea",ScalarType "integer"],ScalarType "integer"),
    ("get_current_ts_config",[],ScalarType "regconfig"),
    ("getdatabaseencoding",[],ScalarType "name"),
    ("getpgusername",[],ScalarType "name"),
    ("gin_cmp_prefix",[ScalarType "text",ScalarType "text",ScalarType "smallint",ScalarType "internal"],ScalarType "integer"),
    ("gin_cmp_tslexeme",[ScalarType "text",ScalarType "text"],ScalarType "integer"),
    ("gin_extract_tsquery",[ScalarType "tsquery",ScalarType "internal",ScalarType "smallint",ScalarType "internal",ScalarType "internal"],ScalarType "internal"),
    ("gin_extract_tsvector",[ScalarType "tsvector",ScalarType "internal"],ScalarType "internal"),
    ("gin_tsquery_consistent",[ScalarType "internal",ScalarType "smallint",ScalarType "tsquery",ScalarType "integer",ScalarType "internal",ScalarType "internal"],ScalarType "boolean"),
    ("ginarrayconsistent",[ScalarType "internal",ScalarType "smallint",ScalarType "anyarray",ScalarType "integer",ScalarType "internal",ScalarType "internal"],ScalarType "boolean"),
    ("ginarrayextract",[ScalarType "anyarray",ScalarType "internal"],ScalarType "internal"),
    ("ginbeginscan",[ScalarType "internal",ScalarType "internal",ScalarType "internal"],ScalarType "internal"),
    ("ginbuild",[ScalarType "internal",ScalarType "internal",ScalarType "internal"],ScalarType "internal"),
    ("ginbulkdelete",[ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal"],ScalarType "internal"),
    ("gincostestimate",[ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal"],ScalarType "void"),
    ("ginendscan",[ScalarType "internal"],ScalarType "void"),
    ("gingetbitmap",[ScalarType "internal",ScalarType "internal"],ScalarType "bigint"),
    ("gininsert",[ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal"],ScalarType "boolean"),
    ("ginmarkpos",[ScalarType "internal"],ScalarType "void"),
    ("ginoptions",[ScalarType "text[]",ScalarType "boolean"],ScalarType "bytea"),
    ("ginqueryarrayextract",[ScalarType "anyarray",ScalarType "internal",ScalarType "smallint",ScalarType "internal",ScalarType "internal"],ScalarType "internal"),
    ("ginrescan",[ScalarType "internal",ScalarType "internal"],ScalarType "void"),
    ("ginrestrpos",[ScalarType "internal"],ScalarType "void"),
    ("ginvacuumcleanup",[ScalarType "internal",ScalarType "internal"],ScalarType "internal"),
    ("gist_box_compress",[ScalarType "internal"],ScalarType "internal"),
    ("gist_box_consistent",[ScalarType "internal",ScalarType "box",ScalarType "integer",ScalarType "oid",ScalarType "internal"],ScalarType "boolean"),
    ("gist_box_decompress",[ScalarType "internal"],ScalarType "internal"),
    ("gist_box_penalty",[ScalarType "internal",ScalarType "internal",ScalarType "internal"],ScalarType "internal"),
    ("gist_box_picksplit",[ScalarType "internal",ScalarType "internal"],ScalarType "internal"),
    ("gist_box_same",[ScalarType "box",ScalarType "box",ScalarType "internal"],ScalarType "internal"),
    ("gist_box_union",[ScalarType "internal",ScalarType "internal"],ScalarType "box"),
    ("gist_circle_compress",[ScalarType "internal"],ScalarType "internal"),
    ("gist_circle_consistent",[ScalarType "internal",ScalarType "circle",ScalarType "integer",ScalarType "oid",ScalarType "internal"],ScalarType "boolean"),
    ("gist_poly_compress",[ScalarType "internal"],ScalarType "internal"),
    ("gist_poly_consistent",[ScalarType "internal",ScalarType "polygon",ScalarType "integer",ScalarType "oid",ScalarType "internal"],ScalarType "boolean"),
    ("gistbeginscan",[ScalarType "internal",ScalarType "internal",ScalarType "internal"],ScalarType "internal"),
    ("gistbuild",[ScalarType "internal",ScalarType "internal",ScalarType "internal"],ScalarType "internal"),
    ("gistbulkdelete",[ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal"],ScalarType "internal"),
    ("gistcostestimate",[ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal"],ScalarType "void"),
    ("gistendscan",[ScalarType "internal"],ScalarType "void"),
    ("gistgetbitmap",[ScalarType "internal",ScalarType "internal"],ScalarType "bigint"),
    ("gistgettuple",[ScalarType "internal",ScalarType "internal"],ScalarType "boolean"),
    ("gistinsert",[ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal"],ScalarType "boolean"),
    ("gistmarkpos",[ScalarType "internal"],ScalarType "void"),
    ("gistoptions",[ScalarType "text[]",ScalarType "boolean"],ScalarType "bytea"),
    ("gistrescan",[ScalarType "internal",ScalarType "internal"],ScalarType "void"),
    ("gistrestrpos",[ScalarType "internal"],ScalarType "void"),
    ("gistvacuumcleanup",[ScalarType "internal",ScalarType "internal"],ScalarType "internal"),
    ("gtsquery_compress",[ScalarType "internal"],ScalarType "internal"),
    ("gtsquery_consistent",[ScalarType "internal",ScalarType "internal",ScalarType "integer",ScalarType "oid",ScalarType "internal"],ScalarType "boolean"),
    ("gtsquery_decompress",[ScalarType "internal"],ScalarType "internal"),
    ("gtsquery_penalty",[ScalarType "internal",ScalarType "internal",ScalarType "internal"],ScalarType "internal"),
    ("gtsquery_picksplit",[ScalarType "internal",ScalarType "internal"],ScalarType "internal"),
    ("gtsquery_same",[ScalarType "bigint",ScalarType "bigint",ScalarType "internal"],ScalarType "internal"),
    ("gtsquery_union",[ScalarType "internal",ScalarType "internal"],ScalarType "internal"),
    ("gtsvector_compress",[ScalarType "internal"],ScalarType "internal"),
    ("gtsvector_consistent",[ScalarType "internal",ScalarType "gtsvector",ScalarType "integer",ScalarType "oid",ScalarType "internal"],ScalarType "boolean"),
    ("gtsvector_decompress",[ScalarType "internal"],ScalarType "internal"),
    ("gtsvector_penalty",[ScalarType "internal",ScalarType "internal",ScalarType "internal"],ScalarType "internal"),
    ("gtsvector_picksplit",[ScalarType "internal",ScalarType "internal"],ScalarType "internal"),
    ("gtsvector_same",[ScalarType "gtsvector",ScalarType "gtsvector",ScalarType "internal"],ScalarType "internal"),
    ("gtsvector_union",[ScalarType "internal",ScalarType "internal"],ScalarType "internal"),
    ("gtsvectorin",[ScalarType "cstring"],ScalarType "gtsvector"),
    ("gtsvectorout",[ScalarType "gtsvector"],ScalarType "cstring"),
    ("has_any_column_privilege",[ScalarType "name",ScalarType "oid",ScalarType "text"],ScalarType "boolean"),
    ("has_any_column_privilege",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("has_any_column_privilege",[ScalarType "name",ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("has_any_column_privilege",[ScalarType "oid",ScalarType "oid",ScalarType "text"],ScalarType "boolean"),
    ("has_any_column_privilege",[ScalarType "oid",ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("has_any_column_privilege",[ScalarType "oid",ScalarType "text"],ScalarType "boolean"),
    ("has_column_privilege",[ScalarType "oid",ScalarType "oid",ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("has_column_privilege",[ScalarType "oid",ScalarType "oid",ScalarType "smallint",ScalarType "text"],ScalarType "boolean"),
    ("has_column_privilege",[ScalarType "text",ScalarType "smallint",ScalarType "text"],ScalarType "boolean"),
    ("has_column_privilege",[ScalarType "text",ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("has_column_privilege",[ScalarType "oid",ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("has_column_privilege",[ScalarType "oid",ScalarType "smallint",ScalarType "text"],ScalarType "boolean"),
    ("has_column_privilege",[ScalarType "name",ScalarType "text",ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("has_column_privilege",[ScalarType "name",ScalarType "text",ScalarType "smallint",ScalarType "text"],ScalarType "boolean"),
    ("has_column_privilege",[ScalarType "name",ScalarType "oid",ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("has_column_privilege",[ScalarType "name",ScalarType "oid",ScalarType "smallint",ScalarType "text"],ScalarType "boolean"),
    ("has_column_privilege",[ScalarType "oid",ScalarType "text",ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("has_column_privilege",[ScalarType "oid",ScalarType "text",ScalarType "smallint",ScalarType "text"],ScalarType "boolean"),
    ("has_database_privilege",[ScalarType "name",ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("has_database_privilege",[ScalarType "oid",ScalarType "oid",ScalarType "text"],ScalarType "boolean"),
    ("has_database_privilege",[ScalarType "oid",ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("has_database_privilege",[ScalarType "name",ScalarType "oid",ScalarType "text"],ScalarType "boolean"),
    ("has_database_privilege",[ScalarType "oid",ScalarType "text"],ScalarType "boolean"),
    ("has_database_privilege",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("has_foreign_data_wrapper_privilege",[ScalarType "oid",ScalarType "oid",ScalarType "text"],ScalarType "boolean"),
    ("has_foreign_data_wrapper_privilege",[ScalarType "oid",ScalarType "text"],ScalarType "boolean"),
    ("has_foreign_data_wrapper_privilege",[ScalarType "name",ScalarType "oid",ScalarType "text"],ScalarType "boolean"),
    ("has_foreign_data_wrapper_privilege",[ScalarType "name",ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("has_foreign_data_wrapper_privilege",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("has_foreign_data_wrapper_privilege",[ScalarType "oid",ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("has_function_privilege",[ScalarType "oid",ScalarType "oid",ScalarType "text"],ScalarType "boolean"),
    ("has_function_privilege",[ScalarType "name",ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("has_function_privilege",[ScalarType "name",ScalarType "oid",ScalarType "text"],ScalarType "boolean"),
    ("has_function_privilege",[ScalarType "oid",ScalarType "text"],ScalarType "boolean"),
    ("has_function_privilege",[ScalarType "oid",ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("has_function_privilege",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("has_language_privilege",[ScalarType "oid",ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("has_language_privilege",[ScalarType "name",ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("has_language_privilege",[ScalarType "name",ScalarType "oid",ScalarType "text"],ScalarType "boolean"),
    ("has_language_privilege",[ScalarType "oid",ScalarType "oid",ScalarType "text"],ScalarType "boolean"),
    ("has_language_privilege",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("has_language_privilege",[ScalarType "oid",ScalarType "text"],ScalarType "boolean"),
    ("has_schema_privilege",[ScalarType "name",ScalarType "oid",ScalarType "text"],ScalarType "boolean"),
    ("has_schema_privilege",[ScalarType "oid",ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("has_schema_privilege",[ScalarType "oid",ScalarType "oid",ScalarType "text"],ScalarType "boolean"),
    ("has_schema_privilege",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("has_schema_privilege",[ScalarType "oid",ScalarType "text"],ScalarType "boolean"),
    ("has_schema_privilege",[ScalarType "name",ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("has_server_privilege",[ScalarType "name",ScalarType "oid",ScalarType "text"],ScalarType "boolean"),
    ("has_server_privilege",[ScalarType "name",ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("has_server_privilege",[ScalarType "oid",ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("has_server_privilege",[ScalarType "oid",ScalarType "oid",ScalarType "text"],ScalarType "boolean"),
    ("has_server_privilege",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("has_server_privilege",[ScalarType "oid",ScalarType "text"],ScalarType "boolean"),
    ("has_table_privilege",[ScalarType "oid",ScalarType "oid",ScalarType "text"],ScalarType "boolean"),
    ("has_table_privilege",[ScalarType "oid",ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("has_table_privilege",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("has_table_privilege",[ScalarType "name",ScalarType "oid",ScalarType "text"],ScalarType "boolean"),
    ("has_table_privilege",[ScalarType "oid",ScalarType "text"],ScalarType "boolean"),
    ("has_table_privilege",[ScalarType "name",ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("has_tablespace_privilege",[ScalarType "oid",ScalarType "text"],ScalarType "boolean"),
    ("has_tablespace_privilege",[ScalarType "name",ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("has_tablespace_privilege",[ScalarType "name",ScalarType "oid",ScalarType "text"],ScalarType "boolean"),
    ("has_tablespace_privilege",[ScalarType "oid",ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("has_tablespace_privilege",[ScalarType "oid",ScalarType "oid",ScalarType "text"],ScalarType "boolean"),
    ("has_tablespace_privilege",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("hash_aclitem",[ScalarType "aclitem"],ScalarType "integer"),
    ("hash_numeric",[ScalarType "numeric"],ScalarType "integer"),
    ("hashbeginscan",[ScalarType "internal",ScalarType "internal",ScalarType "internal"],ScalarType "internal"),
    ("hashbpchar",[ScalarType "character"],ScalarType "integer"),
    ("hashbuild",[ScalarType "internal",ScalarType "internal",ScalarType "internal"],ScalarType "internal"),
    ("hashbulkdelete",[ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal"],ScalarType "internal"),
    ("hashchar",[ScalarType "\"char\""],ScalarType "integer"),
    ("hashcostestimate",[ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal"],ScalarType "void"),
    ("hashendscan",[ScalarType "internal"],ScalarType "void"),
    ("hashenum",[ScalarType "anyenum"],ScalarType "integer"),
    ("hashfloat4",[ScalarType "real"],ScalarType "integer"),
    ("hashfloat8",[ScalarType "double precision"],ScalarType "integer"),
    ("hashgetbitmap",[ScalarType "internal",ScalarType "internal"],ScalarType "bigint"),
    ("hashgettuple",[ScalarType "internal",ScalarType "internal"],ScalarType "boolean"),
    ("hashinet",[ScalarType "inet"],ScalarType "integer"),
    ("hashinsert",[ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal"],ScalarType "boolean"),
    ("hashint2",[ScalarType "smallint"],ScalarType "integer"),
    ("hashint2vector",[ScalarType "int2vector"],ScalarType "integer"),
    ("hashint4",[ScalarType "integer"],ScalarType "integer"),
    ("hashint8",[ScalarType "bigint"],ScalarType "integer"),
    ("hashmacaddr",[ScalarType "macaddr"],ScalarType "integer"),
    ("hashmarkpos",[ScalarType "internal"],ScalarType "void"),
    ("hashname",[ScalarType "name"],ScalarType "integer"),
    ("hashoid",[ScalarType "oid"],ScalarType "integer"),
    ("hashoidvector",[ScalarType "oidvector"],ScalarType "integer"),
    ("hashoptions",[ScalarType "text[]",ScalarType "boolean"],ScalarType "bytea"),
    ("hashrescan",[ScalarType "internal",ScalarType "internal"],ScalarType "void"),
    ("hashrestrpos",[ScalarType "internal"],ScalarType "void"),
    ("hashtext",[ScalarType "text"],ScalarType "integer"),
    ("hashvacuumcleanup",[ScalarType "internal",ScalarType "internal"],ScalarType "internal"),
    ("hashvarlena",[ScalarType "internal"],ScalarType "integer"),
    ("height",[ScalarType "box"],ScalarType "double precision"),
    ("host",[ScalarType "inet"],ScalarType "text"),
    ("hostmask",[ScalarType "inet"],ScalarType "inet"),
    ("iclikejoinsel",[ScalarType "internal",ScalarType "oid",ScalarType "internal",ScalarType "smallint",ScalarType "internal"],ScalarType "double precision"),
    ("iclikesel",[ScalarType "internal",ScalarType "oid",ScalarType "internal",ScalarType "integer"],ScalarType "double precision"),
    ("icnlikejoinsel",[ScalarType "internal",ScalarType "oid",ScalarType "internal",ScalarType "smallint",ScalarType "internal"],ScalarType "double precision"),
    ("icnlikesel",[ScalarType "internal",ScalarType "oid",ScalarType "internal",ScalarType "integer"],ScalarType "double precision"),
    ("icregexeqjoinsel",[ScalarType "internal",ScalarType "oid",ScalarType "internal",ScalarType "smallint",ScalarType "internal"],ScalarType "double precision"),
    ("icregexeqsel",[ScalarType "internal",ScalarType "oid",ScalarType "internal",ScalarType "integer"],ScalarType "double precision"),
    ("icregexnejoinsel",[ScalarType "internal",ScalarType "oid",ScalarType "internal",ScalarType "smallint",ScalarType "internal"],ScalarType "double precision"),
    ("icregexnesel",[ScalarType "internal",ScalarType "oid",ScalarType "internal",ScalarType "integer"],ScalarType "double precision"),
    ("inet_client_addr",[],ScalarType "inet"),
    ("inet_client_port",[],ScalarType "integer"),
    ("inet_in",[ScalarType "cstring"],ScalarType "inet"),
    ("inet_out",[ScalarType "inet"],ScalarType "cstring"),
    ("inet_recv",[ScalarType "internal"],ScalarType "inet"),
    ("inet_send",[ScalarType "inet"],ScalarType "bytea"),
    ("inet_server_addr",[],ScalarType "inet"),
    ("inet_server_port",[],ScalarType "integer"),
    ("inetand",[ScalarType "inet",ScalarType "inet"],ScalarType "inet"),
    ("inetmi",[ScalarType "inet",ScalarType "inet"],ScalarType "bigint"),
    ("inetmi_int8",[ScalarType "inet",ScalarType "bigint"],ScalarType "inet"),
    ("inetnot",[ScalarType "inet"],ScalarType "inet"),
    ("inetor",[ScalarType "inet",ScalarType "inet"],ScalarType "inet"),
    ("inetpl",[ScalarType "inet",ScalarType "bigint"],ScalarType "inet"),
    ("initcap",[ScalarType "text"],ScalarType "text"),
    ("int2",[ScalarType "bigint"],ScalarType "smallint"),
    ("int2",[ScalarType "real"],ScalarType "smallint"),
    ("int2",[ScalarType "double precision"],ScalarType "smallint"),
    ("int2",[ScalarType "integer"],ScalarType "smallint"),
    ("int2",[ScalarType "numeric"],ScalarType "smallint"),
    ("int24div",[ScalarType "smallint",ScalarType "integer"],ScalarType "integer"),
    ("int24eq",[ScalarType "smallint",ScalarType "integer"],ScalarType "boolean"),
    ("int24ge",[ScalarType "smallint",ScalarType "integer"],ScalarType "boolean"),
    ("int24gt",[ScalarType "smallint",ScalarType "integer"],ScalarType "boolean"),
    ("int24le",[ScalarType "smallint",ScalarType "integer"],ScalarType "boolean"),
    ("int24lt",[ScalarType "smallint",ScalarType "integer"],ScalarType "boolean"),
    ("int24mi",[ScalarType "smallint",ScalarType "integer"],ScalarType "integer"),
    ("int24mul",[ScalarType "smallint",ScalarType "integer"],ScalarType "integer"),
    ("int24ne",[ScalarType "smallint",ScalarType "integer"],ScalarType "boolean"),
    ("int24pl",[ScalarType "smallint",ScalarType "integer"],ScalarType "integer"),
    ("int28div",[ScalarType "smallint",ScalarType "bigint"],ScalarType "bigint"),
    ("int28eq",[ScalarType "smallint",ScalarType "bigint"],ScalarType "boolean"),
    ("int28ge",[ScalarType "smallint",ScalarType "bigint"],ScalarType "boolean"),
    ("int28gt",[ScalarType "smallint",ScalarType "bigint"],ScalarType "boolean"),
    ("int28le",[ScalarType "smallint",ScalarType "bigint"],ScalarType "boolean"),
    ("int28lt",[ScalarType "smallint",ScalarType "bigint"],ScalarType "boolean"),
    ("int28mi",[ScalarType "smallint",ScalarType "bigint"],ScalarType "bigint"),
    ("int28mul",[ScalarType "smallint",ScalarType "bigint"],ScalarType "bigint"),
    ("int28ne",[ScalarType "smallint",ScalarType "bigint"],ScalarType "boolean"),
    ("int28pl",[ScalarType "smallint",ScalarType "bigint"],ScalarType "bigint"),
    ("int2_accum",[ScalarType "numeric[]",ScalarType "smallint"],ScalarType "numeric[]"),
    ("int2_avg_accum",[ScalarType "bigint[]",ScalarType "smallint"],ScalarType "bigint[]"),
    ("int2_mul_cash",[ScalarType "smallint",ScalarType "money"],ScalarType "money"),
    ("int2_sum",[ScalarType "bigint",ScalarType "smallint"],ScalarType "bigint"),
    ("int2abs",[ScalarType "smallint"],ScalarType "smallint"),
    ("int2and",[ScalarType "smallint",ScalarType "smallint"],ScalarType "smallint"),
    ("int2div",[ScalarType "smallint",ScalarType "smallint"],ScalarType "smallint"),
    ("int2eq",[ScalarType "smallint",ScalarType "smallint"],ScalarType "boolean"),
    ("int2ge",[ScalarType "smallint",ScalarType "smallint"],ScalarType "boolean"),
    ("int2gt",[ScalarType "smallint",ScalarType "smallint"],ScalarType "boolean"),
    ("int2in",[ScalarType "cstring"],ScalarType "smallint"),
    ("int2larger",[ScalarType "smallint",ScalarType "smallint"],ScalarType "smallint"),
    ("int2le",[ScalarType "smallint",ScalarType "smallint"],ScalarType "boolean"),
    ("int2lt",[ScalarType "smallint",ScalarType "smallint"],ScalarType "boolean"),
    ("int2mi",[ScalarType "smallint",ScalarType "smallint"],ScalarType "smallint"),
    ("int2mod",[ScalarType "smallint",ScalarType "smallint"],ScalarType "smallint"),
    ("int2mul",[ScalarType "smallint",ScalarType "smallint"],ScalarType "smallint"),
    ("int2ne",[ScalarType "smallint",ScalarType "smallint"],ScalarType "boolean"),
    ("int2not",[ScalarType "smallint"],ScalarType "smallint"),
    ("int2or",[ScalarType "smallint",ScalarType "smallint"],ScalarType "smallint"),
    ("int2out",[ScalarType "smallint"],ScalarType "cstring"),
    ("int2pl",[ScalarType "smallint",ScalarType "smallint"],ScalarType "smallint"),
    ("int2recv",[ScalarType "internal"],ScalarType "smallint"),
    ("int2send",[ScalarType "smallint"],ScalarType "bytea"),
    ("int2shl",[ScalarType "smallint",ScalarType "integer"],ScalarType "smallint"),
    ("int2shr",[ScalarType "smallint",ScalarType "integer"],ScalarType "smallint"),
    ("int2smaller",[ScalarType "smallint",ScalarType "smallint"],ScalarType "smallint"),
    ("int2um",[ScalarType "smallint"],ScalarType "smallint"),
    ("int2up",[ScalarType "smallint"],ScalarType "smallint"),
    ("int2vectoreq",[ScalarType "int2vector",ScalarType "int2vector"],ScalarType "boolean"),
    ("int2vectorin",[ScalarType "cstring"],ScalarType "int2vector"),
    ("int2vectorout",[ScalarType "int2vector"],ScalarType "cstring"),
    ("int2vectorrecv",[ScalarType "internal"],ScalarType "int2vector"),
    ("int2vectorsend",[ScalarType "int2vector"],ScalarType "bytea"),
    ("int2xor",[ScalarType "smallint",ScalarType "smallint"],ScalarType "smallint"),
    ("int4",[ScalarType "\"char\""],ScalarType "integer"),
    ("int4",[ScalarType "boolean"],ScalarType "integer"),
    ("int4",[ScalarType "bigint"],ScalarType "integer"),
    ("int4",[ScalarType "real"],ScalarType "integer"),
    ("int4",[ScalarType "double precision"],ScalarType "integer"),
    ("int4",[ScalarType "smallint"],ScalarType "integer"),
    ("int4",[ScalarType "bit"],ScalarType "integer"),
    ("int4",[ScalarType "numeric"],ScalarType "integer"),
    ("int42div",[ScalarType "integer",ScalarType "smallint"],ScalarType "integer"),
    ("int42eq",[ScalarType "integer",ScalarType "smallint"],ScalarType "boolean"),
    ("int42ge",[ScalarType "integer",ScalarType "smallint"],ScalarType "boolean"),
    ("int42gt",[ScalarType "integer",ScalarType "smallint"],ScalarType "boolean"),
    ("int42le",[ScalarType "integer",ScalarType "smallint"],ScalarType "boolean"),
    ("int42lt",[ScalarType "integer",ScalarType "smallint"],ScalarType "boolean"),
    ("int42mi",[ScalarType "integer",ScalarType "smallint"],ScalarType "integer"),
    ("int42mul",[ScalarType "integer",ScalarType "smallint"],ScalarType "integer"),
    ("int42ne",[ScalarType "integer",ScalarType "smallint"],ScalarType "boolean"),
    ("int42pl",[ScalarType "integer",ScalarType "smallint"],ScalarType "integer"),
    ("int48div",[ScalarType "integer",ScalarType "bigint"],ScalarType "bigint"),
    ("int48eq",[ScalarType "integer",ScalarType "bigint"],ScalarType "boolean"),
    ("int48ge",[ScalarType "integer",ScalarType "bigint"],ScalarType "boolean"),
    ("int48gt",[ScalarType "integer",ScalarType "bigint"],ScalarType "boolean"),
    ("int48le",[ScalarType "integer",ScalarType "bigint"],ScalarType "boolean"),
    ("int48lt",[ScalarType "integer",ScalarType "bigint"],ScalarType "boolean"),
    ("int48mi",[ScalarType "integer",ScalarType "bigint"],ScalarType "bigint"),
    ("int48mul",[ScalarType "integer",ScalarType "bigint"],ScalarType "bigint"),
    ("int48ne",[ScalarType "integer",ScalarType "bigint"],ScalarType "boolean"),
    ("int48pl",[ScalarType "integer",ScalarType "bigint"],ScalarType "bigint"),
    ("int4_accum",[ScalarType "numeric[]",ScalarType "integer"],ScalarType "numeric[]"),
    ("int4_avg_accum",[ScalarType "bigint[]",ScalarType "integer"],ScalarType "bigint[]"),
    ("int4_mul_cash",[ScalarType "integer",ScalarType "money"],ScalarType "money"),
    ("int4_sum",[ScalarType "bigint",ScalarType "integer"],ScalarType "bigint"),
    ("int4abs",[ScalarType "integer"],ScalarType "integer"),
    ("int4and",[ScalarType "integer",ScalarType "integer"],ScalarType "integer"),
    ("int4div",[ScalarType "integer",ScalarType "integer"],ScalarType "integer"),
    ("int4eq",[ScalarType "integer",ScalarType "integer"],ScalarType "boolean"),
    ("int4ge",[ScalarType "integer",ScalarType "integer"],ScalarType "boolean"),
    ("int4gt",[ScalarType "integer",ScalarType "integer"],ScalarType "boolean"),
    ("int4in",[ScalarType "cstring"],ScalarType "integer"),
    ("int4inc",[ScalarType "integer"],ScalarType "integer"),
    ("int4larger",[ScalarType "integer",ScalarType "integer"],ScalarType "integer"),
    ("int4le",[ScalarType "integer",ScalarType "integer"],ScalarType "boolean"),
    ("int4lt",[ScalarType "integer",ScalarType "integer"],ScalarType "boolean"),
    ("int4mi",[ScalarType "integer",ScalarType "integer"],ScalarType "integer"),
    ("int4mod",[ScalarType "integer",ScalarType "integer"],ScalarType "integer"),
    ("int4mul",[ScalarType "integer",ScalarType "integer"],ScalarType "integer"),
    ("int4ne",[ScalarType "integer",ScalarType "integer"],ScalarType "boolean"),
    ("int4not",[ScalarType "integer"],ScalarType "integer"),
    ("int4or",[ScalarType "integer",ScalarType "integer"],ScalarType "integer"),
    ("int4out",[ScalarType "integer"],ScalarType "cstring"),
    ("int4pl",[ScalarType "integer",ScalarType "integer"],ScalarType "integer"),
    ("int4recv",[ScalarType "internal"],ScalarType "integer"),
    ("int4send",[ScalarType "integer"],ScalarType "bytea"),
    ("int4shl",[ScalarType "integer",ScalarType "integer"],ScalarType "integer"),
    ("int4shr",[ScalarType "integer",ScalarType "integer"],ScalarType "integer"),
    ("int4smaller",[ScalarType "integer",ScalarType "integer"],ScalarType "integer"),
    ("int4um",[ScalarType "integer"],ScalarType "integer"),
    ("int4up",[ScalarType "integer"],ScalarType "integer"),
    ("int4xor",[ScalarType "integer",ScalarType "integer"],ScalarType "integer"),
    ("int8",[ScalarType "integer"],ScalarType "bigint"),
    ("int8",[ScalarType "oid"],ScalarType "bigint"),
    ("int8",[ScalarType "bit"],ScalarType "bigint"),
    ("int8",[ScalarType "numeric"],ScalarType "bigint"),
    ("int8",[ScalarType "double precision"],ScalarType "bigint"),
    ("int8",[ScalarType "smallint"],ScalarType "bigint"),
    ("int8",[ScalarType "real"],ScalarType "bigint"),
    ("int82div",[ScalarType "bigint",ScalarType "smallint"],ScalarType "bigint"),
    ("int82eq",[ScalarType "bigint",ScalarType "smallint"],ScalarType "boolean"),
    ("int82ge",[ScalarType "bigint",ScalarType "smallint"],ScalarType "boolean"),
    ("int82gt",[ScalarType "bigint",ScalarType "smallint"],ScalarType "boolean"),
    ("int82le",[ScalarType "bigint",ScalarType "smallint"],ScalarType "boolean"),
    ("int82lt",[ScalarType "bigint",ScalarType "smallint"],ScalarType "boolean"),
    ("int82mi",[ScalarType "bigint",ScalarType "smallint"],ScalarType "bigint"),
    ("int82mul",[ScalarType "bigint",ScalarType "smallint"],ScalarType "bigint"),
    ("int82ne",[ScalarType "bigint",ScalarType "smallint"],ScalarType "boolean"),
    ("int82pl",[ScalarType "bigint",ScalarType "smallint"],ScalarType "bigint"),
    ("int84div",[ScalarType "bigint",ScalarType "integer"],ScalarType "bigint"),
    ("int84eq",[ScalarType "bigint",ScalarType "integer"],ScalarType "boolean"),
    ("int84ge",[ScalarType "bigint",ScalarType "integer"],ScalarType "boolean"),
    ("int84gt",[ScalarType "bigint",ScalarType "integer"],ScalarType "boolean"),
    ("int84le",[ScalarType "bigint",ScalarType "integer"],ScalarType "boolean"),
    ("int84lt",[ScalarType "bigint",ScalarType "integer"],ScalarType "boolean"),
    ("int84mi",[ScalarType "bigint",ScalarType "integer"],ScalarType "bigint"),
    ("int84mul",[ScalarType "bigint",ScalarType "integer"],ScalarType "bigint"),
    ("int84ne",[ScalarType "bigint",ScalarType "integer"],ScalarType "boolean"),
    ("int84pl",[ScalarType "bigint",ScalarType "integer"],ScalarType "bigint"),
    ("int8_accum",[ScalarType "numeric[]",ScalarType "bigint"],ScalarType "numeric[]"),
    ("int8_avg",[ScalarType "bigint[]"],ScalarType "numeric"),
    ("int8_avg_accum",[ScalarType "numeric[]",ScalarType "bigint"],ScalarType "numeric[]"),
    ("int8_sum",[ScalarType "numeric",ScalarType "bigint"],ScalarType "numeric"),
    ("int8abs",[ScalarType "bigint"],ScalarType "bigint"),
    ("int8and",[ScalarType "bigint",ScalarType "bigint"],ScalarType "bigint"),
    ("int8div",[ScalarType "bigint",ScalarType "bigint"],ScalarType "bigint"),
    ("int8eq",[ScalarType "bigint",ScalarType "bigint"],ScalarType "boolean"),
    ("int8ge",[ScalarType "bigint",ScalarType "bigint"],ScalarType "boolean"),
    ("int8gt",[ScalarType "bigint",ScalarType "bigint"],ScalarType "boolean"),
    ("int8in",[ScalarType "cstring"],ScalarType "bigint"),
    ("int8inc",[ScalarType "bigint"],ScalarType "bigint"),
    ("int8inc_any",[ScalarType "bigint",ScalarType "\"any\""],ScalarType "bigint"),
    ("int8inc_float8_float8",[ScalarType "bigint",ScalarType "double precision",ScalarType "double precision"],ScalarType "bigint"),
    ("int8larger",[ScalarType "bigint",ScalarType "bigint"],ScalarType "bigint"),
    ("int8le",[ScalarType "bigint",ScalarType "bigint"],ScalarType "boolean"),
    ("int8lt",[ScalarType "bigint",ScalarType "bigint"],ScalarType "boolean"),
    ("int8mi",[ScalarType "bigint",ScalarType "bigint"],ScalarType "bigint"),
    ("int8mod",[ScalarType "bigint",ScalarType "bigint"],ScalarType "bigint"),
    ("int8mul",[ScalarType "bigint",ScalarType "bigint"],ScalarType "bigint"),
    ("int8ne",[ScalarType "bigint",ScalarType "bigint"],ScalarType "boolean"),
    ("int8not",[ScalarType "bigint"],ScalarType "bigint"),
    ("int8or",[ScalarType "bigint",ScalarType "bigint"],ScalarType "bigint"),
    ("int8out",[ScalarType "bigint"],ScalarType "cstring"),
    ("int8pl",[ScalarType "bigint",ScalarType "bigint"],ScalarType "bigint"),
    ("int8pl_inet",[ScalarType "bigint",ScalarType "inet"],ScalarType "inet"),
    ("int8recv",[ScalarType "internal"],ScalarType "bigint"),
    ("int8send",[ScalarType "bigint"],ScalarType "bytea"),
    ("int8shl",[ScalarType "bigint",ScalarType "integer"],ScalarType "bigint"),
    ("int8shr",[ScalarType "bigint",ScalarType "integer"],ScalarType "bigint"),
    ("int8smaller",[ScalarType "bigint",ScalarType "bigint"],ScalarType "bigint"),
    ("int8um",[ScalarType "bigint"],ScalarType "bigint"),
    ("int8up",[ScalarType "bigint"],ScalarType "bigint"),
    ("int8xor",[ScalarType "bigint",ScalarType "bigint"],ScalarType "bigint"),
    ("integer_pl_date",[ScalarType "integer",ScalarType "date"],ScalarType "date"),
    ("inter_lb",[ScalarType "line",ScalarType "box"],ScalarType "boolean"),
    ("inter_sb",[ScalarType "lseg",ScalarType "box"],ScalarType "boolean"),
    ("inter_sl",[ScalarType "lseg",ScalarType "line"],ScalarType "boolean"),
    ("internal_in",[ScalarType "cstring"],ScalarType "internal"),
    ("internal_out",[ScalarType "internal"],ScalarType "cstring"),
    ("interval",[ScalarType "reltime"],ScalarType "interval"),
    ("interval",[ScalarType "time without time zone"],ScalarType "interval"),
    ("interval",[ScalarType "interval",ScalarType "integer"],ScalarType "interval"),
    ("interval_accum",[ScalarType "interval[]",ScalarType "interval"],ScalarType "interval[]"),
    ("interval_avg",[ScalarType "interval[]"],ScalarType "interval"),
    ("interval_cmp",[ScalarType "interval",ScalarType "interval"],ScalarType "integer"),
    ("interval_div",[ScalarType "interval",ScalarType "double precision"],ScalarType "interval"),
    ("interval_eq",[ScalarType "interval",ScalarType "interval"],ScalarType "boolean"),
    ("interval_ge",[ScalarType "interval",ScalarType "interval"],ScalarType "boolean"),
    ("interval_gt",[ScalarType "interval",ScalarType "interval"],ScalarType "boolean"),
    ("interval_hash",[ScalarType "interval"],ScalarType "integer"),
    ("interval_in",[ScalarType "cstring",ScalarType "oid",ScalarType "integer"],ScalarType "interval"),
    ("interval_larger",[ScalarType "interval",ScalarType "interval"],ScalarType "interval"),
    ("interval_le",[ScalarType "interval",ScalarType "interval"],ScalarType "boolean"),
    ("interval_lt",[ScalarType "interval",ScalarType "interval"],ScalarType "boolean"),
    ("interval_mi",[ScalarType "interval",ScalarType "interval"],ScalarType "interval"),
    ("interval_mul",[ScalarType "interval",ScalarType "double precision"],ScalarType "interval"),
    ("interval_ne",[ScalarType "interval",ScalarType "interval"],ScalarType "boolean"),
    ("interval_out",[ScalarType "interval"],ScalarType "cstring"),
    ("interval_pl",[ScalarType "interval",ScalarType "interval"],ScalarType "interval"),
    ("interval_pl_date",[ScalarType "interval",ScalarType "date"],ScalarType "timestamp without time zone"),
    ("interval_pl_time",[ScalarType "interval",ScalarType "time without time zone"],ScalarType "time without time zone"),
    ("interval_pl_timestamp",[ScalarType "interval",ScalarType "timestamp without time zone"],ScalarType "timestamp without time zone"),
    ("interval_pl_timestamptz",[ScalarType "interval",ScalarType "timestamp with time zone"],ScalarType "timestamp with time zone"),
    ("interval_pl_timetz",[ScalarType "interval",ScalarType "time with time zone"],ScalarType "time with time zone"),
    ("interval_recv",[ScalarType "internal",ScalarType "oid",ScalarType "integer"],ScalarType "interval"),
    ("interval_send",[ScalarType "interval"],ScalarType "bytea"),
    ("interval_smaller",[ScalarType "interval",ScalarType "interval"],ScalarType "interval"),
    ("interval_um",[ScalarType "interval"],ScalarType "interval"),
    ("intervaltypmodin",[ScalarType "cstring[]"],ScalarType "integer"),
    ("intervaltypmodout",[ScalarType "integer"],ScalarType "cstring"),
    ("intinterval",[ScalarType "abstime",ScalarType "tinterval"],ScalarType "boolean"),
    ("isclosed",[ScalarType "path"],ScalarType "boolean"),
    ("isfinite",[ScalarType "abstime"],ScalarType "boolean"),
    ("isfinite",[ScalarType "interval"],ScalarType "boolean"),
    ("isfinite",[ScalarType "timestamp with time zone"],ScalarType "boolean"),
    ("isfinite",[ScalarType "timestamp without time zone"],ScalarType "boolean"),
    ("isfinite",[ScalarType "date"],ScalarType "boolean"),
    ("ishorizontal",[ScalarType "lseg"],ScalarType "boolean"),
    ("ishorizontal",[ScalarType "point",ScalarType "point"],ScalarType "boolean"),
    ("ishorizontal",[ScalarType "line"],ScalarType "boolean"),
    ("iso8859_1_to_utf8",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("iso8859_to_utf8",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("iso_to_koi8r",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("iso_to_mic",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("iso_to_win1251",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("iso_to_win866",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("isopen",[ScalarType "path"],ScalarType "boolean"),
    ("isparallel",[ScalarType "line",ScalarType "line"],ScalarType "boolean"),
    ("isparallel",[ScalarType "lseg",ScalarType "lseg"],ScalarType "boolean"),
    ("isperp",[ScalarType "line",ScalarType "line"],ScalarType "boolean"),
    ("isperp",[ScalarType "lseg",ScalarType "lseg"],ScalarType "boolean"),
    ("isvertical",[ScalarType "lseg"],ScalarType "boolean"),
    ("isvertical",[ScalarType "line"],ScalarType "boolean"),
    ("isvertical",[ScalarType "point",ScalarType "point"],ScalarType "boolean"),
    ("johab_to_utf8",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("justify_days",[ScalarType "interval"],ScalarType "interval"),
    ("justify_hours",[ScalarType "interval"],ScalarType "interval"),
    ("justify_interval",[ScalarType "interval"],ScalarType "interval"),
    ("koi8r_to_iso",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("koi8r_to_mic",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("koi8r_to_utf8",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("koi8r_to_win1251",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("koi8r_to_win866",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("koi8u_to_utf8",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("language_handler_in",[ScalarType "cstring"],ScalarType "language_handler"),
    ("language_handler_out",[ScalarType "language_handler"],ScalarType "cstring"),
    ("lastval",[],ScalarType "bigint"),
    ("latin1_to_mic",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("latin2_to_mic",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("latin2_to_win1250",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("latin3_to_mic",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("latin4_to_mic",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("length",[ScalarType "tsvector"],ScalarType "integer"),
    ("length",[ScalarType "text"],ScalarType "integer"),
    ("length",[ScalarType "lseg"],ScalarType "double precision"),
    ("length",[ScalarType "character"],ScalarType "integer"),
    ("length",[ScalarType "path"],ScalarType "double precision"),
    ("length",[ScalarType "bytea",ScalarType "name"],ScalarType "integer"),
    ("length",[ScalarType "bit"],ScalarType "integer"),
    ("length",[ScalarType "bytea"],ScalarType "integer"),
    ("like",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("like",[ScalarType "bytea",ScalarType "bytea"],ScalarType "boolean"),
    ("like",[ScalarType "name",ScalarType "text"],ScalarType "boolean"),
    ("like_escape",[ScalarType "bytea",ScalarType "bytea"],ScalarType "bytea"),
    ("like_escape",[ScalarType "text",ScalarType "text"],ScalarType "text"),
    ("likejoinsel",[ScalarType "internal",ScalarType "oid",ScalarType "internal",ScalarType "smallint",ScalarType "internal"],ScalarType "double precision"),
    ("likesel",[ScalarType "internal",ScalarType "oid",ScalarType "internal",ScalarType "integer"],ScalarType "double precision"),
    ("line",[ScalarType "point",ScalarType "point"],ScalarType "line"),
    ("line_distance",[ScalarType "line",ScalarType "line"],ScalarType "double precision"),
    ("line_eq",[ScalarType "line",ScalarType "line"],ScalarType "boolean"),
    ("line_horizontal",[ScalarType "line"],ScalarType "boolean"),
    ("line_in",[ScalarType "cstring"],ScalarType "line"),
    ("line_interpt",[ScalarType "line",ScalarType "line"],ScalarType "point"),
    ("line_intersect",[ScalarType "line",ScalarType "line"],ScalarType "boolean"),
    ("line_out",[ScalarType "line"],ScalarType "cstring"),
    ("line_parallel",[ScalarType "line",ScalarType "line"],ScalarType "boolean"),
    ("line_perp",[ScalarType "line",ScalarType "line"],ScalarType "boolean"),
    ("line_recv",[ScalarType "internal"],ScalarType "line"),
    ("line_send",[ScalarType "line"],ScalarType "bytea"),
    ("line_vertical",[ScalarType "line"],ScalarType "boolean"),
    ("ln",[ScalarType "numeric"],ScalarType "numeric"),
    ("ln",[ScalarType "double precision"],ScalarType "double precision"),
    ("lo_close",[ScalarType "integer"],ScalarType "integer"),
    ("lo_creat",[ScalarType "integer"],ScalarType "oid"),
    ("lo_create",[ScalarType "oid"],ScalarType "oid"),
    ("lo_export",[ScalarType "oid",ScalarType "text"],ScalarType "integer"),
    ("lo_import",[ScalarType "text"],ScalarType "oid"),
    ("lo_import",[ScalarType "text",ScalarType "oid"],ScalarType "oid"),
    ("lo_lseek",[ScalarType "integer",ScalarType "integer",ScalarType "integer"],ScalarType "integer"),
    ("lo_open",[ScalarType "oid",ScalarType "integer"],ScalarType "integer"),
    ("lo_tell",[ScalarType "integer"],ScalarType "integer"),
    ("lo_truncate",[ScalarType "integer",ScalarType "integer"],ScalarType "integer"),
    ("lo_unlink",[ScalarType "oid"],ScalarType "integer"),
    ("log",[ScalarType "double precision"],ScalarType "double precision"),
    ("log",[ScalarType "numeric"],ScalarType "numeric"),
    ("log",[ScalarType "numeric",ScalarType "numeric"],ScalarType "numeric"),
    ("loread",[ScalarType "integer",ScalarType "integer"],ScalarType "bytea"),
    ("lower",[ScalarType "text"],ScalarType "text"),
    ("lowrite",[ScalarType "integer",ScalarType "bytea"],ScalarType "integer"),
    ("lpad",[ScalarType "text",ScalarType "integer",ScalarType "text"],ScalarType "text"),
    ("lpad",[ScalarType "text",ScalarType "integer"],ScalarType "text"),
    ("lseg",[ScalarType "box"],ScalarType "lseg"),
    ("lseg",[ScalarType "point",ScalarType "point"],ScalarType "lseg"),
    ("lseg_center",[ScalarType "lseg"],ScalarType "point"),
    ("lseg_distance",[ScalarType "lseg",ScalarType "lseg"],ScalarType "double precision"),
    ("lseg_eq",[ScalarType "lseg",ScalarType "lseg"],ScalarType "boolean"),
    ("lseg_ge",[ScalarType "lseg",ScalarType "lseg"],ScalarType "boolean"),
    ("lseg_gt",[ScalarType "lseg",ScalarType "lseg"],ScalarType "boolean"),
    ("lseg_horizontal",[ScalarType "lseg"],ScalarType "boolean"),
    ("lseg_in",[ScalarType "cstring"],ScalarType "lseg"),
    ("lseg_interpt",[ScalarType "lseg",ScalarType "lseg"],ScalarType "point"),
    ("lseg_intersect",[ScalarType "lseg",ScalarType "lseg"],ScalarType "boolean"),
    ("lseg_le",[ScalarType "lseg",ScalarType "lseg"],ScalarType "boolean"),
    ("lseg_length",[ScalarType "lseg"],ScalarType "double precision"),
    ("lseg_lt",[ScalarType "lseg",ScalarType "lseg"],ScalarType "boolean"),
    ("lseg_ne",[ScalarType "lseg",ScalarType "lseg"],ScalarType "boolean"),
    ("lseg_out",[ScalarType "lseg"],ScalarType "cstring"),
    ("lseg_parallel",[ScalarType "lseg",ScalarType "lseg"],ScalarType "boolean"),
    ("lseg_perp",[ScalarType "lseg",ScalarType "lseg"],ScalarType "boolean"),
    ("lseg_recv",[ScalarType "internal"],ScalarType "lseg"),
    ("lseg_send",[ScalarType "lseg"],ScalarType "bytea"),
    ("lseg_vertical",[ScalarType "lseg"],ScalarType "boolean"),
    ("ltrim",[ScalarType "text",ScalarType "text"],ScalarType "text"),
    ("ltrim",[ScalarType "text"],ScalarType "text"),
    ("macaddr_cmp",[ScalarType "macaddr",ScalarType "macaddr"],ScalarType "integer"),
    ("macaddr_eq",[ScalarType "macaddr",ScalarType "macaddr"],ScalarType "boolean"),
    ("macaddr_ge",[ScalarType "macaddr",ScalarType "macaddr"],ScalarType "boolean"),
    ("macaddr_gt",[ScalarType "macaddr",ScalarType "macaddr"],ScalarType "boolean"),
    ("macaddr_in",[ScalarType "cstring"],ScalarType "macaddr"),
    ("macaddr_le",[ScalarType "macaddr",ScalarType "macaddr"],ScalarType "boolean"),
    ("macaddr_lt",[ScalarType "macaddr",ScalarType "macaddr"],ScalarType "boolean"),
    ("macaddr_ne",[ScalarType "macaddr",ScalarType "macaddr"],ScalarType "boolean"),
    ("macaddr_out",[ScalarType "macaddr"],ScalarType "cstring"),
    ("macaddr_recv",[ScalarType "internal"],ScalarType "macaddr"),
    ("macaddr_send",[ScalarType "macaddr"],ScalarType "bytea"),
    ("makeaclitem",[ScalarType "oid",ScalarType "oid",ScalarType "text",ScalarType "boolean"],ScalarType "aclitem"),
    ("masklen",[ScalarType "inet"],ScalarType "integer"),
    ("md5",[ScalarType "bytea"],ScalarType "text"),
    ("md5",[ScalarType "text"],ScalarType "text"),
    ("mic_to_ascii",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("mic_to_big5",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("mic_to_euc_cn",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("mic_to_euc_jp",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("mic_to_euc_kr",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("mic_to_euc_tw",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("mic_to_iso",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("mic_to_koi8r",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("mic_to_latin1",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("mic_to_latin2",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("mic_to_latin3",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("mic_to_latin4",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("mic_to_sjis",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("mic_to_win1250",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("mic_to_win1251",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("mic_to_win866",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("mktinterval",[ScalarType "abstime",ScalarType "abstime"],ScalarType "tinterval"),
    ("mod",[ScalarType "bigint",ScalarType "bigint"],ScalarType "bigint"),
    ("mod",[ScalarType "numeric",ScalarType "numeric"],ScalarType "numeric"),
    ("mod",[ScalarType "integer",ScalarType "integer"],ScalarType "integer"),
    ("mod",[ScalarType "smallint",ScalarType "smallint"],ScalarType "smallint"),
    ("mul_d_interval",[ScalarType "double precision",ScalarType "interval"],ScalarType "interval"),
    ("name",[ScalarType "character"],ScalarType "name"),
    ("name",[ScalarType "text"],ScalarType "name"),
    ("name",[ScalarType "character varying"],ScalarType "name"),
    ("nameeq",[ScalarType "name",ScalarType "name"],ScalarType "boolean"),
    ("namege",[ScalarType "name",ScalarType "name"],ScalarType "boolean"),
    ("namegt",[ScalarType "name",ScalarType "name"],ScalarType "boolean"),
    ("nameiclike",[ScalarType "name",ScalarType "text"],ScalarType "boolean"),
    ("nameicnlike",[ScalarType "name",ScalarType "text"],ScalarType "boolean"),
    ("nameicregexeq",[ScalarType "name",ScalarType "text"],ScalarType "boolean"),
    ("nameicregexne",[ScalarType "name",ScalarType "text"],ScalarType "boolean"),
    ("namein",[ScalarType "cstring"],ScalarType "name"),
    ("namele",[ScalarType "name",ScalarType "name"],ScalarType "boolean"),
    ("namelike",[ScalarType "name",ScalarType "text"],ScalarType "boolean"),
    ("namelt",[ScalarType "name",ScalarType "name"],ScalarType "boolean"),
    ("namene",[ScalarType "name",ScalarType "name"],ScalarType "boolean"),
    ("namenlike",[ScalarType "name",ScalarType "text"],ScalarType "boolean"),
    ("nameout",[ScalarType "name"],ScalarType "cstring"),
    ("namerecv",[ScalarType "internal"],ScalarType "name"),
    ("nameregexeq",[ScalarType "name",ScalarType "text"],ScalarType "boolean"),
    ("nameregexne",[ScalarType "name",ScalarType "text"],ScalarType "boolean"),
    ("namesend",[ScalarType "name"],ScalarType "bytea"),
    ("neqjoinsel",[ScalarType "internal",ScalarType "oid",ScalarType "internal",ScalarType "smallint",ScalarType "internal"],ScalarType "double precision"),
    ("neqsel",[ScalarType "internal",ScalarType "oid",ScalarType "internal",ScalarType "integer"],ScalarType "double precision"),
    ("netmask",[ScalarType "inet"],ScalarType "inet"),
    ("network",[ScalarType "inet"],ScalarType "cidr"),
    ("network_cmp",[ScalarType "inet",ScalarType "inet"],ScalarType "integer"),
    ("network_eq",[ScalarType "inet",ScalarType "inet"],ScalarType "boolean"),
    ("network_ge",[ScalarType "inet",ScalarType "inet"],ScalarType "boolean"),
    ("network_gt",[ScalarType "inet",ScalarType "inet"],ScalarType "boolean"),
    ("network_le",[ScalarType "inet",ScalarType "inet"],ScalarType "boolean"),
    ("network_lt",[ScalarType "inet",ScalarType "inet"],ScalarType "boolean"),
    ("network_ne",[ScalarType "inet",ScalarType "inet"],ScalarType "boolean"),
    ("network_sub",[ScalarType "inet",ScalarType "inet"],ScalarType "boolean"),
    ("network_subeq",[ScalarType "inet",ScalarType "inet"],ScalarType "boolean"),
    ("network_sup",[ScalarType "inet",ScalarType "inet"],ScalarType "boolean"),
    ("network_supeq",[ScalarType "inet",ScalarType "inet"],ScalarType "boolean"),
    ("nextval",[ScalarType "regclass"],ScalarType "bigint"),
    ("nlikejoinsel",[ScalarType "internal",ScalarType "oid",ScalarType "internal",ScalarType "smallint",ScalarType "internal"],ScalarType "double precision"),
    ("nlikesel",[ScalarType "internal",ScalarType "oid",ScalarType "internal",ScalarType "integer"],ScalarType "double precision"),
    ("notlike",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("notlike",[ScalarType "name",ScalarType "text"],ScalarType "boolean"),
    ("notlike",[ScalarType "bytea",ScalarType "bytea"],ScalarType "boolean"),
    ("now",[],ScalarType "timestamp with time zone"),
    ("npoints",[ScalarType "path"],ScalarType "integer"),
    ("npoints",[ScalarType "polygon"],ScalarType "integer"),
    ("numeric",[ScalarType "integer"],ScalarType "numeric"),
    ("numeric",[ScalarType "bigint"],ScalarType "numeric"),
    ("numeric",[ScalarType "numeric",ScalarType "integer"],ScalarType "numeric"),
    ("numeric",[ScalarType "double precision"],ScalarType "numeric"),
    ("numeric",[ScalarType "smallint"],ScalarType "numeric"),
    ("numeric",[ScalarType "real"],ScalarType "numeric"),
    ("numeric_abs",[ScalarType "numeric"],ScalarType "numeric"),
    ("numeric_accum",[ScalarType "numeric[]",ScalarType "numeric"],ScalarType "numeric[]"),
    ("numeric_add",[ScalarType "numeric",ScalarType "numeric"],ScalarType "numeric"),
    ("numeric_avg",[ScalarType "numeric[]"],ScalarType "numeric"),
    ("numeric_avg_accum",[ScalarType "numeric[]",ScalarType "numeric"],ScalarType "numeric[]"),
    ("numeric_cmp",[ScalarType "numeric",ScalarType "numeric"],ScalarType "integer"),
    ("numeric_div",[ScalarType "numeric",ScalarType "numeric"],ScalarType "numeric"),
    ("numeric_div_trunc",[ScalarType "numeric",ScalarType "numeric"],ScalarType "numeric"),
    ("numeric_eq",[ScalarType "numeric",ScalarType "numeric"],ScalarType "boolean"),
    ("numeric_exp",[ScalarType "numeric"],ScalarType "numeric"),
    ("numeric_fac",[ScalarType "bigint"],ScalarType "numeric"),
    ("numeric_ge",[ScalarType "numeric",ScalarType "numeric"],ScalarType "boolean"),
    ("numeric_gt",[ScalarType "numeric",ScalarType "numeric"],ScalarType "boolean"),
    ("numeric_in",[ScalarType "cstring",ScalarType "oid",ScalarType "integer"],ScalarType "numeric"),
    ("numeric_inc",[ScalarType "numeric"],ScalarType "numeric"),
    ("numeric_larger",[ScalarType "numeric",ScalarType "numeric"],ScalarType "numeric"),
    ("numeric_le",[ScalarType "numeric",ScalarType "numeric"],ScalarType "boolean"),
    ("numeric_ln",[ScalarType "numeric"],ScalarType "numeric"),
    ("numeric_log",[ScalarType "numeric",ScalarType "numeric"],ScalarType "numeric"),
    ("numeric_lt",[ScalarType "numeric",ScalarType "numeric"],ScalarType "boolean"),
    ("numeric_mod",[ScalarType "numeric",ScalarType "numeric"],ScalarType "numeric"),
    ("numeric_mul",[ScalarType "numeric",ScalarType "numeric"],ScalarType "numeric"),
    ("numeric_ne",[ScalarType "numeric",ScalarType "numeric"],ScalarType "boolean"),
    ("numeric_out",[ScalarType "numeric"],ScalarType "cstring"),
    ("numeric_power",[ScalarType "numeric",ScalarType "numeric"],ScalarType "numeric"),
    ("numeric_recv",[ScalarType "internal",ScalarType "oid",ScalarType "integer"],ScalarType "numeric"),
    ("numeric_send",[ScalarType "numeric"],ScalarType "bytea"),
    ("numeric_smaller",[ScalarType "numeric",ScalarType "numeric"],ScalarType "numeric"),
    ("numeric_sqrt",[ScalarType "numeric"],ScalarType "numeric"),
    ("numeric_stddev_pop",[ScalarType "numeric[]"],ScalarType "numeric"),
    ("numeric_stddev_samp",[ScalarType "numeric[]"],ScalarType "numeric"),
    ("numeric_sub",[ScalarType "numeric",ScalarType "numeric"],ScalarType "numeric"),
    ("numeric_uminus",[ScalarType "numeric"],ScalarType "numeric"),
    ("numeric_uplus",[ScalarType "numeric"],ScalarType "numeric"),
    ("numeric_var_pop",[ScalarType "numeric[]"],ScalarType "numeric"),
    ("numeric_var_samp",[ScalarType "numeric[]"],ScalarType "numeric"),
    ("numerictypmodin",[ScalarType "cstring[]"],ScalarType "integer"),
    ("numerictypmodout",[ScalarType "integer"],ScalarType "cstring"),
    ("numnode",[ScalarType "tsquery"],ScalarType "integer"),
    ("obj_description",[ScalarType "oid"],ScalarType "text"),
    ("obj_description",[ScalarType "oid",ScalarType "name"],ScalarType "text"),
    ("octet_length",[ScalarType "bytea"],ScalarType "integer"),
    ("octet_length",[ScalarType "text"],ScalarType "integer"),
    ("octet_length",[ScalarType "character"],ScalarType "integer"),
    ("octet_length",[ScalarType "bit"],ScalarType "integer"),
    ("oid",[ScalarType "bigint"],ScalarType "oid"),
    ("oideq",[ScalarType "oid",ScalarType "oid"],ScalarType "boolean"),
    ("oidge",[ScalarType "oid",ScalarType "oid"],ScalarType "boolean"),
    ("oidgt",[ScalarType "oid",ScalarType "oid"],ScalarType "boolean"),
    ("oidin",[ScalarType "cstring"],ScalarType "oid"),
    ("oidlarger",[ScalarType "oid",ScalarType "oid"],ScalarType "oid"),
    ("oidle",[ScalarType "oid",ScalarType "oid"],ScalarType "boolean"),
    ("oidlt",[ScalarType "oid",ScalarType "oid"],ScalarType "boolean"),
    ("oidne",[ScalarType "oid",ScalarType "oid"],ScalarType "boolean"),
    ("oidout",[ScalarType "oid"],ScalarType "cstring"),
    ("oidrecv",[ScalarType "internal"],ScalarType "oid"),
    ("oidsend",[ScalarType "oid"],ScalarType "bytea"),
    ("oidsmaller",[ScalarType "oid",ScalarType "oid"],ScalarType "oid"),
    ("oidvectoreq",[ScalarType "oidvector",ScalarType "oidvector"],ScalarType "boolean"),
    ("oidvectorge",[ScalarType "oidvector",ScalarType "oidvector"],ScalarType "boolean"),
    ("oidvectorgt",[ScalarType "oidvector",ScalarType "oidvector"],ScalarType "boolean"),
    ("oidvectorin",[ScalarType "cstring"],ScalarType "oidvector"),
    ("oidvectorle",[ScalarType "oidvector",ScalarType "oidvector"],ScalarType "boolean"),
    ("oidvectorlt",[ScalarType "oidvector",ScalarType "oidvector"],ScalarType "boolean"),
    ("oidvectorne",[ScalarType "oidvector",ScalarType "oidvector"],ScalarType "boolean"),
    ("oidvectorout",[ScalarType "oidvector"],ScalarType "cstring"),
    ("oidvectorrecv",[ScalarType "internal"],ScalarType "oidvector"),
    ("oidvectorsend",[ScalarType "oidvector"],ScalarType "bytea"),
    ("oidvectortypes",[ScalarType "oidvector"],ScalarType "text"),
    ("on_pb",[ScalarType "point",ScalarType "box"],ScalarType "boolean"),
    ("on_pl",[ScalarType "point",ScalarType "line"],ScalarType "boolean"),
    ("on_ppath",[ScalarType "point",ScalarType "path"],ScalarType "boolean"),
    ("on_ps",[ScalarType "point",ScalarType "lseg"],ScalarType "boolean"),
    ("on_sb",[ScalarType "lseg",ScalarType "box"],ScalarType "boolean"),
    ("on_sl",[ScalarType "lseg",ScalarType "line"],ScalarType "boolean"),
    ("opaque_in",[ScalarType "cstring"],ScalarType "opaque"),
    ("opaque_out",[ScalarType "opaque"],ScalarType "cstring"),
    ("overlaps",[ScalarType "time without time zone",ScalarType "interval",ScalarType "time without time zone",ScalarType "interval"],ScalarType "boolean"),
    ("overlaps",[ScalarType "timestamp without time zone",ScalarType "timestamp without time zone",ScalarType "timestamp without time zone",ScalarType "interval"],ScalarType "boolean"),
    ("overlaps",[ScalarType "timestamp without time zone",ScalarType "interval",ScalarType "timestamp without time zone",ScalarType "timestamp without time zone"],ScalarType "boolean"),
    ("overlaps",[ScalarType "time without time zone",ScalarType "interval",ScalarType "time without time zone",ScalarType "time without time zone"],ScalarType "boolean"),
    ("overlaps",[ScalarType "time with time zone",ScalarType "time with time zone",ScalarType "time with time zone",ScalarType "time with time zone"],ScalarType "boolean"),
    ("overlaps",[ScalarType "time without time zone",ScalarType "time without time zone",ScalarType "time without time zone",ScalarType "interval"],ScalarType "boolean"),
    ("overlaps",[ScalarType "timestamp with time zone",ScalarType "interval",ScalarType "timestamp with time zone",ScalarType "timestamp with time zone"],ScalarType "boolean"),
    ("overlaps",[ScalarType "timestamp without time zone",ScalarType "timestamp without time zone",ScalarType "timestamp without time zone",ScalarType "timestamp without time zone"],ScalarType "boolean"),
    ("overlaps",[ScalarType "timestamp with time zone",ScalarType "timestamp with time zone",ScalarType "timestamp with time zone",ScalarType "interval"],ScalarType "boolean"),
    ("overlaps",[ScalarType "timestamp with time zone",ScalarType "interval",ScalarType "timestamp with time zone",ScalarType "interval"],ScalarType "boolean"),
    ("overlaps",[ScalarType "timestamp with time zone",ScalarType "timestamp with time zone",ScalarType "timestamp with time zone",ScalarType "timestamp with time zone"],ScalarType "boolean"),
    ("overlaps",[ScalarType "timestamp without time zone",ScalarType "interval",ScalarType "timestamp without time zone",ScalarType "interval"],ScalarType "boolean"),
    ("overlaps",[ScalarType "time without time zone",ScalarType "time without time zone",ScalarType "time without time zone",ScalarType "time without time zone"],ScalarType "boolean"),
    ("overlay",[ScalarType "text",ScalarType "text",ScalarType "integer"],ScalarType "text"),
    ("overlay",[ScalarType "text",ScalarType "text",ScalarType "integer",ScalarType "integer"],ScalarType "text"),
    ("path",[ScalarType "polygon"],ScalarType "path"),
    ("path_add",[ScalarType "path",ScalarType "path"],ScalarType "path"),
    ("path_add_pt",[ScalarType "path",ScalarType "point"],ScalarType "path"),
    ("path_center",[ScalarType "path"],ScalarType "point"),
    ("path_contain_pt",[ScalarType "path",ScalarType "point"],ScalarType "boolean"),
    ("path_distance",[ScalarType "path",ScalarType "path"],ScalarType "double precision"),
    ("path_div_pt",[ScalarType "path",ScalarType "point"],ScalarType "path"),
    ("path_in",[ScalarType "cstring"],ScalarType "path"),
    ("path_inter",[ScalarType "path",ScalarType "path"],ScalarType "boolean"),
    ("path_length",[ScalarType "path"],ScalarType "double precision"),
    ("path_mul_pt",[ScalarType "path",ScalarType "point"],ScalarType "path"),
    ("path_n_eq",[ScalarType "path",ScalarType "path"],ScalarType "boolean"),
    ("path_n_ge",[ScalarType "path",ScalarType "path"],ScalarType "boolean"),
    ("path_n_gt",[ScalarType "path",ScalarType "path"],ScalarType "boolean"),
    ("path_n_le",[ScalarType "path",ScalarType "path"],ScalarType "boolean"),
    ("path_n_lt",[ScalarType "path",ScalarType "path"],ScalarType "boolean"),
    ("path_npoints",[ScalarType "path"],ScalarType "integer"),
    ("path_out",[ScalarType "path"],ScalarType "cstring"),
    ("path_recv",[ScalarType "internal"],ScalarType "path"),
    ("path_send",[ScalarType "path"],ScalarType "bytea"),
    ("path_sub_pt",[ScalarType "path",ScalarType "point"],ScalarType "path"),
    ("pclose",[ScalarType "path"],ScalarType "path"),
    ("pg_advisory_lock",[ScalarType "bigint"],ScalarType "void"),
    ("pg_advisory_lock",[ScalarType "integer",ScalarType "integer"],ScalarType "void"),
    ("pg_advisory_lock_shared",[ScalarType "integer",ScalarType "integer"],ScalarType "void"),
    ("pg_advisory_lock_shared",[ScalarType "bigint"],ScalarType "void"),
    ("pg_advisory_unlock",[ScalarType "integer",ScalarType "integer"],ScalarType "boolean"),
    ("pg_advisory_unlock",[ScalarType "bigint"],ScalarType "boolean"),
    ("pg_advisory_unlock_all",[],ScalarType "void"),
    ("pg_advisory_unlock_shared",[ScalarType "bigint"],ScalarType "boolean"),
    ("pg_advisory_unlock_shared",[ScalarType "integer",ScalarType "integer"],ScalarType "boolean"),
    ("pg_backend_pid",[],ScalarType "integer"),
    ("pg_cancel_backend",[ScalarType "integer"],ScalarType "boolean"),
    ("pg_char_to_encoding",[ScalarType "name"],ScalarType "integer"),
    ("pg_client_encoding",[],ScalarType "name"),
    ("pg_column_size",[ScalarType "\"any\""],ScalarType "integer"),
    ("pg_conf_load_time",[],ScalarType "timestamp with time zone"),
    ("pg_conversion_is_visible",[ScalarType "oid"],ScalarType "boolean"),
    ("pg_current_xlog_insert_location",[],ScalarType "text"),
    ("pg_current_xlog_location",[],ScalarType "text"),
    ("pg_cursor",[ScalarType "OUT name text",ScalarType "OUT statement text",ScalarType "OUT is_holdable boolean",ScalarType "OUT is_binary boolean",ScalarType "OUT is_scrollable boolean",ScalarType "OUT creation_time timestamp with time zone"],ScalarType "SETOF record"),
    ("pg_database_size",[ScalarType "name"],ScalarType "bigint"),
    ("pg_database_size",[ScalarType "oid"],ScalarType "bigint"),
    ("pg_encoding_to_char",[ScalarType "integer"],ScalarType "name"),
    ("pg_function_is_visible",[ScalarType "oid"],ScalarType "boolean"),
    ("pg_get_constraintdef",[ScalarType "oid",ScalarType "boolean"],ScalarType "text"),
    ("pg_get_constraintdef",[ScalarType "oid"],ScalarType "text"),
    ("pg_get_expr",[ScalarType "text",ScalarType "oid",ScalarType "boolean"],ScalarType "text"),
    ("pg_get_expr",[ScalarType "text",ScalarType "oid"],ScalarType "text"),
    ("pg_get_function_arguments",[ScalarType "oid"],ScalarType "text"),
    ("pg_get_function_identity_arguments",[ScalarType "oid"],ScalarType "text"),
    ("pg_get_function_result",[ScalarType "oid"],ScalarType "text"),
    ("pg_get_functiondef",[ScalarType "oid"],ScalarType "text"),
    ("pg_get_indexdef",[ScalarType "oid"],ScalarType "text"),
    ("pg_get_indexdef",[ScalarType "oid",ScalarType "integer",ScalarType "boolean"],ScalarType "text"),
    ("pg_get_keywords",[ScalarType "OUT word text",ScalarType "OUT catcode \"char\"",ScalarType "OUT catdesc text"],ScalarType "SETOF record"),
    ("pg_get_ruledef",[ScalarType "oid",ScalarType "boolean"],ScalarType "text"),
    ("pg_get_ruledef",[ScalarType "oid"],ScalarType "text"),
    ("pg_get_serial_sequence",[ScalarType "text",ScalarType "text"],ScalarType "text"),
    ("pg_get_triggerdef",[ScalarType "oid"],ScalarType "text"),
    ("pg_get_userbyid",[ScalarType "oid"],ScalarType "name"),
    ("pg_get_viewdef",[ScalarType "oid"],ScalarType "text"),
    ("pg_get_viewdef",[ScalarType "oid",ScalarType "boolean"],ScalarType "text"),
    ("pg_get_viewdef",[ScalarType "text",ScalarType "boolean"],ScalarType "text"),
    ("pg_get_viewdef",[ScalarType "text"],ScalarType "text"),
    ("pg_has_role",[ScalarType "oid",ScalarType "name",ScalarType "text"],ScalarType "boolean"),
    ("pg_has_role",[ScalarType "oid",ScalarType "text"],ScalarType "boolean"),
    ("pg_has_role",[ScalarType "oid",ScalarType "oid",ScalarType "text"],ScalarType "boolean"),
    ("pg_has_role",[ScalarType "name",ScalarType "oid",ScalarType "text"],ScalarType "boolean"),
    ("pg_has_role",[ScalarType "name",ScalarType "name",ScalarType "text"],ScalarType "boolean"),
    ("pg_has_role",[ScalarType "name",ScalarType "text"],ScalarType "boolean"),
    ("pg_is_other_temp_schema",[ScalarType "oid"],ScalarType "boolean"),
    ("pg_lock_status",[ScalarType "OUT locktype text",ScalarType "OUT database oid",ScalarType "OUT relation oid",ScalarType "OUT page integer",ScalarType "OUT tuple smallint",ScalarType "OUT virtualxid text",ScalarType "OUT transactionid xid",ScalarType "OUT classid oid",ScalarType "OUT objid oid",ScalarType "OUT objsubid smallint",ScalarType "OUT virtualtransaction text",ScalarType "OUT pid integer",ScalarType "OUT mode text",ScalarType "OUT granted boolean"],ScalarType "SETOF record"),
    ("pg_ls_dir",[ScalarType "text"],ScalarType "SETOF text"),
    ("pg_my_temp_schema",[],ScalarType "oid"),
    ("pg_opclass_is_visible",[ScalarType "oid"],ScalarType "boolean"),
    ("pg_operator_is_visible",[ScalarType "oid"],ScalarType "boolean"),
    ("pg_options_to_table",[ScalarType "options_array text[]",ScalarType "OUT option_name text",ScalarType "OUT option_value text"],ScalarType "SETOF record"),
    ("pg_postmaster_start_time",[],ScalarType "timestamp with time zone"),
    ("pg_prepared_statement",[ScalarType "OUT name text",ScalarType "OUT statement text",ScalarType "OUT prepare_time timestamp with time zone",ScalarType "OUT parameter_types regtype[]",ScalarType "OUT from_sql boolean"],ScalarType "SETOF record"),
    ("pg_prepared_xact",[ScalarType "OUT transaction xid",ScalarType "OUT gid text",ScalarType "OUT prepared timestamp with time zone",ScalarType "OUT ownerid oid",ScalarType "OUT dbid oid"],ScalarType "SETOF record"),
    ("pg_read_file",[ScalarType "text",ScalarType "bigint",ScalarType "bigint"],ScalarType "text"),
    ("pg_relation_size",[ScalarType "regclass"],ScalarType "bigint"),
    ("pg_relation_size",[ScalarType "regclass",ScalarType "text"],ScalarType "bigint"),
    ("pg_reload_conf",[],ScalarType "boolean"),
    ("pg_rotate_logfile",[],ScalarType "boolean"),
    ("pg_show_all_settings",[ScalarType "OUT name text",ScalarType "OUT setting text",ScalarType "OUT unit text",ScalarType "OUT category text",ScalarType "OUT short_desc text",ScalarType "OUT extra_desc text",ScalarType "OUT context text",ScalarType "OUT vartype text",ScalarType "OUT source text",ScalarType "OUT min_val text",ScalarType "OUT max_val text",ScalarType "OUT enumvals text[]",ScalarType "OUT boot_val text",ScalarType "OUT reset_val text",ScalarType "OUT sourcefile text",ScalarType "OUT sourceline integer"],ScalarType "SETOF record"),
    ("pg_size_pretty",[ScalarType "bigint"],ScalarType "text"),
    ("pg_sleep",[ScalarType "double precision"],ScalarType "void"),
    ("pg_start_backup",[ScalarType "label text",ScalarType "fast boolean DEFAULT false"],ScalarType "text"),
    ("pg_stat_clear_snapshot",[],ScalarType "void"),
    ("pg_stat_file",[ScalarType "filename text",ScalarType "OUT size bigint",ScalarType "OUT access timestamp with time zone",ScalarType "OUT modification timestamp with time zone",ScalarType "OUT change timestamp with time zone",ScalarType "OUT creation timestamp with time zone",ScalarType "OUT isdir boolean"],ScalarType "record"),
    ("pg_stat_get_activity",[ScalarType "pid integer",ScalarType "OUT datid oid",ScalarType "OUT procpid integer",ScalarType "OUT usesysid oid",ScalarType "OUT current_query text",ScalarType "OUT waiting boolean",ScalarType "OUT xact_start timestamp with time zone",ScalarType "OUT query_start timestamp with time zone",ScalarType "OUT backend_start timestamp with time zone",ScalarType "OUT client_addr inet",ScalarType "OUT client_port integer"],ScalarType "SETOF record"),
    ("pg_stat_get_backend_activity",[ScalarType "integer"],ScalarType "text"),
    ("pg_stat_get_backend_activity_start",[ScalarType "integer"],ScalarType "timestamp with time zone"),
    ("pg_stat_get_backend_client_addr",[ScalarType "integer"],ScalarType "inet"),
    ("pg_stat_get_backend_client_port",[ScalarType "integer"],ScalarType "integer"),
    ("pg_stat_get_backend_dbid",[ScalarType "integer"],ScalarType "oid"),
    ("pg_stat_get_backend_idset",[],ScalarType "SETOF integer"),
    ("pg_stat_get_backend_pid",[ScalarType "integer"],ScalarType "integer"),
    ("pg_stat_get_backend_start",[ScalarType "integer"],ScalarType "timestamp with time zone"),
    ("pg_stat_get_backend_userid",[ScalarType "integer"],ScalarType "oid"),
    ("pg_stat_get_backend_waiting",[ScalarType "integer"],ScalarType "boolean"),
    ("pg_stat_get_backend_xact_start",[ScalarType "integer"],ScalarType "timestamp with time zone"),
    ("pg_stat_get_bgwriter_buf_written_checkpoints",[],ScalarType "bigint"),
    ("pg_stat_get_bgwriter_buf_written_clean",[],ScalarType "bigint"),
    ("pg_stat_get_bgwriter_maxwritten_clean",[],ScalarType "bigint"),
    ("pg_stat_get_bgwriter_requested_checkpoints",[],ScalarType "bigint"),
    ("pg_stat_get_bgwriter_timed_checkpoints",[],ScalarType "bigint"),
    ("pg_stat_get_blocks_fetched",[ScalarType "oid"],ScalarType "bigint"),
    ("pg_stat_get_blocks_hit",[ScalarType "oid"],ScalarType "bigint"),
    ("pg_stat_get_buf_alloc",[],ScalarType "bigint"),
    ("pg_stat_get_buf_written_backend",[],ScalarType "bigint"),
    ("pg_stat_get_db_blocks_fetched",[ScalarType "oid"],ScalarType "bigint"),
    ("pg_stat_get_db_blocks_hit",[ScalarType "oid"],ScalarType "bigint"),
    ("pg_stat_get_db_numbackends",[ScalarType "oid"],ScalarType "integer"),
    ("pg_stat_get_db_tuples_deleted",[ScalarType "oid"],ScalarType "bigint"),
    ("pg_stat_get_db_tuples_fetched",[ScalarType "oid"],ScalarType "bigint"),
    ("pg_stat_get_db_tuples_inserted",[ScalarType "oid"],ScalarType "bigint"),
    ("pg_stat_get_db_tuples_returned",[ScalarType "oid"],ScalarType "bigint"),
    ("pg_stat_get_db_tuples_updated",[ScalarType "oid"],ScalarType "bigint"),
    ("pg_stat_get_db_xact_commit",[ScalarType "oid"],ScalarType "bigint"),
    ("pg_stat_get_db_xact_rollback",[ScalarType "oid"],ScalarType "bigint"),
    ("pg_stat_get_dead_tuples",[ScalarType "oid"],ScalarType "bigint"),
    ("pg_stat_get_function_calls",[ScalarType "oid"],ScalarType "bigint"),
    ("pg_stat_get_function_self_time",[ScalarType "oid"],ScalarType "bigint"),
    ("pg_stat_get_function_time",[ScalarType "oid"],ScalarType "bigint"),
    ("pg_stat_get_last_analyze_time",[ScalarType "oid"],ScalarType "timestamp with time zone"),
    ("pg_stat_get_last_autoanalyze_time",[ScalarType "oid"],ScalarType "timestamp with time zone"),
    ("pg_stat_get_last_autovacuum_time",[ScalarType "oid"],ScalarType "timestamp with time zone"),
    ("pg_stat_get_last_vacuum_time",[ScalarType "oid"],ScalarType "timestamp with time zone"),
    ("pg_stat_get_live_tuples",[ScalarType "oid"],ScalarType "bigint"),
    ("pg_stat_get_numscans",[ScalarType "oid"],ScalarType "bigint"),
    ("pg_stat_get_tuples_deleted",[ScalarType "oid"],ScalarType "bigint"),
    ("pg_stat_get_tuples_fetched",[ScalarType "oid"],ScalarType "bigint"),
    ("pg_stat_get_tuples_hot_updated",[ScalarType "oid"],ScalarType "bigint"),
    ("pg_stat_get_tuples_inserted",[ScalarType "oid"],ScalarType "bigint"),
    ("pg_stat_get_tuples_returned",[ScalarType "oid"],ScalarType "bigint"),
    ("pg_stat_get_tuples_updated",[ScalarType "oid"],ScalarType "bigint"),
    ("pg_stat_reset",[],ScalarType "void"),
    ("pg_stop_backup",[],ScalarType "text"),
    ("pg_switch_xlog",[],ScalarType "text"),
    ("pg_table_is_visible",[ScalarType "oid"],ScalarType "boolean"),
    ("pg_tablespace_databases",[ScalarType "oid"],ScalarType "SETOF oid"),
    ("pg_tablespace_size",[ScalarType "name"],ScalarType "bigint"),
    ("pg_tablespace_size",[ScalarType "oid"],ScalarType "bigint"),
    ("pg_terminate_backend",[ScalarType "integer"],ScalarType "boolean"),
    ("pg_timezone_abbrevs",[ScalarType "OUT abbrev text",ScalarType "OUT utc_offset interval",ScalarType "OUT is_dst boolean"],ScalarType "SETOF record"),
    ("pg_timezone_names",[ScalarType "OUT name text",ScalarType "OUT abbrev text",ScalarType "OUT utc_offset interval",ScalarType "OUT is_dst boolean"],ScalarType "SETOF record"),
    ("pg_total_relation_size",[ScalarType "regclass"],ScalarType "bigint"),
    ("pg_try_advisory_lock",[ScalarType "bigint"],ScalarType "boolean"),
    ("pg_try_advisory_lock",[ScalarType "integer",ScalarType "integer"],ScalarType "boolean"),
    ("pg_try_advisory_lock_shared",[ScalarType "integer",ScalarType "integer"],ScalarType "boolean"),
    ("pg_try_advisory_lock_shared",[ScalarType "bigint"],ScalarType "boolean"),
    ("pg_ts_config_is_visible",[ScalarType "oid"],ScalarType "boolean"),
    ("pg_ts_dict_is_visible",[ScalarType "oid"],ScalarType "boolean"),
    ("pg_ts_parser_is_visible",[ScalarType "oid"],ScalarType "boolean"),
    ("pg_ts_template_is_visible",[ScalarType "oid"],ScalarType "boolean"),
    ("pg_type_is_visible",[ScalarType "oid"],ScalarType "boolean"),
    ("pg_typeof",[ScalarType "\"any\""],ScalarType "regtype"),
    ("pg_xlogfile_name",[ScalarType "text"],ScalarType "text"),
    ("pg_xlogfile_name_offset",[ScalarType "wal_location text",ScalarType "OUT file_name text",ScalarType "OUT file_offset integer"],ScalarType "record"),
    ("pi",[],ScalarType "double precision"),
    ("plainto_tsquery",[ScalarType "regconfig",ScalarType "text"],ScalarType "tsquery"),
    ("plainto_tsquery",[ScalarType "text"],ScalarType "tsquery"),
    ("point",[ScalarType "circle"],ScalarType "point"),
    ("point",[ScalarType "double precision",ScalarType "double precision"],ScalarType "point"),
    ("point",[ScalarType "lseg"],ScalarType "point"),
    ("point",[ScalarType "path"],ScalarType "point"),
    ("point",[ScalarType "box"],ScalarType "point"),
    ("point",[ScalarType "polygon"],ScalarType "point"),
    ("point_above",[ScalarType "point",ScalarType "point"],ScalarType "boolean"),
    ("point_add",[ScalarType "point",ScalarType "point"],ScalarType "point"),
    ("point_below",[ScalarType "point",ScalarType "point"],ScalarType "boolean"),
    ("point_distance",[ScalarType "point",ScalarType "point"],ScalarType "double precision"),
    ("point_div",[ScalarType "point",ScalarType "point"],ScalarType "point"),
    ("point_eq",[ScalarType "point",ScalarType "point"],ScalarType "boolean"),
    ("point_horiz",[ScalarType "point",ScalarType "point"],ScalarType "boolean"),
    ("point_in",[ScalarType "cstring"],ScalarType "point"),
    ("point_left",[ScalarType "point",ScalarType "point"],ScalarType "boolean"),
    ("point_mul",[ScalarType "point",ScalarType "point"],ScalarType "point"),
    ("point_ne",[ScalarType "point",ScalarType "point"],ScalarType "boolean"),
    ("point_out",[ScalarType "point"],ScalarType "cstring"),
    ("point_recv",[ScalarType "internal"],ScalarType "point"),
    ("point_right",[ScalarType "point",ScalarType "point"],ScalarType "boolean"),
    ("point_send",[ScalarType "point"],ScalarType "bytea"),
    ("point_sub",[ScalarType "point",ScalarType "point"],ScalarType "point"),
    ("point_vert",[ScalarType "point",ScalarType "point"],ScalarType "boolean"),
    ("poly_above",[ScalarType "polygon",ScalarType "polygon"],ScalarType "boolean"),
    ("poly_below",[ScalarType "polygon",ScalarType "polygon"],ScalarType "boolean"),
    ("poly_center",[ScalarType "polygon"],ScalarType "point"),
    ("poly_contain",[ScalarType "polygon",ScalarType "polygon"],ScalarType "boolean"),
    ("poly_contain_pt",[ScalarType "polygon",ScalarType "point"],ScalarType "boolean"),
    ("poly_contained",[ScalarType "polygon",ScalarType "polygon"],ScalarType "boolean"),
    ("poly_distance",[ScalarType "polygon",ScalarType "polygon"],ScalarType "double precision"),
    ("poly_in",[ScalarType "cstring"],ScalarType "polygon"),
    ("poly_left",[ScalarType "polygon",ScalarType "polygon"],ScalarType "boolean"),
    ("poly_npoints",[ScalarType "polygon"],ScalarType "integer"),
    ("poly_out",[ScalarType "polygon"],ScalarType "cstring"),
    ("poly_overabove",[ScalarType "polygon",ScalarType "polygon"],ScalarType "boolean"),
    ("poly_overbelow",[ScalarType "polygon",ScalarType "polygon"],ScalarType "boolean"),
    ("poly_overlap",[ScalarType "polygon",ScalarType "polygon"],ScalarType "boolean"),
    ("poly_overleft",[ScalarType "polygon",ScalarType "polygon"],ScalarType "boolean"),
    ("poly_overright",[ScalarType "polygon",ScalarType "polygon"],ScalarType "boolean"),
    ("poly_recv",[ScalarType "internal"],ScalarType "polygon"),
    ("poly_right",[ScalarType "polygon",ScalarType "polygon"],ScalarType "boolean"),
    ("poly_same",[ScalarType "polygon",ScalarType "polygon"],ScalarType "boolean"),
    ("poly_send",[ScalarType "polygon"],ScalarType "bytea"),
    ("polygon",[ScalarType "integer",ScalarType "circle"],ScalarType "polygon"),
    ("polygon",[ScalarType "circle"],ScalarType "polygon"),
    ("polygon",[ScalarType "box"],ScalarType "polygon"),
    ("polygon",[ScalarType "path"],ScalarType "polygon"),
    ("popen",[ScalarType "path"],ScalarType "path"),
    ("position",[ScalarType "bit",ScalarType "bit"],ScalarType "integer"),
    ("position",[ScalarType "text",ScalarType "text"],ScalarType "integer"),
    ("position",[ScalarType "bytea",ScalarType "bytea"],ScalarType "integer"),
    ("positionjoinsel",[ScalarType "internal",ScalarType "oid",ScalarType "internal",ScalarType "smallint",ScalarType "internal"],ScalarType "double precision"),
    ("positionsel",[ScalarType "internal",ScalarType "oid",ScalarType "internal",ScalarType "integer"],ScalarType "double precision"),
    ("postgresql_fdw_validator",[ScalarType "text[]",ScalarType "oid"],ScalarType "boolean"),
    ("pow",[ScalarType "numeric",ScalarType "numeric"],ScalarType "numeric"),
    ("pow",[ScalarType "double precision",ScalarType "double precision"],ScalarType "double precision"),
    ("power",[ScalarType "double precision",ScalarType "double precision"],ScalarType "double precision"),
    ("power",[ScalarType "numeric",ScalarType "numeric"],ScalarType "numeric"),
    ("prsd_end",[ScalarType "internal"],ScalarType "void"),
    ("prsd_headline",[ScalarType "internal",ScalarType "internal",ScalarType "tsquery"],ScalarType "internal"),
    ("prsd_lextype",[ScalarType "internal"],ScalarType "internal"),
    ("prsd_nexttoken",[ScalarType "internal",ScalarType "internal",ScalarType "internal"],ScalarType "internal"),
    ("prsd_start",[ScalarType "internal",ScalarType "integer"],ScalarType "internal"),
    ("pt_contained_circle",[ScalarType "point",ScalarType "circle"],ScalarType "boolean"),
    ("pt_contained_poly",[ScalarType "point",ScalarType "polygon"],ScalarType "boolean"),
    ("query_to_xml",[ScalarType "query text",ScalarType "nulls boolean",ScalarType "tableforest boolean",ScalarType "targetns text"],ScalarType "xml"),
    ("query_to_xml_and_xmlschema",[ScalarType "query text",ScalarType "nulls boolean",ScalarType "tableforest boolean",ScalarType "targetns text"],ScalarType "xml"),
    ("query_to_xmlschema",[ScalarType "query text",ScalarType "nulls boolean",ScalarType "tableforest boolean",ScalarType "targetns text"],ScalarType "xml"),
    ("querytree",[ScalarType "tsquery"],ScalarType "text"),
    ("quote_ident",[ScalarType "text"],ScalarType "text"),
    ("quote_literal",[ScalarType "anyelement"],ScalarType "text"),
    ("quote_literal",[ScalarType "text"],ScalarType "text"),
    ("quote_nullable",[ScalarType "anyelement"],ScalarType "text"),
    ("quote_nullable",[ScalarType "text"],ScalarType "text"),
    ("radians",[ScalarType "double precision"],ScalarType "double precision"),
    ("radius",[ScalarType "circle"],ScalarType "double precision"),
    ("random",[],ScalarType "double precision"),
    ("record_eq",[ScalarType "record",ScalarType "record"],ScalarType "boolean"),
    ("record_ge",[ScalarType "record",ScalarType "record"],ScalarType "boolean"),
    ("record_gt",[ScalarType "record",ScalarType "record"],ScalarType "boolean"),
    ("record_in",[ScalarType "cstring",ScalarType "oid",ScalarType "integer"],ScalarType "record"),
    ("record_le",[ScalarType "record",ScalarType "record"],ScalarType "boolean"),
    ("record_lt",[ScalarType "record",ScalarType "record"],ScalarType "boolean"),
    ("record_ne",[ScalarType "record",ScalarType "record"],ScalarType "boolean"),
    ("record_out",[ScalarType "record"],ScalarType "cstring"),
    ("record_recv",[ScalarType "internal",ScalarType "oid",ScalarType "integer"],ScalarType "record"),
    ("record_send",[ScalarType "record"],ScalarType "bytea"),
    ("regclass",[ScalarType "text"],ScalarType "regclass"),
    ("regclassin",[ScalarType "cstring"],ScalarType "regclass"),
    ("regclassout",[ScalarType "regclass"],ScalarType "cstring"),
    ("regclassrecv",[ScalarType "internal"],ScalarType "regclass"),
    ("regclasssend",[ScalarType "regclass"],ScalarType "bytea"),
    ("regconfigin",[ScalarType "cstring"],ScalarType "regconfig"),
    ("regconfigout",[ScalarType "regconfig"],ScalarType "cstring"),
    ("regconfigrecv",[ScalarType "internal"],ScalarType "regconfig"),
    ("regconfigsend",[ScalarType "regconfig"],ScalarType "bytea"),
    ("regdictionaryin",[ScalarType "cstring"],ScalarType "regdictionary"),
    ("regdictionaryout",[ScalarType "regdictionary"],ScalarType "cstring"),
    ("regdictionaryrecv",[ScalarType "internal"],ScalarType "regdictionary"),
    ("regdictionarysend",[ScalarType "regdictionary"],ScalarType "bytea"),
    ("regexeqjoinsel",[ScalarType "internal",ScalarType "oid",ScalarType "internal",ScalarType "smallint",ScalarType "internal"],ScalarType "double precision"),
    ("regexeqsel",[ScalarType "internal",ScalarType "oid",ScalarType "internal",ScalarType "integer"],ScalarType "double precision"),
    ("regexnejoinsel",[ScalarType "internal",ScalarType "oid",ScalarType "internal",ScalarType "smallint",ScalarType "internal"],ScalarType "double precision"),
    ("regexnesel",[ScalarType "internal",ScalarType "oid",ScalarType "internal",ScalarType "integer"],ScalarType "double precision"),
    ("regexp_matches",[ScalarType "text",ScalarType "text",ScalarType "text"],ScalarType "SETOF text[]"),
    ("regexp_matches",[ScalarType "text",ScalarType "text"],ScalarType "SETOF text[]"),
    ("regexp_replace",[ScalarType "text",ScalarType "text",ScalarType "text"],ScalarType "text"),
    ("regexp_replace",[ScalarType "text",ScalarType "text",ScalarType "text",ScalarType "text"],ScalarType "text"),
    ("regexp_split_to_array",[ScalarType "text",ScalarType "text"],ScalarType "text[]"),
    ("regexp_split_to_array",[ScalarType "text",ScalarType "text",ScalarType "text"],ScalarType "text[]"),
    ("regexp_split_to_table",[ScalarType "text",ScalarType "text"],ScalarType "SETOF text"),
    ("regexp_split_to_table",[ScalarType "text",ScalarType "text",ScalarType "text"],ScalarType "SETOF text"),
    ("regoperatorin",[ScalarType "cstring"],ScalarType "regoperator"),
    ("regoperatorout",[ScalarType "regoperator"],ScalarType "cstring"),
    ("regoperatorrecv",[ScalarType "internal"],ScalarType "regoperator"),
    ("regoperatorsend",[ScalarType "regoperator"],ScalarType "bytea"),
    ("regoperin",[ScalarType "cstring"],ScalarType "regoper"),
    ("regoperout",[ScalarType "regoper"],ScalarType "cstring"),
    ("regoperrecv",[ScalarType "internal"],ScalarType "regoper"),
    ("regopersend",[ScalarType "regoper"],ScalarType "bytea"),
    ("regprocedurein",[ScalarType "cstring"],ScalarType "regprocedure"),
    ("regprocedureout",[ScalarType "regprocedure"],ScalarType "cstring"),
    ("regprocedurerecv",[ScalarType "internal"],ScalarType "regprocedure"),
    ("regproceduresend",[ScalarType "regprocedure"],ScalarType "bytea"),
    ("regprocin",[ScalarType "cstring"],ScalarType "regproc"),
    ("regprocout",[ScalarType "regproc"],ScalarType "cstring"),
    ("regprocrecv",[ScalarType "internal"],ScalarType "regproc"),
    ("regprocsend",[ScalarType "regproc"],ScalarType "bytea"),
    ("regtypein",[ScalarType "cstring"],ScalarType "regtype"),
    ("regtypeout",[ScalarType "regtype"],ScalarType "cstring"),
    ("regtyperecv",[ScalarType "internal"],ScalarType "regtype"),
    ("regtypesend",[ScalarType "regtype"],ScalarType "bytea"),
    ("reltime",[ScalarType "interval"],ScalarType "reltime"),
    ("reltimeeq",[ScalarType "reltime",ScalarType "reltime"],ScalarType "boolean"),
    ("reltimege",[ScalarType "reltime",ScalarType "reltime"],ScalarType "boolean"),
    ("reltimegt",[ScalarType "reltime",ScalarType "reltime"],ScalarType "boolean"),
    ("reltimein",[ScalarType "cstring"],ScalarType "reltime"),
    ("reltimele",[ScalarType "reltime",ScalarType "reltime"],ScalarType "boolean"),
    ("reltimelt",[ScalarType "reltime",ScalarType "reltime"],ScalarType "boolean"),
    ("reltimene",[ScalarType "reltime",ScalarType "reltime"],ScalarType "boolean"),
    ("reltimeout",[ScalarType "reltime"],ScalarType "cstring"),
    ("reltimerecv",[ScalarType "internal"],ScalarType "reltime"),
    ("reltimesend",[ScalarType "reltime"],ScalarType "bytea"),
    ("repeat",[ScalarType "text",ScalarType "integer"],ScalarType "text"),
    ("replace",[ScalarType "text",ScalarType "text",ScalarType "text"],ScalarType "text"),
    ("round",[ScalarType "numeric",ScalarType "integer"],ScalarType "numeric"),
    ("round",[ScalarType "numeric"],ScalarType "numeric"),
    ("round",[ScalarType "double precision"],ScalarType "double precision"),
    ("rpad",[ScalarType "text",ScalarType "integer",ScalarType "text"],ScalarType "text"),
    ("rpad",[ScalarType "text",ScalarType "integer"],ScalarType "text"),
    ("rtrim",[ScalarType "text",ScalarType "text"],ScalarType "text"),
    ("rtrim",[ScalarType "text"],ScalarType "text"),
    ("scalargtjoinsel",[ScalarType "internal",ScalarType "oid",ScalarType "internal",ScalarType "smallint",ScalarType "internal"],ScalarType "double precision"),
    ("scalargtsel",[ScalarType "internal",ScalarType "oid",ScalarType "internal",ScalarType "integer"],ScalarType "double precision"),
    ("scalarltjoinsel",[ScalarType "internal",ScalarType "oid",ScalarType "internal",ScalarType "smallint",ScalarType "internal"],ScalarType "double precision"),
    ("scalarltsel",[ScalarType "internal",ScalarType "oid",ScalarType "internal",ScalarType "integer"],ScalarType "double precision"),
    ("schema_to_xml",[ScalarType "schema name",ScalarType "nulls boolean",ScalarType "tableforest boolean",ScalarType "targetns text"],ScalarType "xml"),
    ("schema_to_xml_and_xmlschema",[ScalarType "schema name",ScalarType "nulls boolean",ScalarType "tableforest boolean",ScalarType "targetns text"],ScalarType "xml"),
    ("schema_to_xmlschema",[ScalarType "schema name",ScalarType "nulls boolean",ScalarType "tableforest boolean",ScalarType "targetns text"],ScalarType "xml"),
    ("session_user",[],ScalarType "name"),
    ("set_bit",[ScalarType "bytea",ScalarType "integer",ScalarType "integer"],ScalarType "bytea"),
    ("set_byte",[ScalarType "bytea",ScalarType "integer",ScalarType "integer"],ScalarType "bytea"),
    ("set_config",[ScalarType "text",ScalarType "text",ScalarType "boolean"],ScalarType "text"),
    ("set_masklen",[ScalarType "inet",ScalarType "integer"],ScalarType "inet"),
    ("set_masklen",[ScalarType "cidr",ScalarType "integer"],ScalarType "cidr"),
    ("setseed",[ScalarType "double precision"],ScalarType "void"),
    ("setval",[ScalarType "regclass",ScalarType "bigint",ScalarType "boolean"],ScalarType "bigint"),
    ("setval",[ScalarType "regclass",ScalarType "bigint"],ScalarType "bigint"),
    ("setweight",[ScalarType "tsvector",ScalarType "\"char\""],ScalarType "tsvector"),
    ("shell_in",[ScalarType "cstring"],ScalarType "opaque"),
    ("shell_out",[ScalarType "opaque"],ScalarType "cstring"),
    ("shift_jis_2004_to_euc_jis_2004",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("shift_jis_2004_to_utf8",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("shobj_description",[ScalarType "oid",ScalarType "name"],ScalarType "text"),
    ("sign",[ScalarType "double precision"],ScalarType "double precision"),
    ("sign",[ScalarType "numeric"],ScalarType "numeric"),
    ("similar_escape",[ScalarType "text",ScalarType "text"],ScalarType "text"),
    ("sin",[ScalarType "double precision"],ScalarType "double precision"),
    ("sjis_to_euc_jp",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("sjis_to_mic",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("sjis_to_utf8",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("slope",[ScalarType "point",ScalarType "point"],ScalarType "double precision"),
    ("smgreq",[ScalarType "smgr",ScalarType "smgr"],ScalarType "boolean"),
    ("smgrin",[ScalarType "cstring"],ScalarType "smgr"),
    ("smgrne",[ScalarType "smgr",ScalarType "smgr"],ScalarType "boolean"),
    ("smgrout",[ScalarType "smgr"],ScalarType "cstring"),
    ("split_part",[ScalarType "text",ScalarType "text",ScalarType "integer"],ScalarType "text"),
    ("sqrt",[ScalarType "double precision"],ScalarType "double precision"),
    ("sqrt",[ScalarType "numeric"],ScalarType "numeric"),
    ("statement_timestamp",[],ScalarType "timestamp with time zone"),
    ("string_to_array",[ScalarType "text",ScalarType "text"],ScalarType "text[]"),
    ("strip",[ScalarType "tsvector"],ScalarType "tsvector"),
    ("strpos",[ScalarType "text",ScalarType "text"],ScalarType "integer"),
    ("substr",[ScalarType "text",ScalarType "integer",ScalarType "integer"],ScalarType "text"),
    ("substr",[ScalarType "bytea",ScalarType "integer",ScalarType "integer"],ScalarType "bytea"),
    ("substr",[ScalarType "bytea",ScalarType "integer"],ScalarType "bytea"),
    ("substr",[ScalarType "text",ScalarType "integer"],ScalarType "text"),
    ("substring",[ScalarType "bytea",ScalarType "integer"],ScalarType "bytea"),
    ("substring",[ScalarType "bytea",ScalarType "integer",ScalarType "integer"],ScalarType "bytea"),
    ("substring",[ScalarType "text",ScalarType "integer"],ScalarType "text"),
    ("substring",[ScalarType "text",ScalarType "text",ScalarType "text"],ScalarType "text"),
    ("substring",[ScalarType "text",ScalarType "text"],ScalarType "text"),
    ("substring",[ScalarType "text",ScalarType "integer",ScalarType "integer"],ScalarType "text"),
    ("substring",[ScalarType "bit",ScalarType "integer",ScalarType "integer"],ScalarType "bit"),
    ("substring",[ScalarType "bit",ScalarType "integer"],ScalarType "bit"),
    ("table_to_xml",[ScalarType "tbl regclass",ScalarType "nulls boolean",ScalarType "tableforest boolean",ScalarType "targetns text"],ScalarType "xml"),
    ("table_to_xml_and_xmlschema",[ScalarType "tbl regclass",ScalarType "nulls boolean",ScalarType "tableforest boolean",ScalarType "targetns text"],ScalarType "xml"),
    ("table_to_xmlschema",[ScalarType "tbl regclass",ScalarType "nulls boolean",ScalarType "tableforest boolean",ScalarType "targetns text"],ScalarType "xml"),
    ("tan",[ScalarType "double precision"],ScalarType "double precision"),
    ("text",[ScalarType "\"char\""],ScalarType "text"),
    ("text",[ScalarType "xml"],ScalarType "text"),
    ("text",[ScalarType "inet"],ScalarType "text"),
    ("text",[ScalarType "name"],ScalarType "text"),
    ("text",[ScalarType "character"],ScalarType "text"),
    ("text",[ScalarType "boolean"],ScalarType "text"),
    ("text_ge",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("text_gt",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("text_larger",[ScalarType "text",ScalarType "text"],ScalarType "text"),
    ("text_le",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("text_lt",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("text_pattern_ge",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("text_pattern_gt",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("text_pattern_le",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("text_pattern_lt",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("text_smaller",[ScalarType "text",ScalarType "text"],ScalarType "text"),
    ("textanycat",[ScalarType "text",ScalarType "anynonarray"],ScalarType "text"),
    ("textcat",[ScalarType "text",ScalarType "text"],ScalarType "text"),
    ("texteq",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("texticlike",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("texticnlike",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("texticregexeq",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("texticregexne",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("textin",[ScalarType "cstring"],ScalarType "text"),
    ("textlen",[ScalarType "text"],ScalarType "integer"),
    ("textlike",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("textne",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("textnlike",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("textout",[ScalarType "text"],ScalarType "cstring"),
    ("textrecv",[ScalarType "internal"],ScalarType "text"),
    ("textregexeq",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("textregexne",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("textsend",[ScalarType "text"],ScalarType "bytea"),
    ("thesaurus_init",[ScalarType "internal"],ScalarType "internal"),
    ("thesaurus_lexize",[ScalarType "internal",ScalarType "internal",ScalarType "internal",ScalarType "internal"],ScalarType "internal"),
    ("tideq",[ScalarType "tid",ScalarType "tid"],ScalarType "boolean"),
    ("tidge",[ScalarType "tid",ScalarType "tid"],ScalarType "boolean"),
    ("tidgt",[ScalarType "tid",ScalarType "tid"],ScalarType "boolean"),
    ("tidin",[ScalarType "cstring"],ScalarType "tid"),
    ("tidlarger",[ScalarType "tid",ScalarType "tid"],ScalarType "tid"),
    ("tidle",[ScalarType "tid",ScalarType "tid"],ScalarType "boolean"),
    ("tidlt",[ScalarType "tid",ScalarType "tid"],ScalarType "boolean"),
    ("tidne",[ScalarType "tid",ScalarType "tid"],ScalarType "boolean"),
    ("tidout",[ScalarType "tid"],ScalarType "cstring"),
    ("tidrecv",[ScalarType "internal"],ScalarType "tid"),
    ("tidsend",[ScalarType "tid"],ScalarType "bytea"),
    ("tidsmaller",[ScalarType "tid",ScalarType "tid"],ScalarType "tid"),
    ("time",[ScalarType "time without time zone",ScalarType "integer"],ScalarType "time without time zone"),
    ("time",[ScalarType "time with time zone"],ScalarType "time without time zone"),
    ("time",[ScalarType "abstime"],ScalarType "time without time zone"),
    ("time",[ScalarType "interval"],ScalarType "time without time zone"),
    ("time",[ScalarType "timestamp without time zone"],ScalarType "time without time zone"),
    ("time",[ScalarType "timestamp with time zone"],ScalarType "time without time zone"),
    ("time_cmp",[ScalarType "time without time zone",ScalarType "time without time zone"],ScalarType "integer"),
    ("time_eq",[ScalarType "time without time zone",ScalarType "time without time zone"],ScalarType "boolean"),
    ("time_ge",[ScalarType "time without time zone",ScalarType "time without time zone"],ScalarType "boolean"),
    ("time_gt",[ScalarType "time without time zone",ScalarType "time without time zone"],ScalarType "boolean"),
    ("time_hash",[ScalarType "time without time zone"],ScalarType "integer"),
    ("time_in",[ScalarType "cstring",ScalarType "oid",ScalarType "integer"],ScalarType "time without time zone"),
    ("time_larger",[ScalarType "time without time zone",ScalarType "time without time zone"],ScalarType "time without time zone"),
    ("time_le",[ScalarType "time without time zone",ScalarType "time without time zone"],ScalarType "boolean"),
    ("time_lt",[ScalarType "time without time zone",ScalarType "time without time zone"],ScalarType "boolean"),
    ("time_mi_interval",[ScalarType "time without time zone",ScalarType "interval"],ScalarType "time without time zone"),
    ("time_mi_time",[ScalarType "time without time zone",ScalarType "time without time zone"],ScalarType "interval"),
    ("time_ne",[ScalarType "time without time zone",ScalarType "time without time zone"],ScalarType "boolean"),
    ("time_out",[ScalarType "time without time zone"],ScalarType "cstring"),
    ("time_pl_interval",[ScalarType "time without time zone",ScalarType "interval"],ScalarType "time without time zone"),
    ("time_recv",[ScalarType "internal",ScalarType "oid",ScalarType "integer"],ScalarType "time without time zone"),
    ("time_send",[ScalarType "time without time zone"],ScalarType "bytea"),
    ("time_smaller",[ScalarType "time without time zone",ScalarType "time without time zone"],ScalarType "time without time zone"),
    ("timedate_pl",[ScalarType "time without time zone",ScalarType "date"],ScalarType "timestamp without time zone"),
    ("timemi",[ScalarType "abstime",ScalarType "reltime"],ScalarType "abstime"),
    ("timenow",[],ScalarType "abstime"),
    ("timeofday",[],ScalarType "text"),
    ("timepl",[ScalarType "abstime",ScalarType "reltime"],ScalarType "abstime"),
    ("timestamp",[ScalarType "date",ScalarType "time without time zone"],ScalarType "timestamp without time zone"),
    ("timestamp",[ScalarType "timestamp with time zone"],ScalarType "timestamp without time zone"),
    ("timestamp",[ScalarType "date"],ScalarType "timestamp without time zone"),
    ("timestamp",[ScalarType "abstime"],ScalarType "timestamp without time zone"),
    ("timestamp",[ScalarType "timestamp without time zone",ScalarType "integer"],ScalarType "timestamp without time zone"),
    ("timestamp_cmp",[ScalarType "timestamp without time zone",ScalarType "timestamp without time zone"],ScalarType "integer"),
    ("timestamp_cmp_date",[ScalarType "timestamp without time zone",ScalarType "date"],ScalarType "integer"),
    ("timestamp_cmp_timestamptz",[ScalarType "timestamp without time zone",ScalarType "timestamp with time zone"],ScalarType "integer"),
    ("timestamp_eq",[ScalarType "timestamp without time zone",ScalarType "timestamp without time zone"],ScalarType "boolean"),
    ("timestamp_eq_date",[ScalarType "timestamp without time zone",ScalarType "date"],ScalarType "boolean"),
    ("timestamp_eq_timestamptz",[ScalarType "timestamp without time zone",ScalarType "timestamp with time zone"],ScalarType "boolean"),
    ("timestamp_ge",[ScalarType "timestamp without time zone",ScalarType "timestamp without time zone"],ScalarType "boolean"),
    ("timestamp_ge_date",[ScalarType "timestamp without time zone",ScalarType "date"],ScalarType "boolean"),
    ("timestamp_ge_timestamptz",[ScalarType "timestamp without time zone",ScalarType "timestamp with time zone"],ScalarType "boolean"),
    ("timestamp_gt",[ScalarType "timestamp without time zone",ScalarType "timestamp without time zone"],ScalarType "boolean"),
    ("timestamp_gt_date",[ScalarType "timestamp without time zone",ScalarType "date"],ScalarType "boolean"),
    ("timestamp_gt_timestamptz",[ScalarType "timestamp without time zone",ScalarType "timestamp with time zone"],ScalarType "boolean"),
    ("timestamp_hash",[ScalarType "timestamp without time zone"],ScalarType "integer"),
    ("timestamp_in",[ScalarType "cstring",ScalarType "oid",ScalarType "integer"],ScalarType "timestamp without time zone"),
    ("timestamp_larger",[ScalarType "timestamp without time zone",ScalarType "timestamp without time zone"],ScalarType "timestamp without time zone"),
    ("timestamp_le",[ScalarType "timestamp without time zone",ScalarType "timestamp without time zone"],ScalarType "boolean"),
    ("timestamp_le_date",[ScalarType "timestamp without time zone",ScalarType "date"],ScalarType "boolean"),
    ("timestamp_le_timestamptz",[ScalarType "timestamp without time zone",ScalarType "timestamp with time zone"],ScalarType "boolean"),
    ("timestamp_lt",[ScalarType "timestamp without time zone",ScalarType "timestamp without time zone"],ScalarType "boolean"),
    ("timestamp_lt_date",[ScalarType "timestamp without time zone",ScalarType "date"],ScalarType "boolean"),
    ("timestamp_lt_timestamptz",[ScalarType "timestamp without time zone",ScalarType "timestamp with time zone"],ScalarType "boolean"),
    ("timestamp_mi",[ScalarType "timestamp without time zone",ScalarType "timestamp without time zone"],ScalarType "interval"),
    ("timestamp_mi_interval",[ScalarType "timestamp without time zone",ScalarType "interval"],ScalarType "timestamp without time zone"),
    ("timestamp_ne",[ScalarType "timestamp without time zone",ScalarType "timestamp without time zone"],ScalarType "boolean"),
    ("timestamp_ne_date",[ScalarType "timestamp without time zone",ScalarType "date"],ScalarType "boolean"),
    ("timestamp_ne_timestamptz",[ScalarType "timestamp without time zone",ScalarType "timestamp with time zone"],ScalarType "boolean"),
    ("timestamp_out",[ScalarType "timestamp without time zone"],ScalarType "cstring"),
    ("timestamp_pl_interval",[ScalarType "timestamp without time zone",ScalarType "interval"],ScalarType "timestamp without time zone"),
    ("timestamp_recv",[ScalarType "internal",ScalarType "oid",ScalarType "integer"],ScalarType "timestamp without time zone"),
    ("timestamp_send",[ScalarType "timestamp without time zone"],ScalarType "bytea"),
    ("timestamp_smaller",[ScalarType "timestamp without time zone",ScalarType "timestamp without time zone"],ScalarType "timestamp without time zone"),
    ("timestamptypmodin",[ScalarType "cstring[]"],ScalarType "integer"),
    ("timestamptypmodout",[ScalarType "integer"],ScalarType "cstring"),
    ("timestamptz",[ScalarType "abstime"],ScalarType "timestamp with time zone"),
    ("timestamptz",[ScalarType "timestamp without time zone"],ScalarType "timestamp with time zone"),
    ("timestamptz",[ScalarType "date",ScalarType "time with time zone"],ScalarType "timestamp with time zone"),
    ("timestamptz",[ScalarType "date",ScalarType "time without time zone"],ScalarType "timestamp with time zone"),
    ("timestamptz",[ScalarType "date"],ScalarType "timestamp with time zone"),
    ("timestamptz",[ScalarType "timestamp with time zone",ScalarType "integer"],ScalarType "timestamp with time zone"),
    ("timestamptz_cmp",[ScalarType "timestamp with time zone",ScalarType "timestamp with time zone"],ScalarType "integer"),
    ("timestamptz_cmp_date",[ScalarType "timestamp with time zone",ScalarType "date"],ScalarType "integer"),
    ("timestamptz_cmp_timestamp",[ScalarType "timestamp with time zone",ScalarType "timestamp without time zone"],ScalarType "integer"),
    ("timestamptz_eq",[ScalarType "timestamp with time zone",ScalarType "timestamp with time zone"],ScalarType "boolean"),
    ("timestamptz_eq_date",[ScalarType "timestamp with time zone",ScalarType "date"],ScalarType "boolean"),
    ("timestamptz_eq_timestamp",[ScalarType "timestamp with time zone",ScalarType "timestamp without time zone"],ScalarType "boolean"),
    ("timestamptz_ge",[ScalarType "timestamp with time zone",ScalarType "timestamp with time zone"],ScalarType "boolean"),
    ("timestamptz_ge_date",[ScalarType "timestamp with time zone",ScalarType "date"],ScalarType "boolean"),
    ("timestamptz_ge_timestamp",[ScalarType "timestamp with time zone",ScalarType "timestamp without time zone"],ScalarType "boolean"),
    ("timestamptz_gt",[ScalarType "timestamp with time zone",ScalarType "timestamp with time zone"],ScalarType "boolean"),
    ("timestamptz_gt_date",[ScalarType "timestamp with time zone",ScalarType "date"],ScalarType "boolean"),
    ("timestamptz_gt_timestamp",[ScalarType "timestamp with time zone",ScalarType "timestamp without time zone"],ScalarType "boolean"),
    ("timestamptz_in",[ScalarType "cstring",ScalarType "oid",ScalarType "integer"],ScalarType "timestamp with time zone"),
    ("timestamptz_larger",[ScalarType "timestamp with time zone",ScalarType "timestamp with time zone"],ScalarType "timestamp with time zone"),
    ("timestamptz_le",[ScalarType "timestamp with time zone",ScalarType "timestamp with time zone"],ScalarType "boolean"),
    ("timestamptz_le_date",[ScalarType "timestamp with time zone",ScalarType "date"],ScalarType "boolean"),
    ("timestamptz_le_timestamp",[ScalarType "timestamp with time zone",ScalarType "timestamp without time zone"],ScalarType "boolean"),
    ("timestamptz_lt",[ScalarType "timestamp with time zone",ScalarType "timestamp with time zone"],ScalarType "boolean"),
    ("timestamptz_lt_date",[ScalarType "timestamp with time zone",ScalarType "date"],ScalarType "boolean"),
    ("timestamptz_lt_timestamp",[ScalarType "timestamp with time zone",ScalarType "timestamp without time zone"],ScalarType "boolean"),
    ("timestamptz_mi",[ScalarType "timestamp with time zone",ScalarType "timestamp with time zone"],ScalarType "interval"),
    ("timestamptz_mi_interval",[ScalarType "timestamp with time zone",ScalarType "interval"],ScalarType "timestamp with time zone"),
    ("timestamptz_ne",[ScalarType "timestamp with time zone",ScalarType "timestamp with time zone"],ScalarType "boolean"),
    ("timestamptz_ne_date",[ScalarType "timestamp with time zone",ScalarType "date"],ScalarType "boolean"),
    ("timestamptz_ne_timestamp",[ScalarType "timestamp with time zone",ScalarType "timestamp without time zone"],ScalarType "boolean"),
    ("timestamptz_out",[ScalarType "timestamp with time zone"],ScalarType "cstring"),
    ("timestamptz_pl_interval",[ScalarType "timestamp with time zone",ScalarType "interval"],ScalarType "timestamp with time zone"),
    ("timestamptz_recv",[ScalarType "internal",ScalarType "oid",ScalarType "integer"],ScalarType "timestamp with time zone"),
    ("timestamptz_send",[ScalarType "timestamp with time zone"],ScalarType "bytea"),
    ("timestamptz_smaller",[ScalarType "timestamp with time zone",ScalarType "timestamp with time zone"],ScalarType "timestamp with time zone"),
    ("timestamptztypmodin",[ScalarType "cstring[]"],ScalarType "integer"),
    ("timestamptztypmodout",[ScalarType "integer"],ScalarType "cstring"),
    ("timetypmodin",[ScalarType "cstring[]"],ScalarType "integer"),
    ("timetypmodout",[ScalarType "integer"],ScalarType "cstring"),
    ("timetz",[ScalarType "time with time zone",ScalarType "integer"],ScalarType "time with time zone"),
    ("timetz",[ScalarType "timestamp with time zone"],ScalarType "time with time zone"),
    ("timetz",[ScalarType "time without time zone"],ScalarType "time with time zone"),
    ("timetz_cmp",[ScalarType "time with time zone",ScalarType "time with time zone"],ScalarType "integer"),
    ("timetz_eq",[ScalarType "time with time zone",ScalarType "time with time zone"],ScalarType "boolean"),
    ("timetz_ge",[ScalarType "time with time zone",ScalarType "time with time zone"],ScalarType "boolean"),
    ("timetz_gt",[ScalarType "time with time zone",ScalarType "time with time zone"],ScalarType "boolean"),
    ("timetz_hash",[ScalarType "time with time zone"],ScalarType "integer"),
    ("timetz_in",[ScalarType "cstring",ScalarType "oid",ScalarType "integer"],ScalarType "time with time zone"),
    ("timetz_larger",[ScalarType "time with time zone",ScalarType "time with time zone"],ScalarType "time with time zone"),
    ("timetz_le",[ScalarType "time with time zone",ScalarType "time with time zone"],ScalarType "boolean"),
    ("timetz_lt",[ScalarType "time with time zone",ScalarType "time with time zone"],ScalarType "boolean"),
    ("timetz_mi_interval",[ScalarType "time with time zone",ScalarType "interval"],ScalarType "time with time zone"),
    ("timetz_ne",[ScalarType "time with time zone",ScalarType "time with time zone"],ScalarType "boolean"),
    ("timetz_out",[ScalarType "time with time zone"],ScalarType "cstring"),
    ("timetz_pl_interval",[ScalarType "time with time zone",ScalarType "interval"],ScalarType "time with time zone"),
    ("timetz_recv",[ScalarType "internal",ScalarType "oid",ScalarType "integer"],ScalarType "time with time zone"),
    ("timetz_send",[ScalarType "time with time zone"],ScalarType "bytea"),
    ("timetz_smaller",[ScalarType "time with time zone",ScalarType "time with time zone"],ScalarType "time with time zone"),
    ("timetzdate_pl",[ScalarType "time with time zone",ScalarType "date"],ScalarType "timestamp with time zone"),
    ("timetztypmodin",[ScalarType "cstring[]"],ScalarType "integer"),
    ("timetztypmodout",[ScalarType "integer"],ScalarType "cstring"),
    ("timezone",[ScalarType "interval",ScalarType "timestamp without time zone"],ScalarType "timestamp with time zone"),
    ("timezone",[ScalarType "text",ScalarType "timestamp without time zone"],ScalarType "timestamp with time zone"),
    ("timezone",[ScalarType "interval",ScalarType "timestamp with time zone"],ScalarType "timestamp without time zone"),
    ("timezone",[ScalarType "text",ScalarType "timestamp with time zone"],ScalarType "timestamp without time zone"),
    ("timezone",[ScalarType "interval",ScalarType "time with time zone"],ScalarType "time with time zone"),
    ("timezone",[ScalarType "text",ScalarType "time with time zone"],ScalarType "time with time zone"),
    ("tinterval",[ScalarType "abstime",ScalarType "abstime"],ScalarType "tinterval"),
    ("tintervalct",[ScalarType "tinterval",ScalarType "tinterval"],ScalarType "boolean"),
    ("tintervalend",[ScalarType "tinterval"],ScalarType "abstime"),
    ("tintervaleq",[ScalarType "tinterval",ScalarType "tinterval"],ScalarType "boolean"),
    ("tintervalge",[ScalarType "tinterval",ScalarType "tinterval"],ScalarType "boolean"),
    ("tintervalgt",[ScalarType "tinterval",ScalarType "tinterval"],ScalarType "boolean"),
    ("tintervalin",[ScalarType "cstring"],ScalarType "tinterval"),
    ("tintervalle",[ScalarType "tinterval",ScalarType "tinterval"],ScalarType "boolean"),
    ("tintervalleneq",[ScalarType "tinterval",ScalarType "reltime"],ScalarType "boolean"),
    ("tintervallenge",[ScalarType "tinterval",ScalarType "reltime"],ScalarType "boolean"),
    ("tintervallengt",[ScalarType "tinterval",ScalarType "reltime"],ScalarType "boolean"),
    ("tintervallenle",[ScalarType "tinterval",ScalarType "reltime"],ScalarType "boolean"),
    ("tintervallenlt",[ScalarType "tinterval",ScalarType "reltime"],ScalarType "boolean"),
    ("tintervallenne",[ScalarType "tinterval",ScalarType "reltime"],ScalarType "boolean"),
    ("tintervallt",[ScalarType "tinterval",ScalarType "tinterval"],ScalarType "boolean"),
    ("tintervalne",[ScalarType "tinterval",ScalarType "tinterval"],ScalarType "boolean"),
    ("tintervalout",[ScalarType "tinterval"],ScalarType "cstring"),
    ("tintervalov",[ScalarType "tinterval",ScalarType "tinterval"],ScalarType "boolean"),
    ("tintervalrecv",[ScalarType "internal"],ScalarType "tinterval"),
    ("tintervalrel",[ScalarType "tinterval"],ScalarType "reltime"),
    ("tintervalsame",[ScalarType "tinterval",ScalarType "tinterval"],ScalarType "boolean"),
    ("tintervalsend",[ScalarType "tinterval"],ScalarType "bytea"),
    ("tintervalstart",[ScalarType "tinterval"],ScalarType "abstime"),
    ("to_ascii",[ScalarType "text",ScalarType "integer"],ScalarType "text"),
    ("to_ascii",[ScalarType "text",ScalarType "name"],ScalarType "text"),
    ("to_ascii",[ScalarType "text"],ScalarType "text"),
    ("to_char",[ScalarType "timestamp with time zone",ScalarType "text"],ScalarType "text"),
    ("to_char",[ScalarType "timestamp without time zone",ScalarType "text"],ScalarType "text"),
    ("to_char",[ScalarType "numeric",ScalarType "text"],ScalarType "text"),
    ("to_char",[ScalarType "integer",ScalarType "text"],ScalarType "text"),
    ("to_char",[ScalarType "bigint",ScalarType "text"],ScalarType "text"),
    ("to_char",[ScalarType "real",ScalarType "text"],ScalarType "text"),
    ("to_char",[ScalarType "double precision",ScalarType "text"],ScalarType "text"),
    ("to_char",[ScalarType "interval",ScalarType "text"],ScalarType "text"),
    ("to_date",[ScalarType "text",ScalarType "text"],ScalarType "date"),
    ("to_hex",[ScalarType "bigint"],ScalarType "text"),
    ("to_hex",[ScalarType "integer"],ScalarType "text"),
    ("to_number",[ScalarType "text",ScalarType "text"],ScalarType "numeric"),
    ("to_timestamp",[ScalarType "text",ScalarType "text"],ScalarType "timestamp with time zone"),
    ("to_timestamp",[ScalarType "double precision"],ScalarType "timestamp with time zone"),
    ("to_tsquery",[ScalarType "text"],ScalarType "tsquery"),
    ("to_tsquery",[ScalarType "regconfig",ScalarType "text"],ScalarType "tsquery"),
    ("to_tsvector",[ScalarType "regconfig",ScalarType "text"],ScalarType "tsvector"),
    ("to_tsvector",[ScalarType "text"],ScalarType "tsvector"),
    ("transaction_timestamp",[],ScalarType "timestamp with time zone"),
    ("translate",[ScalarType "text",ScalarType "text",ScalarType "text"],ScalarType "text"),
    ("trigger_out",[ScalarType "trigger"],ScalarType "cstring"),
    ("trunc",[ScalarType "numeric"],ScalarType "numeric"),
    ("trunc",[ScalarType "numeric",ScalarType "integer"],ScalarType "numeric"),
    ("trunc",[ScalarType "macaddr"],ScalarType "macaddr"),
    ("trunc",[ScalarType "double precision"],ScalarType "double precision"),
    ("ts_debug",[ScalarType "config regconfig",ScalarType "document text",ScalarType "OUT alias text",ScalarType "OUT description text",ScalarType "OUT token text",ScalarType "OUT dictionaries regdictionary[]",ScalarType "OUT dictionary regdictionary",ScalarType "OUT lexemes text[]"],ScalarType "SETOF record"),
    ("ts_debug",[ScalarType "document text",ScalarType "OUT alias text",ScalarType "OUT description text",ScalarType "OUT token text",ScalarType "OUT dictionaries regdictionary[]",ScalarType "OUT dictionary regdictionary",ScalarType "OUT lexemes text[]"],ScalarType "SETOF record"),
    ("ts_headline",[ScalarType "regconfig",ScalarType "text",ScalarType "tsquery"],ScalarType "text"),
    ("ts_headline",[ScalarType "regconfig",ScalarType "text",ScalarType "tsquery",ScalarType "text"],ScalarType "text"),
    ("ts_headline",[ScalarType "text",ScalarType "tsquery"],ScalarType "text"),
    ("ts_headline",[ScalarType "text",ScalarType "tsquery",ScalarType "text"],ScalarType "text"),
    ("ts_lexize",[ScalarType "regdictionary",ScalarType "text"],ScalarType "text[]"),
    ("ts_match_qv",[ScalarType "tsquery",ScalarType "tsvector"],ScalarType "boolean"),
    ("ts_match_tq",[ScalarType "text",ScalarType "tsquery"],ScalarType "boolean"),
    ("ts_match_tt",[ScalarType "text",ScalarType "text"],ScalarType "boolean"),
    ("ts_match_vq",[ScalarType "tsvector",ScalarType "tsquery"],ScalarType "boolean"),
    ("ts_parse",[ScalarType "parser_name text",ScalarType "txt text",ScalarType "OUT tokid integer",ScalarType "OUT token text"],ScalarType "SETOF record"),
    ("ts_parse",[ScalarType "parser_oid oid",ScalarType "txt text",ScalarType "OUT tokid integer",ScalarType "OUT token text"],ScalarType "SETOF record"),
    ("ts_rank",[ScalarType "tsvector",ScalarType "tsquery"],ScalarType "real"),
    ("ts_rank",[ScalarType "real[]",ScalarType "tsvector",ScalarType "tsquery",ScalarType "integer"],ScalarType "real"),
    ("ts_rank",[ScalarType "real[]",ScalarType "tsvector",ScalarType "tsquery"],ScalarType "real"),
    ("ts_rank",[ScalarType "tsvector",ScalarType "tsquery",ScalarType "integer"],ScalarType "real"),
    ("ts_rank_cd",[ScalarType "real[]",ScalarType "tsvector",ScalarType "tsquery"],ScalarType "real"),
    ("ts_rank_cd",[ScalarType "real[]",ScalarType "tsvector",ScalarType "tsquery",ScalarType "integer"],ScalarType "real"),
    ("ts_rank_cd",[ScalarType "tsvector",ScalarType "tsquery"],ScalarType "real"),
    ("ts_rank_cd",[ScalarType "tsvector",ScalarType "tsquery",ScalarType "integer"],ScalarType "real"),
    ("ts_rewrite",[ScalarType "tsquery",ScalarType "text"],ScalarType "tsquery"),
    ("ts_rewrite",[ScalarType "tsquery",ScalarType "tsquery",ScalarType "tsquery"],ScalarType "tsquery"),
    ("ts_stat",[ScalarType "query text",ScalarType "weights text",ScalarType "OUT word text",ScalarType "OUT ndoc integer",ScalarType "OUT nentry integer"],ScalarType "SETOF record"),
    ("ts_stat",[ScalarType "query text",ScalarType "OUT word text",ScalarType "OUT ndoc integer",ScalarType "OUT nentry integer"],ScalarType "SETOF record"),
    ("ts_token_type",[ScalarType "parser_name text",ScalarType "OUT tokid integer",ScalarType "OUT alias text",ScalarType "OUT description text"],ScalarType "SETOF record"),
    ("ts_token_type",[ScalarType "parser_oid oid",ScalarType "OUT tokid integer",ScalarType "OUT alias text",ScalarType "OUT description text"],ScalarType "SETOF record"),
    ("ts_typanalyze",[ScalarType "internal"],ScalarType "boolean"),
    ("tsmatchjoinsel",[ScalarType "internal",ScalarType "oid",ScalarType "internal",ScalarType "smallint",ScalarType "internal"],ScalarType "double precision"),
    ("tsmatchsel",[ScalarType "internal",ScalarType "oid",ScalarType "internal",ScalarType "integer"],ScalarType "double precision"),
    ("tsq_mcontained",[ScalarType "tsquery",ScalarType "tsquery"],ScalarType "boolean"),
    ("tsq_mcontains",[ScalarType "tsquery",ScalarType "tsquery"],ScalarType "boolean"),
    ("tsquery_and",[ScalarType "tsquery",ScalarType "tsquery"],ScalarType "tsquery"),
    ("tsquery_cmp",[ScalarType "tsquery",ScalarType "tsquery"],ScalarType "integer"),
    ("tsquery_eq",[ScalarType "tsquery",ScalarType "tsquery"],ScalarType "boolean"),
    ("tsquery_ge",[ScalarType "tsquery",ScalarType "tsquery"],ScalarType "boolean"),
    ("tsquery_gt",[ScalarType "tsquery",ScalarType "tsquery"],ScalarType "boolean"),
    ("tsquery_le",[ScalarType "tsquery",ScalarType "tsquery"],ScalarType "boolean"),
    ("tsquery_lt",[ScalarType "tsquery",ScalarType "tsquery"],ScalarType "boolean"),
    ("tsquery_ne",[ScalarType "tsquery",ScalarType "tsquery"],ScalarType "boolean"),
    ("tsquery_not",[ScalarType "tsquery"],ScalarType "tsquery"),
    ("tsquery_or",[ScalarType "tsquery",ScalarType "tsquery"],ScalarType "tsquery"),
    ("tsqueryin",[ScalarType "cstring"],ScalarType "tsquery"),
    ("tsqueryout",[ScalarType "tsquery"],ScalarType "cstring"),
    ("tsqueryrecv",[ScalarType "internal"],ScalarType "tsquery"),
    ("tsquerysend",[ScalarType "tsquery"],ScalarType "bytea"),
    ("tsvector_cmp",[ScalarType "tsvector",ScalarType "tsvector"],ScalarType "integer"),
    ("tsvector_concat",[ScalarType "tsvector",ScalarType "tsvector"],ScalarType "tsvector"),
    ("tsvector_eq",[ScalarType "tsvector",ScalarType "tsvector"],ScalarType "boolean"),
    ("tsvector_ge",[ScalarType "tsvector",ScalarType "tsvector"],ScalarType "boolean"),
    ("tsvector_gt",[ScalarType "tsvector",ScalarType "tsvector"],ScalarType "boolean"),
    ("tsvector_le",[ScalarType "tsvector",ScalarType "tsvector"],ScalarType "boolean"),
    ("tsvector_lt",[ScalarType "tsvector",ScalarType "tsvector"],ScalarType "boolean"),
    ("tsvector_ne",[ScalarType "tsvector",ScalarType "tsvector"],ScalarType "boolean"),
    ("tsvectorin",[ScalarType "cstring"],ScalarType "tsvector"),
    ("tsvectorout",[ScalarType "tsvector"],ScalarType "cstring"),
    ("tsvectorrecv",[ScalarType "internal"],ScalarType "tsvector"),
    ("tsvectorsend",[ScalarType "tsvector"],ScalarType "bytea"),
    ("txid_current",[],ScalarType "bigint"),
    ("txid_current_snapshot",[],ScalarType "txid_snapshot"),
    ("txid_snapshot_in",[ScalarType "cstring"],ScalarType "txid_snapshot"),
    ("txid_snapshot_out",[ScalarType "txid_snapshot"],ScalarType "cstring"),
    ("txid_snapshot_recv",[ScalarType "internal"],ScalarType "txid_snapshot"),
    ("txid_snapshot_send",[ScalarType "txid_snapshot"],ScalarType "bytea"),
    ("txid_snapshot_xip",[ScalarType "txid_snapshot"],ScalarType "SETOF bigint"),
    ("txid_snapshot_xmax",[ScalarType "txid_snapshot"],ScalarType "bigint"),
    ("txid_snapshot_xmin",[ScalarType "txid_snapshot"],ScalarType "bigint"),
    ("txid_visible_in_snapshot",[ScalarType "bigint",ScalarType "txid_snapshot"],ScalarType "boolean"),
    ("uhc_to_utf8",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("unknownin",[ScalarType "cstring"],ScalarType "unknown"),
    ("unknownout",[ScalarType "unknown"],ScalarType "cstring"),
    ("unknownrecv",[ScalarType "internal"],ScalarType "unknown"),
    ("unknownsend",[ScalarType "unknown"],ScalarType "bytea"),
    ("unnest",[ScalarType "anyarray"],ScalarType "SETOF anyelement"),
    ("upper",[ScalarType "text"],ScalarType "text"),
    ("utf8_to_ascii",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("utf8_to_big5",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("utf8_to_euc_cn",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("utf8_to_euc_jis_2004",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("utf8_to_euc_jp",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("utf8_to_euc_kr",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("utf8_to_euc_tw",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("utf8_to_gb18030",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("utf8_to_gbk",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("utf8_to_iso8859",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("utf8_to_iso8859_1",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("utf8_to_johab",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("utf8_to_koi8r",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("utf8_to_koi8u",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("utf8_to_shift_jis_2004",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("utf8_to_sjis",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("utf8_to_uhc",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("utf8_to_win",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("uuid_cmp",[ScalarType "uuid",ScalarType "uuid"],ScalarType "integer"),
    ("uuid_eq",[ScalarType "uuid",ScalarType "uuid"],ScalarType "boolean"),
    ("uuid_ge",[ScalarType "uuid",ScalarType "uuid"],ScalarType "boolean"),
    ("uuid_gt",[ScalarType "uuid",ScalarType "uuid"],ScalarType "boolean"),
    ("uuid_hash",[ScalarType "uuid"],ScalarType "integer"),
    ("uuid_in",[ScalarType "cstring"],ScalarType "uuid"),
    ("uuid_le",[ScalarType "uuid",ScalarType "uuid"],ScalarType "boolean"),
    ("uuid_lt",[ScalarType "uuid",ScalarType "uuid"],ScalarType "boolean"),
    ("uuid_ne",[ScalarType "uuid",ScalarType "uuid"],ScalarType "boolean"),
    ("uuid_out",[ScalarType "uuid"],ScalarType "cstring"),
    ("uuid_recv",[ScalarType "internal"],ScalarType "uuid"),
    ("uuid_send",[ScalarType "uuid"],ScalarType "bytea"),
    ("varbit",[ScalarType "bit varying",ScalarType "integer",ScalarType "boolean"],ScalarType "bit varying"),
    ("varbit_in",[ScalarType "cstring",ScalarType "oid",ScalarType "integer"],ScalarType "bit varying"),
    ("varbit_out",[ScalarType "bit varying"],ScalarType "cstring"),
    ("varbit_recv",[ScalarType "internal",ScalarType "oid",ScalarType "integer"],ScalarType "bit varying"),
    ("varbit_send",[ScalarType "bit varying"],ScalarType "bytea"),
    ("varbitcmp",[ScalarType "bit varying",ScalarType "bit varying"],ScalarType "integer"),
    ("varbiteq",[ScalarType "bit varying",ScalarType "bit varying"],ScalarType "boolean"),
    ("varbitge",[ScalarType "bit varying",ScalarType "bit varying"],ScalarType "boolean"),
    ("varbitgt",[ScalarType "bit varying",ScalarType "bit varying"],ScalarType "boolean"),
    ("varbitle",[ScalarType "bit varying",ScalarType "bit varying"],ScalarType "boolean"),
    ("varbitlt",[ScalarType "bit varying",ScalarType "bit varying"],ScalarType "boolean"),
    ("varbitne",[ScalarType "bit varying",ScalarType "bit varying"],ScalarType "boolean"),
    ("varbittypmodin",[ScalarType "cstring[]"],ScalarType "integer"),
    ("varbittypmodout",[ScalarType "integer"],ScalarType "cstring"),
    ("varchar",[ScalarType "character varying",ScalarType "integer",ScalarType "boolean"],ScalarType "character varying"),
    ("varchar",[ScalarType "name"],ScalarType "character varying"),
    ("varcharin",[ScalarType "cstring",ScalarType "oid",ScalarType "integer"],ScalarType "character varying"),
    ("varcharout",[ScalarType "character varying"],ScalarType "cstring"),
    ("varcharrecv",[ScalarType "internal",ScalarType "oid",ScalarType "integer"],ScalarType "character varying"),
    ("varcharsend",[ScalarType "character varying"],ScalarType "bytea"),
    ("varchartypmodin",[ScalarType "cstring[]"],ScalarType "integer"),
    ("varchartypmodout",[ScalarType "integer"],ScalarType "cstring"),
    ("version",[],ScalarType "text"),
    ("void_in",[ScalarType "cstring"],ScalarType "void"),
    ("void_out",[ScalarType "void"],ScalarType "cstring"),
    ("width",[ScalarType "box"],ScalarType "double precision"),
    ("width_bucket",[ScalarType "double precision",ScalarType "double precision",ScalarType "double precision",ScalarType "integer"],ScalarType "integer"),
    ("width_bucket",[ScalarType "numeric",ScalarType "numeric",ScalarType "numeric",ScalarType "integer"],ScalarType "integer"),
    ("win1250_to_latin2",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("win1250_to_mic",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("win1251_to_iso",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("win1251_to_koi8r",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("win1251_to_mic",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("win1251_to_win866",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("win866_to_iso",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("win866_to_koi8r",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("win866_to_mic",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("win866_to_win1251",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("win_to_utf8",[ScalarType "integer",ScalarType "integer",ScalarType "cstring",ScalarType "internal",ScalarType "integer"],ScalarType "void"),
    ("xideq",[ScalarType "xid",ScalarType "xid"],ScalarType "boolean"),
    ("xideqint4",[ScalarType "xid",ScalarType "integer"],ScalarType "boolean"),
    ("xidin",[ScalarType "cstring"],ScalarType "xid"),
    ("xidout",[ScalarType "xid"],ScalarType "cstring"),
    ("xidrecv",[ScalarType "internal"],ScalarType "xid"),
    ("xidsend",[ScalarType "xid"],ScalarType "bytea"),
    ("xml",[ScalarType "text"],ScalarType "xml"),
    ("xml_in",[ScalarType "cstring"],ScalarType "xml"),
    ("xml_out",[ScalarType "xml"],ScalarType "cstring"),
    ("xml_recv",[ScalarType "internal"],ScalarType "xml"),
    ("xml_send",[ScalarType "xml"],ScalarType "bytea"),
    ("xmlcomment",[ScalarType "text"],ScalarType "xml"),
    ("xmlconcat2",[ScalarType "xml",ScalarType "xml"],ScalarType "xml"),
    ("xmlvalidate",[ScalarType "xml",ScalarType "text"],ScalarType "boolean"),
    ("xpath",[ScalarType "text",ScalarType "xml",ScalarType "text[]"],ScalarType "xml[]"),
    ("xpath",[ScalarType "text",ScalarType "xml"],ScalarType "xml[]")
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
                    (AllSameType $ ScalarType "boolean")
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
                  ScalarType "boolean"
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
                        AnyElement -> _casesInodeType
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
                  ScalarType "float"
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
                                              ,ScalarType "integer"
                                              ,ScalarType "integer"])
                                   (ConstRetType (ScalarType "text"))
                    Between -> ct
                                   (AllSameTypeNumAny 3)
                                   (ConstRetType (ScalarType "boolean"))
                    ArraySub -> ct
                                   (ExactPredList
                                     [ArgCheck isArrayType NotArrayType
                                     ,exactType (ScalarType "integer")])
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
                  ScalarType "integer"
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
                  AnyElement
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
data Type  = AnyArray 
           | AnyElement 
           | ArrayType (Type) 
           | ScalarType (String) 
           | SetOfType (Type) 
           | TypeError (MySourcePos) (TypeErrorInfo) 
           | TypeList ([Type]) 
           | UnknownType 
           deriving ( Eq,Show)
-- cata
sem_Type :: Type  ->
            T_Type 
sem_Type (AnyArray )  =
    (sem_Type_AnyArray )
sem_Type (AnyElement )  =
    (sem_Type_AnyElement )
sem_Type (ArrayType _type )  =
    (sem_Type_ArrayType (sem_Type _type ) )
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
sem_Type_AnyArray :: T_Type 
sem_Type_AnyArray  =
    (let 
     in  ( ))
sem_Type_AnyElement :: T_Type 
sem_Type_AnyElement  =
    (let 
     in  ( ))
sem_Type_ArrayType :: T_Type  ->
                      T_Type 
sem_Type_ArrayType type_  =
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
                  if tn_ `elem` defaultTypes
                    then ScalarType tn_
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