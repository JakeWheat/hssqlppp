

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


getOperatorType :: String -> OperatorType
getOperatorType s = case () of
                      _ | s `elem` ["not", "@", "u-"] -> LeftUnary
                        | s `elem` ["is null", "is not null"] -> RightUnary
                        | s `elem` ["+", "-", "*", "/","^",
                                    "%","=","and","or","||",
                                    "like","<>","<",">",
                                    "<=",">=","<->"] -> BinaryOp
                        | otherwise -> error $ "don't know flavour of operator " ++ s



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

{-checkPredList :: [ArgCheck] -> [Type] -> [Type]
checkPredList sp chks ts = if length chks /= length ts
                             then [TypeError sp
                                             (WrongNumArgs
                                              (length chks)
                                              (length ts))]
                             else (flip map) (take (length chks) [1..])
                                  (\n -> 
                           where
                             checkArg n acc (chk:chks) (t:ts) =
                                 if chk t
                                   then checkArg (n+1) acc chks ts
                                   else checkArg (n+1) (acc ++ 
                                   
                             checkArg n [] [] acc = acc
-}



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
            ExactPredList chks | length l /= length chks ->
                                   Just $ te $ WrongNumArgs
                                            (length chks)
                                            (length l)
                               | any (\(chk,tc) -> not $ argCheck chk tc)
                                     (zip chks l) ->
                                         Just $ te $ OtherTypeError "types not correct."
                               | otherwise -> Nothing
            AllSameTypePredNum p n | length l /= n ->
                                        Just $ te $ WrongNumArgs
                                                      n
                                                      (length l)
                                   | all (argCheck p) l -> Nothing
                                   | otherwise -> Just $ te $ OtherTypeError "types not correct."
      checkArgListMatches tc tcs = if all (==tc) tcs
                                   then Nothing
                                   else Just $ te (WrongTypes tc tcs)
      te = TypeError sp
      argCheck (ExactType et) t = et == t
      argCheck (Predicate p _) t = p t
      pe = propagateUnknownError


checkTypes _ x _ _ = error $ "can't check types of non type list: " ++ show x



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
-- ArgCheck ----------------------------------------------------
data ArgCheck  = ExactType (Type) 
               | Predicate (TypePred) (TypePredError) 
-- cata
sem_ArgCheck :: ArgCheck  ->
                T_ArgCheck 
sem_ArgCheck (ExactType _type )  =
    (sem_ArgCheck_ExactType (sem_Type _type ) )
sem_ArgCheck (Predicate _typePred _typePredError )  =
    (sem_ArgCheck_Predicate _typePred _typePredError )
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
sem_ArgCheck_ExactType :: T_Type  ->
                          T_ArgCheck 
sem_ArgCheck_ExactType type_  =
    (let 
     in  ( ))
sem_ArgCheck_Predicate :: TypePred ->
                          TypePredError ->
                          T_ArgCheck 
sem_ArgCheck_Predicate typePred_ typePredError_  =
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
                    (AllSameType $ ScalarType "Boolean")
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
                 | CastKeyword (Expression) (TypeName) 
                 | CastOp (Expression) (TypeName) 
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
sem_Expression (CastKeyword _expression _typeName )  =
    (sem_Expression_CastKeyword (sem_Expression _expression ) (sem_TypeName _typeName ) )
sem_Expression (CastOp _expression _typeName )  =
    (sem_Expression_CastOp (sem_Expression _expression ) (sem_TypeName _typeName ) )
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
                  ScalarType "Boolean"
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
sem_Expression_CastKeyword :: T_Expression  ->
                              T_TypeName  ->
                              T_Expression 
sem_Expression_CastKeyword expression_ typeName_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _expressionOinLoop :: Bool
              _expressionOsourcePos :: MySourcePos
              _typeNameOinLoop :: Bool
              _typeNameOsourcePos :: MySourcePos
              _expressionImessages :: ([Message])
              _expressionInodeType :: Type
              _typeNameImessages :: ([Message])
              _typeNameInodeType :: Type
              _lhsOmessages =
                  _expressionImessages ++ _typeNameImessages
              _lhsOnodeType =
                  _expressionInodeType `setUnknown` _typeNameInodeType
              _expressionOinLoop =
                  _lhsIinLoop
              _expressionOsourcePos =
                  _lhsIsourcePos
              _typeNameOinLoop =
                  _lhsIinLoop
              _typeNameOsourcePos =
                  _lhsIsourcePos
              ( _expressionImessages,_expressionInodeType) =
                  (expression_ _expressionOinLoop _expressionOsourcePos )
              ( _typeNameImessages,_typeNameInodeType) =
                  (typeName_ _typeNameOinLoop _typeNameOsourcePos )
          in  ( _lhsOmessages,_lhsOnodeType)))
sem_Expression_CastOp :: T_Expression  ->
                         T_TypeName  ->
                         T_Expression 
sem_Expression_CastOp expression_ typeName_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _expressionOinLoop :: Bool
              _expressionOsourcePos :: MySourcePos
              _typeNameOinLoop :: Bool
              _typeNameOsourcePos :: MySourcePos
              _expressionImessages :: ([Message])
              _expressionInodeType :: Type
              _typeNameImessages :: ([Message])
              _typeNameInodeType :: Type
              _lhsOmessages =
                  _expressionImessages ++ _typeNameImessages
              _lhsOnodeType =
                  _expressionInodeType `setUnknown` _typeNameInodeType
              _expressionOinLoop =
                  _lhsIinLoop
              _expressionOsourcePos =
                  _lhsIsourcePos
              _typeNameOinLoop =
                  _lhsIinLoop
              _typeNameOsourcePos =
                  _lhsIsourcePos
              ( _expressionImessages,_expressionInodeType) =
                  (expression_ _expressionOinLoop _expressionOsourcePos )
              ( _typeNameImessages,_typeNameInodeType) =
                  (typeName_ _typeNameOinLoop _typeNameOsourcePos )
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
                  ScalarType "Float"
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
                    ArrayVal -> ct AllSameType1Any
                                   (RetTypeFun (\t -> ArrayType $ head t))
                    Substring -> ct
                                   (ExactList [ScalarType "String"
                                              ,ScalarType "Integer"
                                              ,ScalarType "Integer"])
                                   (ConstRetType (ScalarType "String"))
                    Between -> ct
                                   (AllSameTypeNumAny 3)
                                   (ConstRetType (ScalarType "Boolean"))
                    ArraySub -> ct
                                   (ExactPredList
                                     [Predicate isArrayType NotArrayType
                                     ,ExactType (ScalarType "Integer")])
                                   (RetTypeFun (\t -> typeFromArray $ head t))
                    Operator s | s == "=" -> ct
                                               (AllSameTypeNumAny 2)
                                               (ConstRetType (ScalarType "Boolean"))
                               | s == "not" -> ct
                                               (ExactList [ScalarType "Boolean"])
                                               (ConstRetType (ScalarType "Boolean"))
                    _ -> UnknownType
                  where
                    ct = checkTypes _lhsIsourcePos _argsInodeType
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
                  ScalarType "Integer"
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
                  ScalarType "String"
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
data FunName  = ArraySub 
              | ArrayVal 
              | Between 
              | Operator (String) 
              | RowCtor 
              | SimpleFun (String) 
              | Substring 
              deriving ( Eq,Show)
-- cata
sem_FunName :: FunName  ->
               T_FunName 
sem_FunName (ArraySub )  =
    (sem_FunName_ArraySub )
sem_FunName (ArrayVal )  =
    (sem_FunName_ArrayVal )
sem_FunName (Between )  =
    (sem_FunName_Between )
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
sem_FunName_ArrayVal :: T_FunName 
sem_FunName_ArrayVal  =
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
                  ArrayVal
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
                   | LeftUnary 
                   | RightUnary 
-- cata
sem_OperatorType :: OperatorType  ->
                    T_OperatorType 
sem_OperatorType (BinaryOp )  =
    (sem_OperatorType_BinaryOp )
sem_OperatorType (LeftUnary )  =
    (sem_OperatorType_LeftUnary )
sem_OperatorType (RightUnary )  =
    (sem_OperatorType_RightUnary )
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
sem_OperatorType_LeftUnary :: T_OperatorType 
sem_OperatorType_LeftUnary  =
    (let 
     in  ( ))
sem_OperatorType_RightUnary :: T_OperatorType 
sem_OperatorType_RightUnary  =
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
data TypeErrorInfo  = NeedOneOrMoreArgs 
                    | NotArrayType (Type) 
                    | OtherTypeError (String) 
                    | WrongNumArgs (Int) (Int) 
                    | WrongType (Type) (Type) 
                    | WrongTypeList ([Type]) ([Type]) 
                    | WrongTypes (Type) ([Type]) 
                    deriving ( Eq,Show)
-- cata
sem_TypeErrorInfo :: TypeErrorInfo  ->
                     T_TypeErrorInfo 
sem_TypeErrorInfo (NeedOneOrMoreArgs )  =
    (sem_TypeErrorInfo_NeedOneOrMoreArgs )
sem_TypeErrorInfo (NotArrayType _got )  =
    (sem_TypeErrorInfo_NotArrayType (sem_Type _got ) )
sem_TypeErrorInfo (OtherTypeError _string )  =
    (sem_TypeErrorInfo_OtherTypeError _string )
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
sem_TypeErrorInfo_NeedOneOrMoreArgs :: T_TypeErrorInfo 
sem_TypeErrorInfo_NeedOneOrMoreArgs  =
    (let 
     in  ( ))
sem_TypeErrorInfo_NotArrayType :: T_Type  ->
                                  T_TypeErrorInfo 
sem_TypeErrorInfo_NotArrayType got_  =
    (let 
     in  ( ))
sem_TypeErrorInfo_OtherTypeError :: String ->
                                    T_TypeErrorInfo 
sem_TypeErrorInfo_OtherTypeError string_  =
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
sem_TypeName (ArrayTypeName _typeName )  =
    (sem_TypeName_ArrayTypeName (sem_TypeName _typeName ) )
sem_TypeName (PrecTypeName _string _integer )  =
    (sem_TypeName_PrecTypeName _string _integer )
sem_TypeName (SetOfTypeName _typeName )  =
    (sem_TypeName_SetOfTypeName (sem_TypeName _typeName ) )
sem_TypeName (SimpleTypeName _string )  =
    (sem_TypeName_SimpleTypeName _string )
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
sem_TypeName_ArrayTypeName typeName_  =
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
sem_TypeName_PrecTypeName :: String ->
                             Integer ->
                             T_TypeName 
sem_TypeName_PrecTypeName string_ integer_  =
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
sem_TypeName_SetOfTypeName typeName_  =
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
sem_TypeName_SimpleTypeName :: String ->
                               T_TypeName 
sem_TypeName_SimpleTypeName string_  =
    (\ _lhsIinLoop
       _lhsIsourcePos ->
         (let _lhsOmessages :: ([Message])
              _lhsOnodeType :: Type
              _lhsOmessages =
                  []
              _lhsOnodeType =
                  UnknownType
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