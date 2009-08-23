

-- UUAGC 0.9.10 (Ast.ag)
module Ast where



binOpToSymbol :: BinOp -> String
binOpToSymbol op = case op of
                        Plus -> "+"
                        Minus -> "-"
                        Mult -> "*"
                        Div -> "/"
                        Pow -> "^"
                        Mod -> "%"
                        Eql -> "="
                        And -> "and"
                        Or -> "or"
                        Conc -> "||"
                        Like -> "like"
                        Cast -> "::"
                        NotEql -> "<>"
                        Lt -> "<"
                        Gt -> ">"
                        Lte -> "<="
                        Gte -> ">="
                        DistBetween -> "<->"

unOpToSymbol :: UnOp -> String
unOpToSymbol op = case op of
                         Not -> "not"
                         IsNull -> "is null"
                         IsNotNull -> "is not null"
                         SetOf -> "setof"
                         Abs -> "@"
                         Neg -> "-"



makeSelect :: Statement
makeSelect = Select Dupes (SelectList [SelExp (Identifier "*")] [])
                    Nothing Nothing [] Nothing [] Asc Nothing Nothing

runAtts :: StatementList -> Int
runAtts sts = sem_StatementList sts


-- AttributeDef ------------------------------------------------
data AttributeDef  = AttributeDef (String) (String) (Maybe Expression) (RowConstraintList) 
                   deriving ( Eq,Show)
-- cata
sem_AttributeDef :: AttributeDef  ->
                    T_AttributeDef 
sem_AttributeDef (AttributeDef _name _typ _check _cons )  =
    (sem_AttributeDef_AttributeDef _name _typ _check (sem_RowConstraintList _cons ) )
-- semantic domain
type T_AttributeDef  = ( )
sem_AttributeDef_AttributeDef :: String ->
                                 String ->
                                 (Maybe Expression) ->
                                 T_RowConstraintList  ->
                                 T_AttributeDef 
sem_AttributeDef_AttributeDef name_ typ_ check_ cons_  =
    (let 
     in  ( ))
-- AttributeDefList --------------------------------------------
type AttributeDefList  = [(AttributeDef)]
-- cata
sem_AttributeDefList :: AttributeDefList  ->
                        T_AttributeDefList 
sem_AttributeDefList list  =
    (Prelude.foldr sem_AttributeDefList_Cons sem_AttributeDefList_Nil (Prelude.map sem_AttributeDef list) )
-- semantic domain
type T_AttributeDefList  = ( )
sem_AttributeDefList_Cons :: T_AttributeDef  ->
                             T_AttributeDefList  ->
                             T_AttributeDefList 
sem_AttributeDefList_Cons hd_ tl_  =
    (let 
     in  ( ))
sem_AttributeDefList_Nil :: T_AttributeDefList 
sem_AttributeDefList_Nil  =
    (let 
     in  ( ))
-- BinOp -------------------------------------------------------
data BinOp  = And 
            | Cast 
            | Conc 
            | DistBetween 
            | Div 
            | Eql 
            | Gt 
            | Gte 
            | Like 
            | Lt 
            | Lte 
            | Minus 
            | Mod 
            | Mult 
            | NotEql 
            | Or 
            | Plus 
            | Pow 
            deriving ( Eq,Show)
-- cata
sem_BinOp :: BinOp  ->
             T_BinOp 
sem_BinOp (And )  =
    (sem_BinOp_And )
sem_BinOp (Cast )  =
    (sem_BinOp_Cast )
sem_BinOp (Conc )  =
    (sem_BinOp_Conc )
sem_BinOp (DistBetween )  =
    (sem_BinOp_DistBetween )
sem_BinOp (Div )  =
    (sem_BinOp_Div )
sem_BinOp (Eql )  =
    (sem_BinOp_Eql )
sem_BinOp (Gt )  =
    (sem_BinOp_Gt )
sem_BinOp (Gte )  =
    (sem_BinOp_Gte )
sem_BinOp (Like )  =
    (sem_BinOp_Like )
sem_BinOp (Lt )  =
    (sem_BinOp_Lt )
sem_BinOp (Lte )  =
    (sem_BinOp_Lte )
sem_BinOp (Minus )  =
    (sem_BinOp_Minus )
sem_BinOp (Mod )  =
    (sem_BinOp_Mod )
sem_BinOp (Mult )  =
    (sem_BinOp_Mult )
sem_BinOp (NotEql )  =
    (sem_BinOp_NotEql )
sem_BinOp (Or )  =
    (sem_BinOp_Or )
sem_BinOp (Plus )  =
    (sem_BinOp_Plus )
sem_BinOp (Pow )  =
    (sem_BinOp_Pow )
-- semantic domain
type T_BinOp  = ( )
sem_BinOp_And :: T_BinOp 
sem_BinOp_And  =
    (let 
     in  ( ))
sem_BinOp_Cast :: T_BinOp 
sem_BinOp_Cast  =
    (let 
     in  ( ))
sem_BinOp_Conc :: T_BinOp 
sem_BinOp_Conc  =
    (let 
     in  ( ))
sem_BinOp_DistBetween :: T_BinOp 
sem_BinOp_DistBetween  =
    (let 
     in  ( ))
sem_BinOp_Div :: T_BinOp 
sem_BinOp_Div  =
    (let 
     in  ( ))
sem_BinOp_Eql :: T_BinOp 
sem_BinOp_Eql  =
    (let 
     in  ( ))
sem_BinOp_Gt :: T_BinOp 
sem_BinOp_Gt  =
    (let 
     in  ( ))
sem_BinOp_Gte :: T_BinOp 
sem_BinOp_Gte  =
    (let 
     in  ( ))
sem_BinOp_Like :: T_BinOp 
sem_BinOp_Like  =
    (let 
     in  ( ))
sem_BinOp_Lt :: T_BinOp 
sem_BinOp_Lt  =
    (let 
     in  ( ))
sem_BinOp_Lte :: T_BinOp 
sem_BinOp_Lte  =
    (let 
     in  ( ))
sem_BinOp_Minus :: T_BinOp 
sem_BinOp_Minus  =
    (let 
     in  ( ))
sem_BinOp_Mod :: T_BinOp 
sem_BinOp_Mod  =
    (let 
     in  ( ))
sem_BinOp_Mult :: T_BinOp 
sem_BinOp_Mult  =
    (let 
     in  ( ))
sem_BinOp_NotEql :: T_BinOp 
sem_BinOp_NotEql  =
    (let 
     in  ( ))
sem_BinOp_Or :: T_BinOp 
sem_BinOp_Or  =
    (let 
     in  ( ))
sem_BinOp_Plus :: T_BinOp 
sem_BinOp_Plus  =
    (let 
     in  ( ))
sem_BinOp_Pow :: T_BinOp 
sem_BinOp_Pow  =
    (let 
     in  ( ))
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
type T_Cascade  = ( )
sem_Cascade_Cascade :: T_Cascade 
sem_Cascade_Cascade  =
    (let 
     in  ( ))
sem_Cascade_Restrict :: T_Cascade 
sem_Cascade_Restrict  =
    (let 
     in  ( ))
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
type T_CombineType  = ( )
sem_CombineType_Except :: T_CombineType 
sem_CombineType_Except  =
    (let 
     in  ( ))
sem_CombineType_Intersect :: T_CombineType 
sem_CombineType_Intersect  =
    (let 
     in  ( ))
sem_CombineType_Union :: T_CombineType 
sem_CombineType_Union  =
    (let 
     in  ( ))
sem_CombineType_UnionAll :: T_CombineType 
sem_CombineType_UnionAll  =
    (let 
     in  ( ))
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
type T_Constraint  = ( )
sem_Constraint_CheckConstraint :: T_Expression  ->
                                  T_Constraint 
sem_Constraint_CheckConstraint expression_  =
    (let 
     in  ( ))
sem_Constraint_PrimaryKeyConstraint :: T_StringList  ->
                                       T_Constraint 
sem_Constraint_PrimaryKeyConstraint stringList_  =
    (let 
     in  ( ))
sem_Constraint_ReferenceConstraint :: T_StringList  ->
                                      String ->
                                      T_StringList  ->
                                      T_Cascade  ->
                                      T_Cascade  ->
                                      T_Constraint 
sem_Constraint_ReferenceConstraint atts_ table_ tableAtts_ onUpdate_ onDelete_  =
    (let 
     in  ( ))
sem_Constraint_UniqueConstraint :: T_StringList  ->
                                   T_Constraint 
sem_Constraint_UniqueConstraint stringList_  =
    (let 
     in  ( ))
-- ConstraintList ----------------------------------------------
type ConstraintList  = [(Constraint)]
-- cata
sem_ConstraintList :: ConstraintList  ->
                      T_ConstraintList 
sem_ConstraintList list  =
    (Prelude.foldr sem_ConstraintList_Cons sem_ConstraintList_Nil (Prelude.map sem_Constraint list) )
-- semantic domain
type T_ConstraintList  = ( )
sem_ConstraintList_Cons :: T_Constraint  ->
                           T_ConstraintList  ->
                           T_ConstraintList 
sem_ConstraintList_Cons hd_ tl_  =
    (let 
     in  ( ))
sem_ConstraintList_Nil :: T_ConstraintList 
sem_ConstraintList_Nil  =
    (let 
     in  ( ))
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
type T_CopySource  = ( )
sem_CopySource_CopyFilename :: String ->
                               T_CopySource 
sem_CopySource_CopyFilename string_  =
    (let 
     in  ( ))
sem_CopySource_Stdin :: T_CopySource 
sem_CopySource_Stdin  =
    (let 
     in  ( ))
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
type T_Direction  = ( )
sem_Direction_Asc :: T_Direction 
sem_Direction_Asc  =
    (let 
     in  ( ))
sem_Direction_Desc :: T_Direction 
sem_Direction_Desc  =
    (let 
     in  ( ))
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
type T_Distinct  = ( )
sem_Distinct_Distinct :: T_Distinct 
sem_Distinct_Distinct  =
    (let 
     in  ( ))
sem_Distinct_Dupes :: T_Distinct 
sem_Distinct_Dupes  =
    (let 
     in  ( ))
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
type T_DropType  = ( )
sem_DropType_Domain :: T_DropType 
sem_DropType_Domain  =
    (let 
     in  ( ))
sem_DropType_Table :: T_DropType 
sem_DropType_Table  =
    (let 
     in  ( ))
sem_DropType_Type :: T_DropType 
sem_DropType_Type  =
    (let 
     in  ( ))
sem_DropType_View :: T_DropType 
sem_DropType_View  =
    (let 
     in  ( ))
-- Expression --------------------------------------------------
data Expression  = ArrayLit (ExpressionList) 
                 | ArraySub (Expression) (ExpressionList) 
                 | Between (Expression) (Expression) (Expression) 
                 | BinOpCall (BinOp) (Expression) (Expression) 
                 | BooleanLit (Bool) 
                 | Case (ExpressionListExpressionPairList) (Maybe Expression) 
                 | CastKeyword (Expression) (TypeName) 
                 | Exists (Statement) 
                 | FloatLit (Double) 
                 | FunCall (String) (ExpressionList) 
                 | Identifier (String) 
                 | InPredicate (Expression) (Bool) (InList) 
                 | IntegerLit (Integer) 
                 | NullLit 
                 | PositionalArg (Integer) 
                 | Row (ExpressionList) 
                 | ScalarSubQuery (Statement) 
                 | StringLit (String) (String) 
                 | Substring (Expression) (Expression) (Expression) 
                 | UnOpCall (UnOp) (Expression) 
                 | WindowFn (Expression) (ExpressionList) (ExpressionList) (Direction) 
                 deriving ( Eq,Show)
-- cata
sem_Expression :: Expression  ->
                  T_Expression 
sem_Expression (ArrayLit _expressionList )  =
    (sem_Expression_ArrayLit (sem_ExpressionList _expressionList ) )
sem_Expression (ArraySub _expression _expressionList )  =
    (sem_Expression_ArraySub (sem_Expression _expression ) (sem_ExpressionList _expressionList ) )
sem_Expression (Between _val _lower _upper )  =
    (sem_Expression_Between (sem_Expression _val ) (sem_Expression _lower ) (sem_Expression _upper ) )
sem_Expression (BinOpCall _binOp _arg1 _arg2 )  =
    (sem_Expression_BinOpCall (sem_BinOp _binOp ) (sem_Expression _arg1 ) (sem_Expression _arg2 ) )
sem_Expression (BooleanLit _bool )  =
    (sem_Expression_BooleanLit _bool )
sem_Expression (Case _cases _els )  =
    (sem_Expression_Case (sem_ExpressionListExpressionPairList _cases ) _els )
sem_Expression (CastKeyword _expression _typeName )  =
    (sem_Expression_CastKeyword (sem_Expression _expression ) (sem_TypeName _typeName ) )
sem_Expression (Exists _statement )  =
    (sem_Expression_Exists (sem_Statement _statement ) )
sem_Expression (FloatLit _double )  =
    (sem_Expression_FloatLit _double )
sem_Expression (FunCall _string _expressionList )  =
    (sem_Expression_FunCall _string (sem_ExpressionList _expressionList ) )
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
sem_Expression (Row _expressionList )  =
    (sem_Expression_Row (sem_ExpressionList _expressionList ) )
sem_Expression (ScalarSubQuery _statement )  =
    (sem_Expression_ScalarSubQuery (sem_Statement _statement ) )
sem_Expression (StringLit _quote _value )  =
    (sem_Expression_StringLit _quote _value )
sem_Expression (Substring _str _from _for )  =
    (sem_Expression_Substring (sem_Expression _str ) (sem_Expression _from ) (sem_Expression _for ) )
sem_Expression (UnOpCall _unOp _expression )  =
    (sem_Expression_UnOpCall (sem_UnOp _unOp ) (sem_Expression _expression ) )
sem_Expression (WindowFn _fn _partitionBy _orderBy _dir )  =
    (sem_Expression_WindowFn (sem_Expression _fn ) (sem_ExpressionList _partitionBy ) (sem_ExpressionList _orderBy ) (sem_Direction _dir ) )
-- semantic domain
type T_Expression  = ( )
sem_Expression_ArrayLit :: T_ExpressionList  ->
                           T_Expression 
sem_Expression_ArrayLit expressionList_  =
    (let 
     in  ( ))
sem_Expression_ArraySub :: T_Expression  ->
                           T_ExpressionList  ->
                           T_Expression 
sem_Expression_ArraySub expression_ expressionList_  =
    (let 
     in  ( ))
sem_Expression_Between :: T_Expression  ->
                          T_Expression  ->
                          T_Expression  ->
                          T_Expression 
sem_Expression_Between val_ lower_ upper_  =
    (let 
     in  ( ))
sem_Expression_BinOpCall :: T_BinOp  ->
                            T_Expression  ->
                            T_Expression  ->
                            T_Expression 
sem_Expression_BinOpCall binOp_ arg1_ arg2_  =
    (let 
     in  ( ))
sem_Expression_BooleanLit :: Bool ->
                             T_Expression 
sem_Expression_BooleanLit bool_  =
    (let 
     in  ( ))
sem_Expression_Case :: T_ExpressionListExpressionPairList  ->
                       (Maybe Expression) ->
                       T_Expression 
sem_Expression_Case cases_ els_  =
    (let 
     in  ( ))
sem_Expression_CastKeyword :: T_Expression  ->
                              T_TypeName  ->
                              T_Expression 
sem_Expression_CastKeyword expression_ typeName_  =
    (let 
     in  ( ))
sem_Expression_Exists :: T_Statement  ->
                         T_Expression 
sem_Expression_Exists statement_  =
    (let _statementIsum :: Int
         ( _statementIsum) =
             (statement_ )
     in  ( ))
sem_Expression_FloatLit :: Double ->
                           T_Expression 
sem_Expression_FloatLit double_  =
    (let 
     in  ( ))
sem_Expression_FunCall :: String ->
                          T_ExpressionList  ->
                          T_Expression 
sem_Expression_FunCall string_ expressionList_  =
    (let 
     in  ( ))
sem_Expression_Identifier :: String ->
                             T_Expression 
sem_Expression_Identifier string_  =
    (let 
     in  ( ))
sem_Expression_InPredicate :: T_Expression  ->
                              Bool ->
                              T_InList  ->
                              T_Expression 
sem_Expression_InPredicate expression_ bool_ inList_  =
    (let 
     in  ( ))
sem_Expression_IntegerLit :: Integer ->
                             T_Expression 
sem_Expression_IntegerLit integer_  =
    (let 
     in  ( ))
sem_Expression_NullLit :: T_Expression 
sem_Expression_NullLit  =
    (let 
     in  ( ))
sem_Expression_PositionalArg :: Integer ->
                                T_Expression 
sem_Expression_PositionalArg integer_  =
    (let 
     in  ( ))
sem_Expression_Row :: T_ExpressionList  ->
                      T_Expression 
sem_Expression_Row expressionList_  =
    (let 
     in  ( ))
sem_Expression_ScalarSubQuery :: T_Statement  ->
                                 T_Expression 
sem_Expression_ScalarSubQuery statement_  =
    (let _statementIsum :: Int
         ( _statementIsum) =
             (statement_ )
     in  ( ))
sem_Expression_StringLit :: String ->
                            String ->
                            T_Expression 
sem_Expression_StringLit quote_ value_  =
    (let 
     in  ( ))
sem_Expression_Substring :: T_Expression  ->
                            T_Expression  ->
                            T_Expression  ->
                            T_Expression 
sem_Expression_Substring str_ from_ for_  =
    (let 
     in  ( ))
sem_Expression_UnOpCall :: T_UnOp  ->
                           T_Expression  ->
                           T_Expression 
sem_Expression_UnOpCall unOp_ expression_  =
    (let 
     in  ( ))
sem_Expression_WindowFn :: T_Expression  ->
                           T_ExpressionList  ->
                           T_ExpressionList  ->
                           T_Direction  ->
                           T_Expression 
sem_Expression_WindowFn fn_ partitionBy_ orderBy_ dir_  =
    (let 
     in  ( ))
-- ExpressionList ----------------------------------------------
type ExpressionList  = [(Expression)]
-- cata
sem_ExpressionList :: ExpressionList  ->
                      T_ExpressionList 
sem_ExpressionList list  =
    (Prelude.foldr sem_ExpressionList_Cons sem_ExpressionList_Nil (Prelude.map sem_Expression list) )
-- semantic domain
type T_ExpressionList  = ( )
sem_ExpressionList_Cons :: T_Expression  ->
                           T_ExpressionList  ->
                           T_ExpressionList 
sem_ExpressionList_Cons hd_ tl_  =
    (let 
     in  ( ))
sem_ExpressionList_Nil :: T_ExpressionList 
sem_ExpressionList_Nil  =
    (let 
     in  ( ))
-- ExpressionListExpressionPair --------------------------------
type ExpressionListExpressionPair  = ( (ExpressionList),(Expression))
-- cata
sem_ExpressionListExpressionPair :: ExpressionListExpressionPair  ->
                                    T_ExpressionListExpressionPair 
sem_ExpressionListExpressionPair ( x1,x2)  =
    (sem_ExpressionListExpressionPair_Tuple (sem_ExpressionList x1 ) (sem_Expression x2 ) )
-- semantic domain
type T_ExpressionListExpressionPair  = ( )
sem_ExpressionListExpressionPair_Tuple :: T_ExpressionList  ->
                                          T_Expression  ->
                                          T_ExpressionListExpressionPair 
sem_ExpressionListExpressionPair_Tuple x1_ x2_  =
    (let 
     in  ( ))
-- ExpressionListExpressionPairList ----------------------------
type ExpressionListExpressionPairList  = [(ExpressionListExpressionPair)]
-- cata
sem_ExpressionListExpressionPairList :: ExpressionListExpressionPairList  ->
                                        T_ExpressionListExpressionPairList 
sem_ExpressionListExpressionPairList list  =
    (Prelude.foldr sem_ExpressionListExpressionPairList_Cons sem_ExpressionListExpressionPairList_Nil (Prelude.map sem_ExpressionListExpressionPair list) )
-- semantic domain
type T_ExpressionListExpressionPairList  = ( )
sem_ExpressionListExpressionPairList_Cons :: T_ExpressionListExpressionPair  ->
                                             T_ExpressionListExpressionPairList  ->
                                             T_ExpressionListExpressionPairList 
sem_ExpressionListExpressionPairList_Cons hd_ tl_  =
    (let 
     in  ( ))
sem_ExpressionListExpressionPairList_Nil :: T_ExpressionListExpressionPairList 
sem_ExpressionListExpressionPairList_Nil  =
    (let 
     in  ( ))
-- ExpressionListList ------------------------------------------
type ExpressionListList  = [(ExpressionList)]
-- cata
sem_ExpressionListList :: ExpressionListList  ->
                          T_ExpressionListList 
sem_ExpressionListList list  =
    (Prelude.foldr sem_ExpressionListList_Cons sem_ExpressionListList_Nil (Prelude.map sem_ExpressionList list) )
-- semantic domain
type T_ExpressionListList  = ( )
sem_ExpressionListList_Cons :: T_ExpressionList  ->
                               T_ExpressionListList  ->
                               T_ExpressionListList 
sem_ExpressionListList_Cons hd_ tl_  =
    (let 
     in  ( ))
sem_ExpressionListList_Nil :: T_ExpressionListList 
sem_ExpressionListList_Nil  =
    (let 
     in  ( ))
-- ExpressionStatementListPair ---------------------------------
type ExpressionStatementListPair  = ( (Expression),(StatementList))
-- cata
sem_ExpressionStatementListPair :: ExpressionStatementListPair  ->
                                   T_ExpressionStatementListPair 
sem_ExpressionStatementListPair ( x1,x2)  =
    (sem_ExpressionStatementListPair_Tuple (sem_Expression x1 ) (sem_StatementList x2 ) )
-- semantic domain
type T_ExpressionStatementListPair  = ( )
sem_ExpressionStatementListPair_Tuple :: T_Expression  ->
                                         T_StatementList  ->
                                         T_ExpressionStatementListPair 
sem_ExpressionStatementListPair_Tuple x1_ x2_  =
    (let _x2Isum :: Int
         ( _x2Isum) =
             (x2_ )
     in  ( ))
-- ExpressionStatementListPairList -----------------------------
type ExpressionStatementListPairList  = [(ExpressionStatementListPair)]
-- cata
sem_ExpressionStatementListPairList :: ExpressionStatementListPairList  ->
                                       T_ExpressionStatementListPairList 
sem_ExpressionStatementListPairList list  =
    (Prelude.foldr sem_ExpressionStatementListPairList_Cons sem_ExpressionStatementListPairList_Nil (Prelude.map sem_ExpressionStatementListPair list) )
-- semantic domain
type T_ExpressionStatementListPairList  = ( )
sem_ExpressionStatementListPairList_Cons :: T_ExpressionStatementListPair  ->
                                            T_ExpressionStatementListPairList  ->
                                            T_ExpressionStatementListPairList 
sem_ExpressionStatementListPairList_Cons hd_ tl_  =
    (let 
     in  ( ))
sem_ExpressionStatementListPairList_Nil :: T_ExpressionStatementListPairList 
sem_ExpressionStatementListPairList_Nil  =
    (let 
     in  ( ))
-- ExpressonListStatementListPair ------------------------------
type ExpressonListStatementListPair  = ( (ExpressionList),(StatementList))
-- cata
sem_ExpressonListStatementListPair :: ExpressonListStatementListPair  ->
                                      T_ExpressonListStatementListPair 
sem_ExpressonListStatementListPair ( x1,x2)  =
    (sem_ExpressonListStatementListPair_Tuple (sem_ExpressionList x1 ) (sem_StatementList x2 ) )
-- semantic domain
type T_ExpressonListStatementListPair  = ( )
sem_ExpressonListStatementListPair_Tuple :: T_ExpressionList  ->
                                            T_StatementList  ->
                                            T_ExpressonListStatementListPair 
sem_ExpressonListStatementListPair_Tuple x1_ x2_  =
    (let _x2Isum :: Int
         ( _x2Isum) =
             (x2_ )
     in  ( ))
-- ExpressonListStatementListPairList --------------------------
type ExpressonListStatementListPairList  = [(ExpressonListStatementListPair)]
-- cata
sem_ExpressonListStatementListPairList :: ExpressonListStatementListPairList  ->
                                          T_ExpressonListStatementListPairList 
sem_ExpressonListStatementListPairList list  =
    (Prelude.foldr sem_ExpressonListStatementListPairList_Cons sem_ExpressonListStatementListPairList_Nil (Prelude.map sem_ExpressonListStatementListPair list) )
-- semantic domain
type T_ExpressonListStatementListPairList  = ( )
sem_ExpressonListStatementListPairList_Cons :: T_ExpressonListStatementListPair  ->
                                               T_ExpressonListStatementListPairList  ->
                                               T_ExpressonListStatementListPairList 
sem_ExpressonListStatementListPairList_Cons hd_ tl_  =
    (let 
     in  ( ))
sem_ExpressonListStatementListPairList_Nil :: T_ExpressonListStatementListPairList 
sem_ExpressonListStatementListPairList_Nil  =
    (let 
     in  ( ))
-- FnBody ------------------------------------------------------
data FnBody  = PlpgsqlFnBody (VarDefList) (StatementList) 
             | SqlFnBody (StatementList) 
             deriving ( Eq,Show)
-- cata
sem_FnBody :: FnBody  ->
              T_FnBody 
sem_FnBody (PlpgsqlFnBody _varDefList _statementList )  =
    (sem_FnBody_PlpgsqlFnBody (sem_VarDefList _varDefList ) (sem_StatementList _statementList ) )
sem_FnBody (SqlFnBody _statementList )  =
    (sem_FnBody_SqlFnBody (sem_StatementList _statementList ) )
-- semantic domain
type T_FnBody  = ( )
sem_FnBody_PlpgsqlFnBody :: T_VarDefList  ->
                            T_StatementList  ->
                            T_FnBody 
sem_FnBody_PlpgsqlFnBody varDefList_ statementList_  =
    (let _statementListIsum :: Int
         ( _statementListIsum) =
             (statementList_ )
     in  ( ))
sem_FnBody_SqlFnBody :: T_StatementList  ->
                        T_FnBody 
sem_FnBody_SqlFnBody statementList_  =
    (let _statementListIsum :: Int
         ( _statementListIsum) =
             (statementList_ )
     in  ( ))
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
type T_IfExists  = ( )
sem_IfExists_IfExists :: T_IfExists 
sem_IfExists_IfExists  =
    (let 
     in  ( ))
sem_IfExists_Require :: T_IfExists 
sem_IfExists_Require  =
    (let 
     in  ( ))
-- InList ------------------------------------------------------
data InList  = InList (ExpressionList) 
             | InSelect (Statement) 
             deriving ( Eq,Show)
-- cata
sem_InList :: InList  ->
              T_InList 
sem_InList (InList _expressionList )  =
    (sem_InList_InList (sem_ExpressionList _expressionList ) )
sem_InList (InSelect _statement )  =
    (sem_InList_InSelect (sem_Statement _statement ) )
-- semantic domain
type T_InList  = ( )
sem_InList_InList :: T_ExpressionList  ->
                     T_InList 
sem_InList_InList expressionList_  =
    (let 
     in  ( ))
sem_InList_InSelect :: T_Statement  ->
                       T_InList 
sem_InList_InSelect statement_  =
    (let _statementIsum :: Int
         ( _statementIsum) =
             (statement_ )
     in  ( ))
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
type T_JoinExpression  = ( )
sem_JoinExpression_JoinOn :: T_Expression  ->
                             T_JoinExpression 
sem_JoinExpression_JoinOn expression_  =
    (let 
     in  ( ))
sem_JoinExpression_JoinUsing :: T_StringList  ->
                                T_JoinExpression 
sem_JoinExpression_JoinUsing stringList_  =
    (let 
     in  ( ))
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
type T_JoinType  = ( )
sem_JoinType_Cross :: T_JoinType 
sem_JoinType_Cross  =
    (let 
     in  ( ))
sem_JoinType_FullOuter :: T_JoinType 
sem_JoinType_FullOuter  =
    (let 
     in  ( ))
sem_JoinType_Inner :: T_JoinType 
sem_JoinType_Inner  =
    (let 
     in  ( ))
sem_JoinType_LeftOuter :: T_JoinType 
sem_JoinType_LeftOuter  =
    (let 
     in  ( ))
sem_JoinType_RightOuter :: T_JoinType 
sem_JoinType_RightOuter  =
    (let 
     in  ( ))
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
type T_Language  = ( )
sem_Language_Plpgsql :: T_Language 
sem_Language_Plpgsql  =
    (let 
     in  ( ))
sem_Language_Sql :: T_Language 
sem_Language_Sql  =
    (let 
     in  ( ))
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
type T_Natural  = ( )
sem_Natural_Natural :: T_Natural 
sem_Natural_Natural  =
    (let 
     in  ( ))
sem_Natural_Unnatural :: T_Natural 
sem_Natural_Unnatural  =
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
type T_ParamDef  = ( )
sem_ParamDef_ParamDef :: String ->
                         T_TypeName  ->
                         T_ParamDef 
sem_ParamDef_ParamDef string_ typeName_  =
    (let 
     in  ( ))
sem_ParamDef_ParamDefTp :: T_TypeName  ->
                           T_ParamDef 
sem_ParamDef_ParamDefTp typeName_  =
    (let 
     in  ( ))
-- ParamDefList ------------------------------------------------
type ParamDefList  = [(ParamDef)]
-- cata
sem_ParamDefList :: ParamDefList  ->
                    T_ParamDefList 
sem_ParamDefList list  =
    (Prelude.foldr sem_ParamDefList_Cons sem_ParamDefList_Nil (Prelude.map sem_ParamDef list) )
-- semantic domain
type T_ParamDefList  = ( )
sem_ParamDefList_Cons :: T_ParamDef  ->
                         T_ParamDefList  ->
                         T_ParamDefList 
sem_ParamDefList_Cons hd_ tl_  =
    (let 
     in  ( ))
sem_ParamDefList_Nil :: T_ParamDefList 
sem_ParamDefList_Nil  =
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
type T_RaiseType  = ( )
sem_RaiseType_RError :: T_RaiseType 
sem_RaiseType_RError  =
    (let 
     in  ( ))
sem_RaiseType_RException :: T_RaiseType 
sem_RaiseType_RException  =
    (let 
     in  ( ))
sem_RaiseType_RNotice :: T_RaiseType 
sem_RaiseType_RNotice  =
    (let 
     in  ( ))
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
type T_RestartIdentity  = ( )
sem_RestartIdentity_ContinueIdentity :: T_RestartIdentity 
sem_RestartIdentity_ContinueIdentity  =
    (let 
     in  ( ))
sem_RestartIdentity_RestartIdentity :: T_RestartIdentity 
sem_RestartIdentity_RestartIdentity  =
    (let 
     in  ( ))
-- RowConstraint -----------------------------------------------
data RowConstraint  = NotNullConstraint 
                    | NullConstraint 
                    | RowCheckConstraint (Expression) 
                    | RowPrimaryKeyConstraint 
                    | RowReferenceConstraint (String) (StringList) (Cascade) (Cascade) 
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
sem_RowConstraint (RowReferenceConstraint _table _atts _onUpdate _onDelete )  =
    (sem_RowConstraint_RowReferenceConstraint _table (sem_StringList _atts ) (sem_Cascade _onUpdate ) (sem_Cascade _onDelete ) )
sem_RowConstraint (RowUniqueConstraint )  =
    (sem_RowConstraint_RowUniqueConstraint )
-- semantic domain
type T_RowConstraint  = ( )
sem_RowConstraint_NotNullConstraint :: T_RowConstraint 
sem_RowConstraint_NotNullConstraint  =
    (let 
     in  ( ))
sem_RowConstraint_NullConstraint :: T_RowConstraint 
sem_RowConstraint_NullConstraint  =
    (let 
     in  ( ))
sem_RowConstraint_RowCheckConstraint :: T_Expression  ->
                                        T_RowConstraint 
sem_RowConstraint_RowCheckConstraint expression_  =
    (let 
     in  ( ))
sem_RowConstraint_RowPrimaryKeyConstraint :: T_RowConstraint 
sem_RowConstraint_RowPrimaryKeyConstraint  =
    (let 
     in  ( ))
sem_RowConstraint_RowReferenceConstraint :: String ->
                                            T_StringList  ->
                                            T_Cascade  ->
                                            T_Cascade  ->
                                            T_RowConstraint 
sem_RowConstraint_RowReferenceConstraint table_ atts_ onUpdate_ onDelete_  =
    (let 
     in  ( ))
sem_RowConstraint_RowUniqueConstraint :: T_RowConstraint 
sem_RowConstraint_RowUniqueConstraint  =
    (let 
     in  ( ))
-- RowConstraintList -------------------------------------------
type RowConstraintList  = [(RowConstraint)]
-- cata
sem_RowConstraintList :: RowConstraintList  ->
                         T_RowConstraintList 
sem_RowConstraintList list  =
    (Prelude.foldr sem_RowConstraintList_Cons sem_RowConstraintList_Nil (Prelude.map sem_RowConstraint list) )
-- semantic domain
type T_RowConstraintList  = ( )
sem_RowConstraintList_Cons :: T_RowConstraint  ->
                              T_RowConstraintList  ->
                              T_RowConstraintList 
sem_RowConstraintList_Cons hd_ tl_  =
    (let 
     in  ( ))
sem_RowConstraintList_Nil :: T_RowConstraintList 
sem_RowConstraintList_Nil  =
    (let 
     in  ( ))
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
type T_SelectItem  = ( )
sem_SelectItem_SelExp :: T_Expression  ->
                         T_SelectItem 
sem_SelectItem_SelExp expression_  =
    (let 
     in  ( ))
sem_SelectItem_SelectItem :: T_Expression  ->
                             String ->
                             T_SelectItem 
sem_SelectItem_SelectItem expression_ string_  =
    (let 
     in  ( ))
-- SelectItemList ----------------------------------------------
type SelectItemList  = [(SelectItem)]
-- cata
sem_SelectItemList :: SelectItemList  ->
                      T_SelectItemList 
sem_SelectItemList list  =
    (Prelude.foldr sem_SelectItemList_Cons sem_SelectItemList_Nil (Prelude.map sem_SelectItem list) )
-- semantic domain
type T_SelectItemList  = ( )
sem_SelectItemList_Cons :: T_SelectItem  ->
                           T_SelectItemList  ->
                           T_SelectItemList 
sem_SelectItemList_Cons hd_ tl_  =
    (let 
     in  ( ))
sem_SelectItemList_Nil :: T_SelectItemList 
sem_SelectItemList_Nil  =
    (let 
     in  ( ))
-- SelectList --------------------------------------------------
data SelectList  = SelectList (SelectItemList) (StringList) 
                 deriving ( Eq,Show)
-- cata
sem_SelectList :: SelectList  ->
                  T_SelectList 
sem_SelectList (SelectList _selectItemList _stringList )  =
    (sem_SelectList_SelectList (sem_SelectItemList _selectItemList ) (sem_StringList _stringList ) )
-- semantic domain
type T_SelectList  = ( )
sem_SelectList_SelectList :: T_SelectItemList  ->
                             T_StringList  ->
                             T_SelectList 
sem_SelectList_SelectList selectItemList_ stringList_  =
    (let 
     in  ( ))
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
type T_SetClause  = ( )
sem_SetClause_RowSetClause :: T_StringList  ->
                              T_ExpressionList  ->
                              T_SetClause 
sem_SetClause_RowSetClause stringList_ expressionList_  =
    (let 
     in  ( ))
sem_SetClause_SetClause :: String ->
                           T_Expression  ->
                           T_SetClause 
sem_SetClause_SetClause string_ expression_  =
    (let 
     in  ( ))
-- SetClauseList -----------------------------------------------
type SetClauseList  = [(SetClause)]
-- cata
sem_SetClauseList :: SetClauseList  ->
                     T_SetClauseList 
sem_SetClauseList list  =
    (Prelude.foldr sem_SetClauseList_Cons sem_SetClauseList_Nil (Prelude.map sem_SetClause list) )
-- semantic domain
type T_SetClauseList  = ( )
sem_SetClauseList_Cons :: T_SetClause  ->
                          T_SetClauseList  ->
                          T_SetClauseList 
sem_SetClauseList_Cons hd_ tl_  =
    (let 
     in  ( ))
sem_SetClauseList_Nil :: T_SetClauseList 
sem_SetClauseList_Nil  =
    (let 
     in  ( ))
-- Statement ---------------------------------------------------
data Statement  = Assignment (String) (Expression) 
                | CaseStatement (Expression) (ExpressonListStatementListPairList) (StatementList) 
                | CombineSelect (CombineType) (Statement) (Statement) 
                | ContinueStatement 
                | Copy (String) (StringList) (CopySource) 
                | CopyData (String) 
                | CreateDomain (String) (String) (Maybe Expression) 
                | CreateFunction (Language) (String) (ParamDefList) (TypeName) (String) (FnBody) (Volatility) 
                | CreateTable (String) (AttributeDefList) (ConstraintList) 
                | CreateTableAs (String) (Statement) 
                | CreateType (String) (TypeAttributeDefList) 
                | CreateView (String) (Statement) 
                | Delete (String) (Maybe Expression) (Maybe SelectList) 
                | DropFunction (IfExists) (StringStringListPairList) (Cascade) 
                | DropSomething (DropType) (IfExists) (StringList) (Cascade) 
                | Execute (Expression) 
                | ExecuteInto (Expression) (StringList) 
                | ForIntegerStatement (String) (Expression) (Expression) (StatementList) 
                | ForSelectStatement (String) (Statement) (StatementList) 
                | If (ExpressionStatementListPairList) (StatementList) 
                | Insert (String) (StringList) (Statement) (Maybe SelectList) 
                | NullStatement 
                | Perform (Expression) 
                | Raise (RaiseType) (String) (ExpressionList) 
                | Return (Maybe Expression) 
                | ReturnNext (Expression) 
                | ReturnQuery (Statement) 
                | Select (Distinct) (SelectList) (Maybe TableRef) (Maybe Expression) (ExpressionList) (Maybe Expression) (ExpressionList) (Direction) (Maybe Expression) (Maybe Expression) 
                | Truncate (StringList) (RestartIdentity) (Cascade) 
                | Update (String) (SetClauseList) (Maybe Expression) (Maybe SelectList) 
                | Values (ExpressionListList) 
                | WhileStatement (Expression) (StatementList) 
                deriving ( Eq,Show)
-- cata
sem_Statement :: Statement  ->
                 T_Statement 
sem_Statement (Assignment _target _value )  =
    (sem_Statement_Assignment _target (sem_Expression _value ) )
sem_Statement (CaseStatement _val _cases _els )  =
    (sem_Statement_CaseStatement (sem_Expression _val ) (sem_ExpressonListStatementListPairList _cases ) (sem_StatementList _els ) )
sem_Statement (CombineSelect _ctype _sel1 _sel2 )  =
    (sem_Statement_CombineSelect (sem_CombineType _ctype ) (sem_Statement _sel1 ) (sem_Statement _sel2 ) )
sem_Statement (ContinueStatement )  =
    (sem_Statement_ContinueStatement )
sem_Statement (Copy _table _targetCols _source )  =
    (sem_Statement_Copy _table (sem_StringList _targetCols ) (sem_CopySource _source ) )
sem_Statement (CopyData _insData )  =
    (sem_Statement_CopyData _insData )
sem_Statement (CreateDomain _name _typ _check )  =
    (sem_Statement_CreateDomain _name _typ _check )
sem_Statement (CreateFunction _lang _name _params _rettype _bodyQuote _body _vol )  =
    (sem_Statement_CreateFunction (sem_Language _lang ) _name (sem_ParamDefList _params ) (sem_TypeName _rettype ) _bodyQuote (sem_FnBody _body ) (sem_Volatility _vol ) )
sem_Statement (CreateTable _name _atts _cons )  =
    (sem_Statement_CreateTable _name (sem_AttributeDefList _atts ) (sem_ConstraintList _cons ) )
sem_Statement (CreateTableAs _name _expr )  =
    (sem_Statement_CreateTableAs _name (sem_Statement _expr ) )
sem_Statement (CreateType _name _atts )  =
    (sem_Statement_CreateType _name (sem_TypeAttributeDefList _atts ) )
sem_Statement (CreateView _name _expr )  =
    (sem_Statement_CreateView _name (sem_Statement _expr ) )
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
    (sem_Statement_ForSelectStatement _var (sem_Statement _sel ) (sem_StatementList _sts ) )
sem_Statement (If _cases _els )  =
    (sem_Statement_If (sem_ExpressionStatementListPairList _cases ) (sem_StatementList _els ) )
sem_Statement (Insert _table _targetCols _insData _returning )  =
    (sem_Statement_Insert _table (sem_StringList _targetCols ) (sem_Statement _insData ) _returning )
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
    (sem_Statement_ReturnQuery (sem_Statement _sel ) )
sem_Statement (Select _selDistinct _selSelectList _selTref _selWhere _selGroupBy _selHaving _selOrderBy _selDir _selLimit _selOffset )  =
    (sem_Statement_Select (sem_Distinct _selDistinct ) (sem_SelectList _selSelectList ) _selTref _selWhere (sem_ExpressionList _selGroupBy ) _selHaving (sem_ExpressionList _selOrderBy ) (sem_Direction _selDir ) _selLimit _selOffset )
sem_Statement (Truncate _tables _restartIdentity _cascade )  =
    (sem_Statement_Truncate (sem_StringList _tables ) (sem_RestartIdentity _restartIdentity ) (sem_Cascade _cascade ) )
sem_Statement (Update _table _assigns _whr _returning )  =
    (sem_Statement_Update _table (sem_SetClauseList _assigns ) _whr _returning )
sem_Statement (Values _expressionListList )  =
    (sem_Statement_Values (sem_ExpressionListList _expressionListList ) )
sem_Statement (WhileStatement _expr _sts )  =
    (sem_Statement_WhileStatement (sem_Expression _expr ) (sem_StatementList _sts ) )
-- semantic domain
type T_Statement  = ( Int)
sem_Statement_Assignment :: String ->
                            T_Expression  ->
                            T_Statement 
sem_Statement_Assignment target_ value_  =
    (let _lhsOsum :: Int
         _lhsOsum =
             1
     in  ( _lhsOsum))
sem_Statement_CaseStatement :: T_Expression  ->
                               T_ExpressonListStatementListPairList  ->
                               T_StatementList  ->
                               T_Statement 
sem_Statement_CaseStatement val_ cases_ els_  =
    (let _lhsOsum :: Int
         _elsIsum :: Int
         _lhsOsum =
             1
         ( _elsIsum) =
             (els_ )
     in  ( _lhsOsum))
sem_Statement_CombineSelect :: T_CombineType  ->
                               T_Statement  ->
                               T_Statement  ->
                               T_Statement 
sem_Statement_CombineSelect ctype_ sel1_ sel2_  =
    (let _lhsOsum :: Int
         _sel1Isum :: Int
         _sel2Isum :: Int
         _lhsOsum =
             1
         ( _sel1Isum) =
             (sel1_ )
         ( _sel2Isum) =
             (sel2_ )
     in  ( _lhsOsum))
sem_Statement_ContinueStatement :: T_Statement 
sem_Statement_ContinueStatement  =
    (let _lhsOsum :: Int
         _lhsOsum =
             1
     in  ( _lhsOsum))
sem_Statement_Copy :: String ->
                      T_StringList  ->
                      T_CopySource  ->
                      T_Statement 
sem_Statement_Copy table_ targetCols_ source_  =
    (let _lhsOsum :: Int
         _lhsOsum =
             1
     in  ( _lhsOsum))
sem_Statement_CopyData :: String ->
                          T_Statement 
sem_Statement_CopyData insData_  =
    (let _lhsOsum :: Int
         _lhsOsum =
             1
     in  ( _lhsOsum))
sem_Statement_CreateDomain :: String ->
                              String ->
                              (Maybe Expression) ->
                              T_Statement 
sem_Statement_CreateDomain name_ typ_ check_  =
    (let _lhsOsum :: Int
         _lhsOsum =
             1
     in  ( _lhsOsum))
sem_Statement_CreateFunction :: T_Language  ->
                                String ->
                                T_ParamDefList  ->
                                T_TypeName  ->
                                String ->
                                T_FnBody  ->
                                T_Volatility  ->
                                T_Statement 
sem_Statement_CreateFunction lang_ name_ params_ rettype_ bodyQuote_ body_ vol_  =
    (let _lhsOsum :: Int
         _lhsOsum =
             1
     in  ( _lhsOsum))
sem_Statement_CreateTable :: String ->
                             T_AttributeDefList  ->
                             T_ConstraintList  ->
                             T_Statement 
sem_Statement_CreateTable name_ atts_ cons_  =
    (let _lhsOsum :: Int
         _lhsOsum =
             1
     in  ( _lhsOsum))
sem_Statement_CreateTableAs :: String ->
                               T_Statement  ->
                               T_Statement 
sem_Statement_CreateTableAs name_ expr_  =
    (let _lhsOsum :: Int
         _exprIsum :: Int
         _lhsOsum =
             1
         ( _exprIsum) =
             (expr_ )
     in  ( _lhsOsum))
sem_Statement_CreateType :: String ->
                            T_TypeAttributeDefList  ->
                            T_Statement 
sem_Statement_CreateType name_ atts_  =
    (let _lhsOsum :: Int
         _lhsOsum =
             1
     in  ( _lhsOsum))
sem_Statement_CreateView :: String ->
                            T_Statement  ->
                            T_Statement 
sem_Statement_CreateView name_ expr_  =
    (let _lhsOsum :: Int
         _exprIsum :: Int
         _lhsOsum =
             1
         ( _exprIsum) =
             (expr_ )
     in  ( _lhsOsum))
sem_Statement_Delete :: String ->
                        (Maybe Expression) ->
                        (Maybe SelectList) ->
                        T_Statement 
sem_Statement_Delete table_ whr_ returning_  =
    (let _lhsOsum :: Int
         _lhsOsum =
             1
     in  ( _lhsOsum))
sem_Statement_DropFunction :: T_IfExists  ->
                              T_StringStringListPairList  ->
                              T_Cascade  ->
                              T_Statement 
sem_Statement_DropFunction ifE_ sigs_ cascade_  =
    (let _lhsOsum :: Int
         _lhsOsum =
             1
     in  ( _lhsOsum))
sem_Statement_DropSomething :: T_DropType  ->
                               T_IfExists  ->
                               T_StringList  ->
                               T_Cascade  ->
                               T_Statement 
sem_Statement_DropSomething dropType_ ifE_ names_ cascade_  =
    (let _lhsOsum :: Int
         _lhsOsum =
             1
     in  ( _lhsOsum))
sem_Statement_Execute :: T_Expression  ->
                         T_Statement 
sem_Statement_Execute expr_  =
    (let _lhsOsum :: Int
         _lhsOsum =
             1
     in  ( _lhsOsum))
sem_Statement_ExecuteInto :: T_Expression  ->
                             T_StringList  ->
                             T_Statement 
sem_Statement_ExecuteInto expr_ targets_  =
    (let _lhsOsum :: Int
         _lhsOsum =
             1
     in  ( _lhsOsum))
sem_Statement_ForIntegerStatement :: String ->
                                     T_Expression  ->
                                     T_Expression  ->
                                     T_StatementList  ->
                                     T_Statement 
sem_Statement_ForIntegerStatement var_ from_ to_ sts_  =
    (let _lhsOsum :: Int
         _stsIsum :: Int
         _lhsOsum =
             1
         ( _stsIsum) =
             (sts_ )
     in  ( _lhsOsum))
sem_Statement_ForSelectStatement :: String ->
                                    T_Statement  ->
                                    T_StatementList  ->
                                    T_Statement 
sem_Statement_ForSelectStatement var_ sel_ sts_  =
    (let _lhsOsum :: Int
         _selIsum :: Int
         _stsIsum :: Int
         _lhsOsum =
             1
         ( _selIsum) =
             (sel_ )
         ( _stsIsum) =
             (sts_ )
     in  ( _lhsOsum))
sem_Statement_If :: T_ExpressionStatementListPairList  ->
                    T_StatementList  ->
                    T_Statement 
sem_Statement_If cases_ els_  =
    (let _lhsOsum :: Int
         _elsIsum :: Int
         _lhsOsum =
             1
         ( _elsIsum) =
             (els_ )
     in  ( _lhsOsum))
sem_Statement_Insert :: String ->
                        T_StringList  ->
                        T_Statement  ->
                        (Maybe SelectList) ->
                        T_Statement 
sem_Statement_Insert table_ targetCols_ insData_ returning_  =
    (let _lhsOsum :: Int
         _insDataIsum :: Int
         _lhsOsum =
             1
         ( _insDataIsum) =
             (insData_ )
     in  ( _lhsOsum))
sem_Statement_NullStatement :: T_Statement 
sem_Statement_NullStatement  =
    (let _lhsOsum :: Int
         _lhsOsum =
             1
     in  ( _lhsOsum))
sem_Statement_Perform :: T_Expression  ->
                         T_Statement 
sem_Statement_Perform expr_  =
    (let _lhsOsum :: Int
         _lhsOsum =
             1
     in  ( _lhsOsum))
sem_Statement_Raise :: T_RaiseType  ->
                       String ->
                       T_ExpressionList  ->
                       T_Statement 
sem_Statement_Raise level_ message_ args_  =
    (let _lhsOsum :: Int
         _lhsOsum =
             1
     in  ( _lhsOsum))
sem_Statement_Return :: (Maybe Expression) ->
                        T_Statement 
sem_Statement_Return value_  =
    (let _lhsOsum :: Int
         _lhsOsum =
             1
     in  ( _lhsOsum))
sem_Statement_ReturnNext :: T_Expression  ->
                            T_Statement 
sem_Statement_ReturnNext expr_  =
    (let _lhsOsum :: Int
         _lhsOsum =
             1
     in  ( _lhsOsum))
sem_Statement_ReturnQuery :: T_Statement  ->
                             T_Statement 
sem_Statement_ReturnQuery sel_  =
    (let _lhsOsum :: Int
         _selIsum :: Int
         _lhsOsum =
             1
         ( _selIsum) =
             (sel_ )
     in  ( _lhsOsum))
sem_Statement_Select :: T_Distinct  ->
                        T_SelectList  ->
                        (Maybe TableRef) ->
                        (Maybe Expression) ->
                        T_ExpressionList  ->
                        (Maybe Expression) ->
                        T_ExpressionList  ->
                        T_Direction  ->
                        (Maybe Expression) ->
                        (Maybe Expression) ->
                        T_Statement 
sem_Statement_Select selDistinct_ selSelectList_ selTref_ selWhere_ selGroupBy_ selHaving_ selOrderBy_ selDir_ selLimit_ selOffset_  =
    (let _lhsOsum :: Int
         _lhsOsum =
             1
     in  ( _lhsOsum))
sem_Statement_Truncate :: T_StringList  ->
                          T_RestartIdentity  ->
                          T_Cascade  ->
                          T_Statement 
sem_Statement_Truncate tables_ restartIdentity_ cascade_  =
    (let _lhsOsum :: Int
         _lhsOsum =
             1
     in  ( _lhsOsum))
sem_Statement_Update :: String ->
                        T_SetClauseList  ->
                        (Maybe Expression) ->
                        (Maybe SelectList) ->
                        T_Statement 
sem_Statement_Update table_ assigns_ whr_ returning_  =
    (let _lhsOsum :: Int
         _lhsOsum =
             1
     in  ( _lhsOsum))
sem_Statement_Values :: T_ExpressionListList  ->
                        T_Statement 
sem_Statement_Values expressionListList_  =
    (let _lhsOsum :: Int
         _lhsOsum =
             1
     in  ( _lhsOsum))
sem_Statement_WhileStatement :: T_Expression  ->
                                T_StatementList  ->
                                T_Statement 
sem_Statement_WhileStatement expr_ sts_  =
    (let _lhsOsum :: Int
         _stsIsum :: Int
         _lhsOsum =
             1
         ( _stsIsum) =
             (sts_ )
     in  ( _lhsOsum))
-- StatementList -----------------------------------------------
type StatementList  = [(Statement)]
-- cata
sem_StatementList :: StatementList  ->
                     T_StatementList 
sem_StatementList list  =
    (Prelude.foldr sem_StatementList_Cons sem_StatementList_Nil (Prelude.map sem_Statement list) )
-- semantic domain
type T_StatementList  = ( Int)
sem_StatementList_Cons :: T_Statement  ->
                          T_StatementList  ->
                          T_StatementList 
sem_StatementList_Cons hd_ tl_  =
    (let _lhsOsum :: Int
         _hdIsum :: Int
         _tlIsum :: Int
         _lhsOsum =
             _hdIsum + _tlIsum
         ( _hdIsum) =
             (hd_ )
         ( _tlIsum) =
             (tl_ )
     in  ( _lhsOsum))
sem_StatementList_Nil :: T_StatementList 
sem_StatementList_Nil  =
    (let _lhsOsum :: Int
         _lhsOsum =
             0
     in  ( _lhsOsum))
-- StringList --------------------------------------------------
type StringList  = [(String)]
-- cata
sem_StringList :: StringList  ->
                  T_StringList 
sem_StringList list  =
    (Prelude.foldr sem_StringList_Cons sem_StringList_Nil list )
-- semantic domain
type T_StringList  = ( )
sem_StringList_Cons :: String ->
                       T_StringList  ->
                       T_StringList 
sem_StringList_Cons hd_ tl_  =
    (let 
     in  ( ))
sem_StringList_Nil :: T_StringList 
sem_StringList_Nil  =
    (let 
     in  ( ))
-- StringStringListPair ----------------------------------------
type StringStringListPair  = ( (String),(StringList))
-- cata
sem_StringStringListPair :: StringStringListPair  ->
                            T_StringStringListPair 
sem_StringStringListPair ( x1,x2)  =
    (sem_StringStringListPair_Tuple x1 (sem_StringList x2 ) )
-- semantic domain
type T_StringStringListPair  = ( )
sem_StringStringListPair_Tuple :: String ->
                                  T_StringList  ->
                                  T_StringStringListPair 
sem_StringStringListPair_Tuple x1_ x2_  =
    (let 
     in  ( ))
-- StringStringListPairList ------------------------------------
type StringStringListPairList  = [(StringStringListPair)]
-- cata
sem_StringStringListPairList :: StringStringListPairList  ->
                                T_StringStringListPairList 
sem_StringStringListPairList list  =
    (Prelude.foldr sem_StringStringListPairList_Cons sem_StringStringListPairList_Nil (Prelude.map sem_StringStringListPair list) )
-- semantic domain
type T_StringStringListPairList  = ( )
sem_StringStringListPairList_Cons :: T_StringStringListPair  ->
                                     T_StringStringListPairList  ->
                                     T_StringStringListPairList 
sem_StringStringListPairList_Cons hd_ tl_  =
    (let 
     in  ( ))
sem_StringStringListPairList_Nil :: T_StringStringListPairList 
sem_StringStringListPairList_Nil  =
    (let 
     in  ( ))
-- TableRef ----------------------------------------------------
data TableRef  = JoinedTref (TableRef) (Natural) (JoinType) (TableRef) (Maybe JoinExpression) 
               | SubTref (Statement) (String) 
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
sem_TableRef (SubTref _statement _string )  =
    (sem_TableRef_SubTref (sem_Statement _statement ) _string )
sem_TableRef (Tref _string )  =
    (sem_TableRef_Tref _string )
sem_TableRef (TrefAlias _tref _alias )  =
    (sem_TableRef_TrefAlias _tref _alias )
sem_TableRef (TrefFun _expression )  =
    (sem_TableRef_TrefFun (sem_Expression _expression ) )
sem_TableRef (TrefFunAlias _expression _string )  =
    (sem_TableRef_TrefFunAlias (sem_Expression _expression ) _string )
-- semantic domain
type T_TableRef  = ( )
sem_TableRef_JoinedTref :: T_TableRef  ->
                           T_Natural  ->
                           T_JoinType  ->
                           T_TableRef  ->
                           (Maybe JoinExpression) ->
                           T_TableRef 
sem_TableRef_JoinedTref tref_ nat_ joinType_ jtref_ onExpr_  =
    (let 
     in  ( ))
sem_TableRef_SubTref :: T_Statement  ->
                        String ->
                        T_TableRef 
sem_TableRef_SubTref statement_ string_  =
    (let _statementIsum :: Int
         ( _statementIsum) =
             (statement_ )
     in  ( ))
sem_TableRef_Tref :: String ->
                     T_TableRef 
sem_TableRef_Tref string_  =
    (let 
     in  ( ))
sem_TableRef_TrefAlias :: String ->
                          String ->
                          T_TableRef 
sem_TableRef_TrefAlias tref_ alias_  =
    (let 
     in  ( ))
sem_TableRef_TrefFun :: T_Expression  ->
                        T_TableRef 
sem_TableRef_TrefFun expression_  =
    (let 
     in  ( ))
sem_TableRef_TrefFunAlias :: T_Expression  ->
                             String ->
                             T_TableRef 
sem_TableRef_TrefFunAlias expression_ string_  =
    (let 
     in  ( ))
-- TypeAttributeDef --------------------------------------------
data TypeAttributeDef  = TypeAttDef (String) (String) 
                       deriving ( Eq,Show)
-- cata
sem_TypeAttributeDef :: TypeAttributeDef  ->
                        T_TypeAttributeDef 
sem_TypeAttributeDef (TypeAttDef _name _typ )  =
    (sem_TypeAttributeDef_TypeAttDef _name _typ )
-- semantic domain
type T_TypeAttributeDef  = ( )
sem_TypeAttributeDef_TypeAttDef :: String ->
                                   String ->
                                   T_TypeAttributeDef 
sem_TypeAttributeDef_TypeAttDef name_ typ_  =
    (let 
     in  ( ))
-- TypeAttributeDefList ----------------------------------------
type TypeAttributeDefList  = [(TypeAttributeDef)]
-- cata
sem_TypeAttributeDefList :: TypeAttributeDefList  ->
                            T_TypeAttributeDefList 
sem_TypeAttributeDefList list  =
    (Prelude.foldr sem_TypeAttributeDefList_Cons sem_TypeAttributeDefList_Nil (Prelude.map sem_TypeAttributeDef list) )
-- semantic domain
type T_TypeAttributeDefList  = ( )
sem_TypeAttributeDefList_Cons :: T_TypeAttributeDef  ->
                                 T_TypeAttributeDefList  ->
                                 T_TypeAttributeDefList 
sem_TypeAttributeDefList_Cons hd_ tl_  =
    (let 
     in  ( ))
sem_TypeAttributeDefList_Nil :: T_TypeAttributeDefList 
sem_TypeAttributeDefList_Nil  =
    (let 
     in  ( ))
-- TypeName ----------------------------------------------------
data TypeName  = ArrayType (TypeName) 
               | PrecType (String) (Integer) 
               | SetOfType (TypeName) 
               | SimpleType (String) 
               deriving ( Eq,Show)
-- cata
sem_TypeName :: TypeName  ->
                T_TypeName 
sem_TypeName (ArrayType _typeName )  =
    (sem_TypeName_ArrayType (sem_TypeName _typeName ) )
sem_TypeName (PrecType _string _integer )  =
    (sem_TypeName_PrecType _string _integer )
sem_TypeName (SetOfType _typeName )  =
    (sem_TypeName_SetOfType (sem_TypeName _typeName ) )
sem_TypeName (SimpleType _string )  =
    (sem_TypeName_SimpleType _string )
-- semantic domain
type T_TypeName  = ( )
sem_TypeName_ArrayType :: T_TypeName  ->
                          T_TypeName 
sem_TypeName_ArrayType typeName_  =
    (let 
     in  ( ))
sem_TypeName_PrecType :: String ->
                         Integer ->
                         T_TypeName 
sem_TypeName_PrecType string_ integer_  =
    (let 
     in  ( ))
sem_TypeName_SetOfType :: T_TypeName  ->
                          T_TypeName 
sem_TypeName_SetOfType typeName_  =
    (let 
     in  ( ))
sem_TypeName_SimpleType :: String ->
                           T_TypeName 
sem_TypeName_SimpleType string_  =
    (let 
     in  ( ))
-- UnOp --------------------------------------------------------
data UnOp  = Abs 
           | IsNotNull 
           | IsNull 
           | Neg 
           | Not 
           | SetOf 
           deriving ( Eq,Show)
-- cata
sem_UnOp :: UnOp  ->
            T_UnOp 
sem_UnOp (Abs )  =
    (sem_UnOp_Abs )
sem_UnOp (IsNotNull )  =
    (sem_UnOp_IsNotNull )
sem_UnOp (IsNull )  =
    (sem_UnOp_IsNull )
sem_UnOp (Neg )  =
    (sem_UnOp_Neg )
sem_UnOp (Not )  =
    (sem_UnOp_Not )
sem_UnOp (SetOf )  =
    (sem_UnOp_SetOf )
-- semantic domain
type T_UnOp  = ( )
sem_UnOp_Abs :: T_UnOp 
sem_UnOp_Abs  =
    (let 
     in  ( ))
sem_UnOp_IsNotNull :: T_UnOp 
sem_UnOp_IsNotNull  =
    (let 
     in  ( ))
sem_UnOp_IsNull :: T_UnOp 
sem_UnOp_IsNull  =
    (let 
     in  ( ))
sem_UnOp_Neg :: T_UnOp 
sem_UnOp_Neg  =
    (let 
     in  ( ))
sem_UnOp_Not :: T_UnOp 
sem_UnOp_Not  =
    (let 
     in  ( ))
sem_UnOp_SetOf :: T_UnOp 
sem_UnOp_SetOf  =
    (let 
     in  ( ))
-- VarDef ------------------------------------------------------
data VarDef  = VarDef (String) (TypeName) (Maybe Expression) 
             deriving ( Eq,Show)
-- cata
sem_VarDef :: VarDef  ->
              T_VarDef 
sem_VarDef (VarDef _name _typ _value )  =
    (sem_VarDef_VarDef _name (sem_TypeName _typ ) _value )
-- semantic domain
type T_VarDef  = ( )
sem_VarDef_VarDef :: String ->
                     T_TypeName  ->
                     (Maybe Expression) ->
                     T_VarDef 
sem_VarDef_VarDef name_ typ_ value_  =
    (let 
     in  ( ))
-- VarDefList --------------------------------------------------
type VarDefList  = [(VarDef)]
-- cata
sem_VarDefList :: VarDefList  ->
                  T_VarDefList 
sem_VarDefList list  =
    (Prelude.foldr sem_VarDefList_Cons sem_VarDefList_Nil (Prelude.map sem_VarDef list) )
-- semantic domain
type T_VarDefList  = ( )
sem_VarDefList_Cons :: T_VarDef  ->
                       T_VarDefList  ->
                       T_VarDefList 
sem_VarDefList_Cons hd_ tl_  =
    (let 
     in  ( ))
sem_VarDefList_Nil :: T_VarDefList 
sem_VarDefList_Nil  =
    (let 
     in  ( ))
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
type T_Volatility  = ( )
sem_Volatility_Immutable :: T_Volatility 
sem_Volatility_Immutable  =
    (let 
     in  ( ))
sem_Volatility_Stable :: T_Volatility 
sem_Volatility_Stable  =
    (let 
     in  ( ))
sem_Volatility_Volatile :: T_Volatility 
sem_Volatility_Volatile  =
    (let 
     in  ( ))