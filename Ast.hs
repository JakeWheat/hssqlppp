

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
                         --SetOf -> "setof"
                         Abs -> "@"
                         Neg -> "-"



makeSelect :: Statement
makeSelect = Select Dupes (SelectList [SelExp (Identifier "*")] [])
                    Nothing Nothing [] Nothing [] Asc Nothing Nothing

--runAtts :: StatementList -> (Int,Int,[Message])
runAtts sts = let t = sem_StatementList sts
                  t1 = (wrap_StatementList t
                        Inh_StatementList{inLoop_Inh_StatementList=False})
              in (messages_Syn_StatementList t1)
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
                       ( ([Message]))
data Inh_AttributeDef  = Inh_AttributeDef {inLoop_Inh_AttributeDef :: Bool}
data Syn_AttributeDef  = Syn_AttributeDef {messages_Syn_AttributeDef :: [Message]}
wrap_AttributeDef :: T_AttributeDef  ->
                     Inh_AttributeDef  ->
                     Syn_AttributeDef 
wrap_AttributeDef sem (Inh_AttributeDef _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_AttributeDef _lhsOmessages ))
sem_AttributeDef_AttributeDef :: String ->
                                 T_TypeName  ->
                                 (Maybe Expression) ->
                                 T_RowConstraintList  ->
                                 T_AttributeDef 
sem_AttributeDef_AttributeDef name_ typ_ check_ cons_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _typOinLoop :: Bool
              _consOinLoop :: Bool
              _typImessages :: ([Message])
              _consImessages :: ([Message])
              _lhsOmessages =
                  _typImessages ++ _consImessages
              _typOinLoop =
                  _lhsIinLoop
              _consOinLoop =
                  _lhsIinLoop
              ( _typImessages) =
                  (typ_ _typOinLoop )
              ( _consImessages) =
                  (cons_ _consOinLoop )
          in  ( _lhsOmessages)))
-- AttributeDefList --------------------------------------------
type AttributeDefList  = [(AttributeDef)]
-- cata
sem_AttributeDefList :: AttributeDefList  ->
                        T_AttributeDefList 
sem_AttributeDefList list  =
    (Prelude.foldr sem_AttributeDefList_Cons sem_AttributeDefList_Nil (Prelude.map sem_AttributeDef list) )
-- semantic domain
type T_AttributeDefList  = Bool ->
                           ( ([Message]))
data Inh_AttributeDefList  = Inh_AttributeDefList {inLoop_Inh_AttributeDefList :: Bool}
data Syn_AttributeDefList  = Syn_AttributeDefList {messages_Syn_AttributeDefList :: [Message]}
wrap_AttributeDefList :: T_AttributeDefList  ->
                         Inh_AttributeDefList  ->
                         Syn_AttributeDefList 
wrap_AttributeDefList sem (Inh_AttributeDefList _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_AttributeDefList _lhsOmessages ))
sem_AttributeDefList_Cons :: T_AttributeDef  ->
                             T_AttributeDefList  ->
                             T_AttributeDefList 
sem_AttributeDefList_Cons hd_ tl_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _hdOinLoop :: Bool
              _tlOinLoop :: Bool
              _hdImessages :: ([Message])
              _tlImessages :: ([Message])
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _hdOinLoop =
                  _lhsIinLoop
              _tlOinLoop =
                  _lhsIinLoop
              ( _hdImessages) =
                  (hd_ _hdOinLoop )
              ( _tlImessages) =
                  (tl_ _tlOinLoop )
          in  ( _lhsOmessages)))
sem_AttributeDefList_Nil :: T_AttributeDefList 
sem_AttributeDefList_Nil  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
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
type T_BinOp  = Bool ->
                ( ([Message]))
data Inh_BinOp  = Inh_BinOp {inLoop_Inh_BinOp :: Bool}
data Syn_BinOp  = Syn_BinOp {messages_Syn_BinOp :: [Message]}
wrap_BinOp :: T_BinOp  ->
              Inh_BinOp  ->
              Syn_BinOp 
wrap_BinOp sem (Inh_BinOp _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_BinOp _lhsOmessages ))
sem_BinOp_And :: T_BinOp 
sem_BinOp_And  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_BinOp_Cast :: T_BinOp 
sem_BinOp_Cast  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_BinOp_Conc :: T_BinOp 
sem_BinOp_Conc  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_BinOp_DistBetween :: T_BinOp 
sem_BinOp_DistBetween  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_BinOp_Div :: T_BinOp 
sem_BinOp_Div  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_BinOp_Eql :: T_BinOp 
sem_BinOp_Eql  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_BinOp_Gt :: T_BinOp 
sem_BinOp_Gt  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_BinOp_Gte :: T_BinOp 
sem_BinOp_Gte  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_BinOp_Like :: T_BinOp 
sem_BinOp_Like  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_BinOp_Lt :: T_BinOp 
sem_BinOp_Lt  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_BinOp_Lte :: T_BinOp 
sem_BinOp_Lte  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_BinOp_Minus :: T_BinOp 
sem_BinOp_Minus  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_BinOp_Mod :: T_BinOp 
sem_BinOp_Mod  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_BinOp_Mult :: T_BinOp 
sem_BinOp_Mult  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_BinOp_NotEql :: T_BinOp 
sem_BinOp_NotEql  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_BinOp_Or :: T_BinOp 
sem_BinOp_Or  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_BinOp_Plus :: T_BinOp 
sem_BinOp_Plus  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_BinOp_Pow :: T_BinOp 
sem_BinOp_Pow  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
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
                  ( ([Message]))
data Inh_Cascade  = Inh_Cascade {inLoop_Inh_Cascade :: Bool}
data Syn_Cascade  = Syn_Cascade {messages_Syn_Cascade :: [Message]}
wrap_Cascade :: T_Cascade  ->
                Inh_Cascade  ->
                Syn_Cascade 
wrap_Cascade sem (Inh_Cascade _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_Cascade _lhsOmessages ))
sem_Cascade_Cascade :: T_Cascade 
sem_Cascade_Cascade  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_Cascade_Restrict :: T_Cascade 
sem_Cascade_Restrict  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
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
                      ( ([Message]))
data Inh_CombineType  = Inh_CombineType {inLoop_Inh_CombineType :: Bool}
data Syn_CombineType  = Syn_CombineType {messages_Syn_CombineType :: [Message]}
wrap_CombineType :: T_CombineType  ->
                    Inh_CombineType  ->
                    Syn_CombineType 
wrap_CombineType sem (Inh_CombineType _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_CombineType _lhsOmessages ))
sem_CombineType_Except :: T_CombineType 
sem_CombineType_Except  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_CombineType_Intersect :: T_CombineType 
sem_CombineType_Intersect  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_CombineType_Union :: T_CombineType 
sem_CombineType_Union  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_CombineType_UnionAll :: T_CombineType 
sem_CombineType_UnionAll  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
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
                     ( ([Message]))
data Inh_Constraint  = Inh_Constraint {inLoop_Inh_Constraint :: Bool}
data Syn_Constraint  = Syn_Constraint {messages_Syn_Constraint :: [Message]}
wrap_Constraint :: T_Constraint  ->
                   Inh_Constraint  ->
                   Syn_Constraint 
wrap_Constraint sem (Inh_Constraint _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_Constraint _lhsOmessages ))
sem_Constraint_CheckConstraint :: T_Expression  ->
                                  T_Constraint 
sem_Constraint_CheckConstraint expression_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _expressionOinLoop :: Bool
              _expressionImessages :: ([Message])
              _lhsOmessages =
                  _expressionImessages
              _expressionOinLoop =
                  _lhsIinLoop
              ( _expressionImessages) =
                  (expression_ _expressionOinLoop )
          in  ( _lhsOmessages)))
sem_Constraint_PrimaryKeyConstraint :: T_StringList  ->
                                       T_Constraint 
sem_Constraint_PrimaryKeyConstraint stringList_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _stringListOinLoop :: Bool
              _stringListImessages :: ([Message])
              _lhsOmessages =
                  _stringListImessages
              _stringListOinLoop =
                  _lhsIinLoop
              ( _stringListImessages) =
                  (stringList_ _stringListOinLoop )
          in  ( _lhsOmessages)))
sem_Constraint_ReferenceConstraint :: T_StringList  ->
                                      String ->
                                      T_StringList  ->
                                      T_Cascade  ->
                                      T_Cascade  ->
                                      T_Constraint 
sem_Constraint_ReferenceConstraint atts_ table_ tableAtts_ onUpdate_ onDelete_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _attsOinLoop :: Bool
              _tableAttsOinLoop :: Bool
              _onUpdateOinLoop :: Bool
              _onDeleteOinLoop :: Bool
              _attsImessages :: ([Message])
              _tableAttsImessages :: ([Message])
              _onUpdateImessages :: ([Message])
              _onDeleteImessages :: ([Message])
              _lhsOmessages =
                  _attsImessages ++ _tableAttsImessages ++ _onUpdateImessages ++ _onDeleteImessages
              _attsOinLoop =
                  _lhsIinLoop
              _tableAttsOinLoop =
                  _lhsIinLoop
              _onUpdateOinLoop =
                  _lhsIinLoop
              _onDeleteOinLoop =
                  _lhsIinLoop
              ( _attsImessages) =
                  (atts_ _attsOinLoop )
              ( _tableAttsImessages) =
                  (tableAtts_ _tableAttsOinLoop )
              ( _onUpdateImessages) =
                  (onUpdate_ _onUpdateOinLoop )
              ( _onDeleteImessages) =
                  (onDelete_ _onDeleteOinLoop )
          in  ( _lhsOmessages)))
sem_Constraint_UniqueConstraint :: T_StringList  ->
                                   T_Constraint 
sem_Constraint_UniqueConstraint stringList_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _stringListOinLoop :: Bool
              _stringListImessages :: ([Message])
              _lhsOmessages =
                  _stringListImessages
              _stringListOinLoop =
                  _lhsIinLoop
              ( _stringListImessages) =
                  (stringList_ _stringListOinLoop )
          in  ( _lhsOmessages)))
-- ConstraintList ----------------------------------------------
type ConstraintList  = [(Constraint)]
-- cata
sem_ConstraintList :: ConstraintList  ->
                      T_ConstraintList 
sem_ConstraintList list  =
    (Prelude.foldr sem_ConstraintList_Cons sem_ConstraintList_Nil (Prelude.map sem_Constraint list) )
-- semantic domain
type T_ConstraintList  = Bool ->
                         ( ([Message]))
data Inh_ConstraintList  = Inh_ConstraintList {inLoop_Inh_ConstraintList :: Bool}
data Syn_ConstraintList  = Syn_ConstraintList {messages_Syn_ConstraintList :: [Message]}
wrap_ConstraintList :: T_ConstraintList  ->
                       Inh_ConstraintList  ->
                       Syn_ConstraintList 
wrap_ConstraintList sem (Inh_ConstraintList _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_ConstraintList _lhsOmessages ))
sem_ConstraintList_Cons :: T_Constraint  ->
                           T_ConstraintList  ->
                           T_ConstraintList 
sem_ConstraintList_Cons hd_ tl_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _hdOinLoop :: Bool
              _tlOinLoop :: Bool
              _hdImessages :: ([Message])
              _tlImessages :: ([Message])
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _hdOinLoop =
                  _lhsIinLoop
              _tlOinLoop =
                  _lhsIinLoop
              ( _hdImessages) =
                  (hd_ _hdOinLoop )
              ( _tlImessages) =
                  (tl_ _tlOinLoop )
          in  ( _lhsOmessages)))
sem_ConstraintList_Nil :: T_ConstraintList 
sem_ConstraintList_Nil  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
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
                     ( ([Message]))
data Inh_CopySource  = Inh_CopySource {inLoop_Inh_CopySource :: Bool}
data Syn_CopySource  = Syn_CopySource {messages_Syn_CopySource :: [Message]}
wrap_CopySource :: T_CopySource  ->
                   Inh_CopySource  ->
                   Syn_CopySource 
wrap_CopySource sem (Inh_CopySource _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_CopySource _lhsOmessages ))
sem_CopySource_CopyFilename :: String ->
                               T_CopySource 
sem_CopySource_CopyFilename string_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_CopySource_Stdin :: T_CopySource 
sem_CopySource_Stdin  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
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
                    ( ([Message]))
data Inh_Direction  = Inh_Direction {inLoop_Inh_Direction :: Bool}
data Syn_Direction  = Syn_Direction {messages_Syn_Direction :: [Message]}
wrap_Direction :: T_Direction  ->
                  Inh_Direction  ->
                  Syn_Direction 
wrap_Direction sem (Inh_Direction _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_Direction _lhsOmessages ))
sem_Direction_Asc :: T_Direction 
sem_Direction_Asc  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_Direction_Desc :: T_Direction 
sem_Direction_Desc  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
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
                   ( ([Message]))
data Inh_Distinct  = Inh_Distinct {inLoop_Inh_Distinct :: Bool}
data Syn_Distinct  = Syn_Distinct {messages_Syn_Distinct :: [Message]}
wrap_Distinct :: T_Distinct  ->
                 Inh_Distinct  ->
                 Syn_Distinct 
wrap_Distinct sem (Inh_Distinct _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_Distinct _lhsOmessages ))
sem_Distinct_Distinct :: T_Distinct 
sem_Distinct_Distinct  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_Distinct_Dupes :: T_Distinct 
sem_Distinct_Dupes  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
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
                   ( ([Message]))
data Inh_DropType  = Inh_DropType {inLoop_Inh_DropType :: Bool}
data Syn_DropType  = Syn_DropType {messages_Syn_DropType :: [Message]}
wrap_DropType :: T_DropType  ->
                 Inh_DropType  ->
                 Syn_DropType 
wrap_DropType sem (Inh_DropType _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_DropType _lhsOmessages ))
sem_DropType_Domain :: T_DropType 
sem_DropType_Domain  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_DropType_Table :: T_DropType 
sem_DropType_Table  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_DropType_Type :: T_DropType 
sem_DropType_Type  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_DropType_View :: T_DropType 
sem_DropType_View  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
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
type T_Expression  = Bool ->
                     ( ([Message]))
data Inh_Expression  = Inh_Expression {inLoop_Inh_Expression :: Bool}
data Syn_Expression  = Syn_Expression {messages_Syn_Expression :: [Message]}
wrap_Expression :: T_Expression  ->
                   Inh_Expression  ->
                   Syn_Expression 
wrap_Expression sem (Inh_Expression _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_Expression _lhsOmessages ))
sem_Expression_ArrayLit :: T_ExpressionList  ->
                           T_Expression 
sem_Expression_ArrayLit expressionList_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _expressionListOinLoop :: Bool
              _expressionListImessages :: ([Message])
              _lhsOmessages =
                  _expressionListImessages
              _expressionListOinLoop =
                  _lhsIinLoop
              ( _expressionListImessages) =
                  (expressionList_ _expressionListOinLoop )
          in  ( _lhsOmessages)))
sem_Expression_ArraySub :: T_Expression  ->
                           T_ExpressionList  ->
                           T_Expression 
sem_Expression_ArraySub expression_ expressionList_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _expressionOinLoop :: Bool
              _expressionListOinLoop :: Bool
              _expressionImessages :: ([Message])
              _expressionListImessages :: ([Message])
              _lhsOmessages =
                  _expressionImessages ++ _expressionListImessages
              _expressionOinLoop =
                  _lhsIinLoop
              _expressionListOinLoop =
                  _lhsIinLoop
              ( _expressionImessages) =
                  (expression_ _expressionOinLoop )
              ( _expressionListImessages) =
                  (expressionList_ _expressionListOinLoop )
          in  ( _lhsOmessages)))
sem_Expression_Between :: T_Expression  ->
                          T_Expression  ->
                          T_Expression  ->
                          T_Expression 
sem_Expression_Between val_ lower_ upper_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _valOinLoop :: Bool
              _lowerOinLoop :: Bool
              _upperOinLoop :: Bool
              _valImessages :: ([Message])
              _lowerImessages :: ([Message])
              _upperImessages :: ([Message])
              _lhsOmessages =
                  _valImessages ++ _lowerImessages ++ _upperImessages
              _valOinLoop =
                  _lhsIinLoop
              _lowerOinLoop =
                  _lhsIinLoop
              _upperOinLoop =
                  _lhsIinLoop
              ( _valImessages) =
                  (val_ _valOinLoop )
              ( _lowerImessages) =
                  (lower_ _lowerOinLoop )
              ( _upperImessages) =
                  (upper_ _upperOinLoop )
          in  ( _lhsOmessages)))
sem_Expression_BinOpCall :: T_BinOp  ->
                            T_Expression  ->
                            T_Expression  ->
                            T_Expression 
sem_Expression_BinOpCall binOp_ arg1_ arg2_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _binOpOinLoop :: Bool
              _arg1OinLoop :: Bool
              _arg2OinLoop :: Bool
              _binOpImessages :: ([Message])
              _arg1Imessages :: ([Message])
              _arg2Imessages :: ([Message])
              _lhsOmessages =
                  _binOpImessages ++ _arg1Imessages ++ _arg2Imessages
              _binOpOinLoop =
                  _lhsIinLoop
              _arg1OinLoop =
                  _lhsIinLoop
              _arg2OinLoop =
                  _lhsIinLoop
              ( _binOpImessages) =
                  (binOp_ _binOpOinLoop )
              ( _arg1Imessages) =
                  (arg1_ _arg1OinLoop )
              ( _arg2Imessages) =
                  (arg2_ _arg2OinLoop )
          in  ( _lhsOmessages)))
sem_Expression_BooleanLit :: Bool ->
                             T_Expression 
sem_Expression_BooleanLit bool_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_Expression_Case :: T_ExpressionListExpressionPairList  ->
                       (Maybe Expression) ->
                       T_Expression 
sem_Expression_Case cases_ els_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _casesOinLoop :: Bool
              _casesImessages :: ([Message])
              _lhsOmessages =
                  _casesImessages
              _casesOinLoop =
                  _lhsIinLoop
              ( _casesImessages) =
                  (cases_ _casesOinLoop )
          in  ( _lhsOmessages)))
sem_Expression_CastKeyword :: T_Expression  ->
                              T_TypeName  ->
                              T_Expression 
sem_Expression_CastKeyword expression_ typeName_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _expressionOinLoop :: Bool
              _typeNameOinLoop :: Bool
              _expressionImessages :: ([Message])
              _typeNameImessages :: ([Message])
              _lhsOmessages =
                  _expressionImessages ++ _typeNameImessages
              _expressionOinLoop =
                  _lhsIinLoop
              _typeNameOinLoop =
                  _lhsIinLoop
              ( _expressionImessages) =
                  (expression_ _expressionOinLoop )
              ( _typeNameImessages) =
                  (typeName_ _typeNameOinLoop )
          in  ( _lhsOmessages)))
sem_Expression_Exists :: T_Statement  ->
                         T_Expression 
sem_Expression_Exists statement_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _statementOinLoop :: Bool
              _statementImessages :: ([Message])
              _lhsOmessages =
                  _statementImessages
              _statementOinLoop =
                  _lhsIinLoop
              ( _statementImessages) =
                  (statement_ _statementOinLoop )
          in  ( _lhsOmessages)))
sem_Expression_FloatLit :: Double ->
                           T_Expression 
sem_Expression_FloatLit double_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_Expression_FunCall :: String ->
                          T_ExpressionList  ->
                          T_Expression 
sem_Expression_FunCall string_ expressionList_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _expressionListOinLoop :: Bool
              _expressionListImessages :: ([Message])
              _lhsOmessages =
                  _expressionListImessages
              _expressionListOinLoop =
                  _lhsIinLoop
              ( _expressionListImessages) =
                  (expressionList_ _expressionListOinLoop )
          in  ( _lhsOmessages)))
sem_Expression_Identifier :: String ->
                             T_Expression 
sem_Expression_Identifier string_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_Expression_InPredicate :: T_Expression  ->
                              Bool ->
                              T_InList  ->
                              T_Expression 
sem_Expression_InPredicate expression_ bool_ inList_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _expressionOinLoop :: Bool
              _inListOinLoop :: Bool
              _expressionImessages :: ([Message])
              _inListImessages :: ([Message])
              _lhsOmessages =
                  _expressionImessages ++ _inListImessages
              _expressionOinLoop =
                  _lhsIinLoop
              _inListOinLoop =
                  _lhsIinLoop
              ( _expressionImessages) =
                  (expression_ _expressionOinLoop )
              ( _inListImessages) =
                  (inList_ _inListOinLoop )
          in  ( _lhsOmessages)))
sem_Expression_IntegerLit :: Integer ->
                             T_Expression 
sem_Expression_IntegerLit integer_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_Expression_NullLit :: T_Expression 
sem_Expression_NullLit  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_Expression_PositionalArg :: Integer ->
                                T_Expression 
sem_Expression_PositionalArg integer_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_Expression_Row :: T_ExpressionList  ->
                      T_Expression 
sem_Expression_Row expressionList_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _expressionListOinLoop :: Bool
              _expressionListImessages :: ([Message])
              _lhsOmessages =
                  _expressionListImessages
              _expressionListOinLoop =
                  _lhsIinLoop
              ( _expressionListImessages) =
                  (expressionList_ _expressionListOinLoop )
          in  ( _lhsOmessages)))
sem_Expression_ScalarSubQuery :: T_Statement  ->
                                 T_Expression 
sem_Expression_ScalarSubQuery statement_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _statementOinLoop :: Bool
              _statementImessages :: ([Message])
              _lhsOmessages =
                  _statementImessages
              _statementOinLoop =
                  _lhsIinLoop
              ( _statementImessages) =
                  (statement_ _statementOinLoop )
          in  ( _lhsOmessages)))
sem_Expression_StringLit :: String ->
                            String ->
                            T_Expression 
sem_Expression_StringLit quote_ value_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_Expression_Substring :: T_Expression  ->
                            T_Expression  ->
                            T_Expression  ->
                            T_Expression 
sem_Expression_Substring str_ from_ for_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _strOinLoop :: Bool
              _fromOinLoop :: Bool
              _forOinLoop :: Bool
              _strImessages :: ([Message])
              _fromImessages :: ([Message])
              _forImessages :: ([Message])
              _lhsOmessages =
                  _strImessages ++ _fromImessages ++ _forImessages
              _strOinLoop =
                  _lhsIinLoop
              _fromOinLoop =
                  _lhsIinLoop
              _forOinLoop =
                  _lhsIinLoop
              ( _strImessages) =
                  (str_ _strOinLoop )
              ( _fromImessages) =
                  (from_ _fromOinLoop )
              ( _forImessages) =
                  (for_ _forOinLoop )
          in  ( _lhsOmessages)))
sem_Expression_UnOpCall :: T_UnOp  ->
                           T_Expression  ->
                           T_Expression 
sem_Expression_UnOpCall unOp_ expression_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _unOpOinLoop :: Bool
              _expressionOinLoop :: Bool
              _unOpImessages :: ([Message])
              _expressionImessages :: ([Message])
              _lhsOmessages =
                  _unOpImessages ++ _expressionImessages
              _unOpOinLoop =
                  _lhsIinLoop
              _expressionOinLoop =
                  _lhsIinLoop
              ( _unOpImessages) =
                  (unOp_ _unOpOinLoop )
              ( _expressionImessages) =
                  (expression_ _expressionOinLoop )
          in  ( _lhsOmessages)))
sem_Expression_WindowFn :: T_Expression  ->
                           T_ExpressionList  ->
                           T_ExpressionList  ->
                           T_Direction  ->
                           T_Expression 
sem_Expression_WindowFn fn_ partitionBy_ orderBy_ dir_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _fnOinLoop :: Bool
              _partitionByOinLoop :: Bool
              _orderByOinLoop :: Bool
              _dirOinLoop :: Bool
              _fnImessages :: ([Message])
              _partitionByImessages :: ([Message])
              _orderByImessages :: ([Message])
              _dirImessages :: ([Message])
              _lhsOmessages =
                  _fnImessages ++ _partitionByImessages ++ _orderByImessages ++ _dirImessages
              _fnOinLoop =
                  _lhsIinLoop
              _partitionByOinLoop =
                  _lhsIinLoop
              _orderByOinLoop =
                  _lhsIinLoop
              _dirOinLoop =
                  _lhsIinLoop
              ( _fnImessages) =
                  (fn_ _fnOinLoop )
              ( _partitionByImessages) =
                  (partitionBy_ _partitionByOinLoop )
              ( _orderByImessages) =
                  (orderBy_ _orderByOinLoop )
              ( _dirImessages) =
                  (dir_ _dirOinLoop )
          in  ( _lhsOmessages)))
-- ExpressionList ----------------------------------------------
type ExpressionList  = [(Expression)]
-- cata
sem_ExpressionList :: ExpressionList  ->
                      T_ExpressionList 
sem_ExpressionList list  =
    (Prelude.foldr sem_ExpressionList_Cons sem_ExpressionList_Nil (Prelude.map sem_Expression list) )
-- semantic domain
type T_ExpressionList  = Bool ->
                         ( ([Message]))
data Inh_ExpressionList  = Inh_ExpressionList {inLoop_Inh_ExpressionList :: Bool}
data Syn_ExpressionList  = Syn_ExpressionList {messages_Syn_ExpressionList :: [Message]}
wrap_ExpressionList :: T_ExpressionList  ->
                       Inh_ExpressionList  ->
                       Syn_ExpressionList 
wrap_ExpressionList sem (Inh_ExpressionList _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_ExpressionList _lhsOmessages ))
sem_ExpressionList_Cons :: T_Expression  ->
                           T_ExpressionList  ->
                           T_ExpressionList 
sem_ExpressionList_Cons hd_ tl_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _hdOinLoop :: Bool
              _tlOinLoop :: Bool
              _hdImessages :: ([Message])
              _tlImessages :: ([Message])
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _hdOinLoop =
                  _lhsIinLoop
              _tlOinLoop =
                  _lhsIinLoop
              ( _hdImessages) =
                  (hd_ _hdOinLoop )
              ( _tlImessages) =
                  (tl_ _tlOinLoop )
          in  ( _lhsOmessages)))
sem_ExpressionList_Nil :: T_ExpressionList 
sem_ExpressionList_Nil  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
-- ExpressionListExpressionPair --------------------------------
type ExpressionListExpressionPair  = ( (ExpressionList),(Expression))
-- cata
sem_ExpressionListExpressionPair :: ExpressionListExpressionPair  ->
                                    T_ExpressionListExpressionPair 
sem_ExpressionListExpressionPair ( x1,x2)  =
    (sem_ExpressionListExpressionPair_Tuple (sem_ExpressionList x1 ) (sem_Expression x2 ) )
-- semantic domain
type T_ExpressionListExpressionPair  = Bool ->
                                       ( ([Message]))
data Inh_ExpressionListExpressionPair  = Inh_ExpressionListExpressionPair {inLoop_Inh_ExpressionListExpressionPair :: Bool}
data Syn_ExpressionListExpressionPair  = Syn_ExpressionListExpressionPair {messages_Syn_ExpressionListExpressionPair :: [Message]}
wrap_ExpressionListExpressionPair :: T_ExpressionListExpressionPair  ->
                                     Inh_ExpressionListExpressionPair  ->
                                     Syn_ExpressionListExpressionPair 
wrap_ExpressionListExpressionPair sem (Inh_ExpressionListExpressionPair _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_ExpressionListExpressionPair _lhsOmessages ))
sem_ExpressionListExpressionPair_Tuple :: T_ExpressionList  ->
                                          T_Expression  ->
                                          T_ExpressionListExpressionPair 
sem_ExpressionListExpressionPair_Tuple x1_ x2_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _x1OinLoop :: Bool
              _x2OinLoop :: Bool
              _x1Imessages :: ([Message])
              _x2Imessages :: ([Message])
              _lhsOmessages =
                  _x1Imessages ++ _x2Imessages
              _x1OinLoop =
                  _lhsIinLoop
              _x2OinLoop =
                  _lhsIinLoop
              ( _x1Imessages) =
                  (x1_ _x1OinLoop )
              ( _x2Imessages) =
                  (x2_ _x2OinLoop )
          in  ( _lhsOmessages)))
-- ExpressionListExpressionPairList ----------------------------
type ExpressionListExpressionPairList  = [(ExpressionListExpressionPair)]
-- cata
sem_ExpressionListExpressionPairList :: ExpressionListExpressionPairList  ->
                                        T_ExpressionListExpressionPairList 
sem_ExpressionListExpressionPairList list  =
    (Prelude.foldr sem_ExpressionListExpressionPairList_Cons sem_ExpressionListExpressionPairList_Nil (Prelude.map sem_ExpressionListExpressionPair list) )
-- semantic domain
type T_ExpressionListExpressionPairList  = Bool ->
                                           ( ([Message]))
data Inh_ExpressionListExpressionPairList  = Inh_ExpressionListExpressionPairList {inLoop_Inh_ExpressionListExpressionPairList :: Bool}
data Syn_ExpressionListExpressionPairList  = Syn_ExpressionListExpressionPairList {messages_Syn_ExpressionListExpressionPairList :: [Message]}
wrap_ExpressionListExpressionPairList :: T_ExpressionListExpressionPairList  ->
                                         Inh_ExpressionListExpressionPairList  ->
                                         Syn_ExpressionListExpressionPairList 
wrap_ExpressionListExpressionPairList sem (Inh_ExpressionListExpressionPairList _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_ExpressionListExpressionPairList _lhsOmessages ))
sem_ExpressionListExpressionPairList_Cons :: T_ExpressionListExpressionPair  ->
                                             T_ExpressionListExpressionPairList  ->
                                             T_ExpressionListExpressionPairList 
sem_ExpressionListExpressionPairList_Cons hd_ tl_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _hdOinLoop :: Bool
              _tlOinLoop :: Bool
              _hdImessages :: ([Message])
              _tlImessages :: ([Message])
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _hdOinLoop =
                  _lhsIinLoop
              _tlOinLoop =
                  _lhsIinLoop
              ( _hdImessages) =
                  (hd_ _hdOinLoop )
              ( _tlImessages) =
                  (tl_ _tlOinLoop )
          in  ( _lhsOmessages)))
sem_ExpressionListExpressionPairList_Nil :: T_ExpressionListExpressionPairList 
sem_ExpressionListExpressionPairList_Nil  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
-- ExpressionListList ------------------------------------------
type ExpressionListList  = [(ExpressionList)]
-- cata
sem_ExpressionListList :: ExpressionListList  ->
                          T_ExpressionListList 
sem_ExpressionListList list  =
    (Prelude.foldr sem_ExpressionListList_Cons sem_ExpressionListList_Nil (Prelude.map sem_ExpressionList list) )
-- semantic domain
type T_ExpressionListList  = Bool ->
                             ( ([Message]))
data Inh_ExpressionListList  = Inh_ExpressionListList {inLoop_Inh_ExpressionListList :: Bool}
data Syn_ExpressionListList  = Syn_ExpressionListList {messages_Syn_ExpressionListList :: [Message]}
wrap_ExpressionListList :: T_ExpressionListList  ->
                           Inh_ExpressionListList  ->
                           Syn_ExpressionListList 
wrap_ExpressionListList sem (Inh_ExpressionListList _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_ExpressionListList _lhsOmessages ))
sem_ExpressionListList_Cons :: T_ExpressionList  ->
                               T_ExpressionListList  ->
                               T_ExpressionListList 
sem_ExpressionListList_Cons hd_ tl_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _hdOinLoop :: Bool
              _tlOinLoop :: Bool
              _hdImessages :: ([Message])
              _tlImessages :: ([Message])
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _hdOinLoop =
                  _lhsIinLoop
              _tlOinLoop =
                  _lhsIinLoop
              ( _hdImessages) =
                  (hd_ _hdOinLoop )
              ( _tlImessages) =
                  (tl_ _tlOinLoop )
          in  ( _lhsOmessages)))
sem_ExpressionListList_Nil :: T_ExpressionListList 
sem_ExpressionListList_Nil  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
-- ExpressionListStatementListPair -----------------------------
type ExpressionListStatementListPair  = ( (ExpressionList),(StatementList))
-- cata
sem_ExpressionListStatementListPair :: ExpressionListStatementListPair  ->
                                       T_ExpressionListStatementListPair 
sem_ExpressionListStatementListPair ( x1,x2)  =
    (sem_ExpressionListStatementListPair_Tuple (sem_ExpressionList x1 ) (sem_StatementList x2 ) )
-- semantic domain
type T_ExpressionListStatementListPair  = Bool ->
                                          ( ([Message]))
data Inh_ExpressionListStatementListPair  = Inh_ExpressionListStatementListPair {inLoop_Inh_ExpressionListStatementListPair :: Bool}
data Syn_ExpressionListStatementListPair  = Syn_ExpressionListStatementListPair {messages_Syn_ExpressionListStatementListPair :: [Message]}
wrap_ExpressionListStatementListPair :: T_ExpressionListStatementListPair  ->
                                        Inh_ExpressionListStatementListPair  ->
                                        Syn_ExpressionListStatementListPair 
wrap_ExpressionListStatementListPair sem (Inh_ExpressionListStatementListPair _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_ExpressionListStatementListPair _lhsOmessages ))
sem_ExpressionListStatementListPair_Tuple :: T_ExpressionList  ->
                                             T_StatementList  ->
                                             T_ExpressionListStatementListPair 
sem_ExpressionListStatementListPair_Tuple x1_ x2_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _x1OinLoop :: Bool
              _x2OinLoop :: Bool
              _x1Imessages :: ([Message])
              _x2Imessages :: ([Message])
              _lhsOmessages =
                  _x1Imessages ++ _x2Imessages
              _x1OinLoop =
                  _lhsIinLoop
              _x2OinLoop =
                  _lhsIinLoop
              ( _x1Imessages) =
                  (x1_ _x1OinLoop )
              ( _x2Imessages) =
                  (x2_ _x2OinLoop )
          in  ( _lhsOmessages)))
-- ExpressionListStatementListPairList -------------------------
type ExpressionListStatementListPairList  = [(ExpressionListStatementListPair)]
-- cata
sem_ExpressionListStatementListPairList :: ExpressionListStatementListPairList  ->
                                           T_ExpressionListStatementListPairList 
sem_ExpressionListStatementListPairList list  =
    (Prelude.foldr sem_ExpressionListStatementListPairList_Cons sem_ExpressionListStatementListPairList_Nil (Prelude.map sem_ExpressionListStatementListPair list) )
-- semantic domain
type T_ExpressionListStatementListPairList  = Bool ->
                                              ( ([Message]))
data Inh_ExpressionListStatementListPairList  = Inh_ExpressionListStatementListPairList {inLoop_Inh_ExpressionListStatementListPairList :: Bool}
data Syn_ExpressionListStatementListPairList  = Syn_ExpressionListStatementListPairList {messages_Syn_ExpressionListStatementListPairList :: [Message]}
wrap_ExpressionListStatementListPairList :: T_ExpressionListStatementListPairList  ->
                                            Inh_ExpressionListStatementListPairList  ->
                                            Syn_ExpressionListStatementListPairList 
wrap_ExpressionListStatementListPairList sem (Inh_ExpressionListStatementListPairList _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_ExpressionListStatementListPairList _lhsOmessages ))
sem_ExpressionListStatementListPairList_Cons :: T_ExpressionListStatementListPair  ->
                                                T_ExpressionListStatementListPairList  ->
                                                T_ExpressionListStatementListPairList 
sem_ExpressionListStatementListPairList_Cons hd_ tl_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _hdOinLoop :: Bool
              _tlOinLoop :: Bool
              _hdImessages :: ([Message])
              _tlImessages :: ([Message])
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _hdOinLoop =
                  _lhsIinLoop
              _tlOinLoop =
                  _lhsIinLoop
              ( _hdImessages) =
                  (hd_ _hdOinLoop )
              ( _tlImessages) =
                  (tl_ _tlOinLoop )
          in  ( _lhsOmessages)))
sem_ExpressionListStatementListPairList_Nil :: T_ExpressionListStatementListPairList 
sem_ExpressionListStatementListPairList_Nil  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
-- ExpressionStatementListPair ---------------------------------
type ExpressionStatementListPair  = ( (Expression),(StatementList))
-- cata
sem_ExpressionStatementListPair :: ExpressionStatementListPair  ->
                                   T_ExpressionStatementListPair 
sem_ExpressionStatementListPair ( x1,x2)  =
    (sem_ExpressionStatementListPair_Tuple (sem_Expression x1 ) (sem_StatementList x2 ) )
-- semantic domain
type T_ExpressionStatementListPair  = Bool ->
                                      ( ([Message]))
data Inh_ExpressionStatementListPair  = Inh_ExpressionStatementListPair {inLoop_Inh_ExpressionStatementListPair :: Bool}
data Syn_ExpressionStatementListPair  = Syn_ExpressionStatementListPair {messages_Syn_ExpressionStatementListPair :: [Message]}
wrap_ExpressionStatementListPair :: T_ExpressionStatementListPair  ->
                                    Inh_ExpressionStatementListPair  ->
                                    Syn_ExpressionStatementListPair 
wrap_ExpressionStatementListPair sem (Inh_ExpressionStatementListPair _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_ExpressionStatementListPair _lhsOmessages ))
sem_ExpressionStatementListPair_Tuple :: T_Expression  ->
                                         T_StatementList  ->
                                         T_ExpressionStatementListPair 
sem_ExpressionStatementListPair_Tuple x1_ x2_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _x1OinLoop :: Bool
              _x2OinLoop :: Bool
              _x1Imessages :: ([Message])
              _x2Imessages :: ([Message])
              _lhsOmessages =
                  _x1Imessages ++ _x2Imessages
              _x1OinLoop =
                  _lhsIinLoop
              _x2OinLoop =
                  _lhsIinLoop
              ( _x1Imessages) =
                  (x1_ _x1OinLoop )
              ( _x2Imessages) =
                  (x2_ _x2OinLoop )
          in  ( _lhsOmessages)))
-- ExpressionStatementListPairList -----------------------------
type ExpressionStatementListPairList  = [(ExpressionStatementListPair)]
-- cata
sem_ExpressionStatementListPairList :: ExpressionStatementListPairList  ->
                                       T_ExpressionStatementListPairList 
sem_ExpressionStatementListPairList list  =
    (Prelude.foldr sem_ExpressionStatementListPairList_Cons sem_ExpressionStatementListPairList_Nil (Prelude.map sem_ExpressionStatementListPair list) )
-- semantic domain
type T_ExpressionStatementListPairList  = Bool ->
                                          ( ([Message]))
data Inh_ExpressionStatementListPairList  = Inh_ExpressionStatementListPairList {inLoop_Inh_ExpressionStatementListPairList :: Bool}
data Syn_ExpressionStatementListPairList  = Syn_ExpressionStatementListPairList {messages_Syn_ExpressionStatementListPairList :: [Message]}
wrap_ExpressionStatementListPairList :: T_ExpressionStatementListPairList  ->
                                        Inh_ExpressionStatementListPairList  ->
                                        Syn_ExpressionStatementListPairList 
wrap_ExpressionStatementListPairList sem (Inh_ExpressionStatementListPairList _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_ExpressionStatementListPairList _lhsOmessages ))
sem_ExpressionStatementListPairList_Cons :: T_ExpressionStatementListPair  ->
                                            T_ExpressionStatementListPairList  ->
                                            T_ExpressionStatementListPairList 
sem_ExpressionStatementListPairList_Cons hd_ tl_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _hdOinLoop :: Bool
              _tlOinLoop :: Bool
              _hdImessages :: ([Message])
              _tlImessages :: ([Message])
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _hdOinLoop =
                  _lhsIinLoop
              _tlOinLoop =
                  _lhsIinLoop
              ( _hdImessages) =
                  (hd_ _hdOinLoop )
              ( _tlImessages) =
                  (tl_ _tlOinLoop )
          in  ( _lhsOmessages)))
sem_ExpressionStatementListPairList_Nil :: T_ExpressionStatementListPairList 
sem_ExpressionStatementListPairList_Nil  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
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
                 ( ([Message]))
data Inh_FnBody  = Inh_FnBody {inLoop_Inh_FnBody :: Bool}
data Syn_FnBody  = Syn_FnBody {messages_Syn_FnBody :: [Message]}
wrap_FnBody :: T_FnBody  ->
               Inh_FnBody  ->
               Syn_FnBody 
wrap_FnBody sem (Inh_FnBody _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_FnBody _lhsOmessages ))
sem_FnBody_PlpgsqlFnBody :: T_VarDefList  ->
                            T_StatementList  ->
                            T_FnBody 
sem_FnBody_PlpgsqlFnBody varDefList_ sts_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _varDefListOinLoop :: Bool
              _stsOinLoop :: Bool
              _varDefListImessages :: ([Message])
              _stsImessages :: ([Message])
              _lhsOmessages =
                  _varDefListImessages ++ _stsImessages
              _varDefListOinLoop =
                  _lhsIinLoop
              _stsOinLoop =
                  _lhsIinLoop
              ( _varDefListImessages) =
                  (varDefList_ _varDefListOinLoop )
              ( _stsImessages) =
                  (sts_ _stsOinLoop )
          in  ( _lhsOmessages)))
sem_FnBody_SqlFnBody :: T_StatementList  ->
                        T_FnBody 
sem_FnBody_SqlFnBody sts_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _stsOinLoop :: Bool
              _stsImessages :: ([Message])
              _lhsOmessages =
                  _stsImessages
              _stsOinLoop =
                  _lhsIinLoop
              ( _stsImessages) =
                  (sts_ _stsOinLoop )
          in  ( _lhsOmessages)))
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
                   ( ([Message]))
data Inh_IfExists  = Inh_IfExists {inLoop_Inh_IfExists :: Bool}
data Syn_IfExists  = Syn_IfExists {messages_Syn_IfExists :: [Message]}
wrap_IfExists :: T_IfExists  ->
                 Inh_IfExists  ->
                 Syn_IfExists 
wrap_IfExists sem (Inh_IfExists _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_IfExists _lhsOmessages ))
sem_IfExists_IfExists :: T_IfExists 
sem_IfExists_IfExists  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_IfExists_Require :: T_IfExists 
sem_IfExists_Require  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
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
type T_InList  = Bool ->
                 ( ([Message]))
data Inh_InList  = Inh_InList {inLoop_Inh_InList :: Bool}
data Syn_InList  = Syn_InList {messages_Syn_InList :: [Message]}
wrap_InList :: T_InList  ->
               Inh_InList  ->
               Syn_InList 
wrap_InList sem (Inh_InList _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_InList _lhsOmessages ))
sem_InList_InList :: T_ExpressionList  ->
                     T_InList 
sem_InList_InList expressionList_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _expressionListOinLoop :: Bool
              _expressionListImessages :: ([Message])
              _lhsOmessages =
                  _expressionListImessages
              _expressionListOinLoop =
                  _lhsIinLoop
              ( _expressionListImessages) =
                  (expressionList_ _expressionListOinLoop )
          in  ( _lhsOmessages)))
sem_InList_InSelect :: T_Statement  ->
                       T_InList 
sem_InList_InSelect statement_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _statementOinLoop :: Bool
              _statementImessages :: ([Message])
              _lhsOmessages =
                  _statementImessages
              _statementOinLoop =
                  _lhsIinLoop
              ( _statementImessages) =
                  (statement_ _statementOinLoop )
          in  ( _lhsOmessages)))
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
                         ( ([Message]))
data Inh_JoinExpression  = Inh_JoinExpression {inLoop_Inh_JoinExpression :: Bool}
data Syn_JoinExpression  = Syn_JoinExpression {messages_Syn_JoinExpression :: [Message]}
wrap_JoinExpression :: T_JoinExpression  ->
                       Inh_JoinExpression  ->
                       Syn_JoinExpression 
wrap_JoinExpression sem (Inh_JoinExpression _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_JoinExpression _lhsOmessages ))
sem_JoinExpression_JoinOn :: T_Expression  ->
                             T_JoinExpression 
sem_JoinExpression_JoinOn expression_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _expressionOinLoop :: Bool
              _expressionImessages :: ([Message])
              _lhsOmessages =
                  _expressionImessages
              _expressionOinLoop =
                  _lhsIinLoop
              ( _expressionImessages) =
                  (expression_ _expressionOinLoop )
          in  ( _lhsOmessages)))
sem_JoinExpression_JoinUsing :: T_StringList  ->
                                T_JoinExpression 
sem_JoinExpression_JoinUsing stringList_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _stringListOinLoop :: Bool
              _stringListImessages :: ([Message])
              _lhsOmessages =
                  _stringListImessages
              _stringListOinLoop =
                  _lhsIinLoop
              ( _stringListImessages) =
                  (stringList_ _stringListOinLoop )
          in  ( _lhsOmessages)))
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
                   ( ([Message]))
data Inh_JoinType  = Inh_JoinType {inLoop_Inh_JoinType :: Bool}
data Syn_JoinType  = Syn_JoinType {messages_Syn_JoinType :: [Message]}
wrap_JoinType :: T_JoinType  ->
                 Inh_JoinType  ->
                 Syn_JoinType 
wrap_JoinType sem (Inh_JoinType _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_JoinType _lhsOmessages ))
sem_JoinType_Cross :: T_JoinType 
sem_JoinType_Cross  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_JoinType_FullOuter :: T_JoinType 
sem_JoinType_FullOuter  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_JoinType_Inner :: T_JoinType 
sem_JoinType_Inner  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_JoinType_LeftOuter :: T_JoinType 
sem_JoinType_LeftOuter  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_JoinType_RightOuter :: T_JoinType 
sem_JoinType_RightOuter  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
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
                   ( ([Message]))
data Inh_Language  = Inh_Language {inLoop_Inh_Language :: Bool}
data Syn_Language  = Syn_Language {messages_Syn_Language :: [Message]}
wrap_Language :: T_Language  ->
                 Inh_Language  ->
                 Syn_Language 
wrap_Language sem (Inh_Language _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_Language _lhsOmessages ))
sem_Language_Plpgsql :: T_Language 
sem_Language_Plpgsql  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_Language_Sql :: T_Language 
sem_Language_Sql  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
-- Message -----------------------------------------------------
data Message  = Error (String) 
              | Notice (String) 
              | Warning (String) 
              deriving ( Show)
-- cata
sem_Message :: Message  ->
               T_Message 
sem_Message (Error _string )  =
    (sem_Message_Error _string )
sem_Message (Notice _string )  =
    (sem_Message_Notice _string )
sem_Message (Warning _string )  =
    (sem_Message_Warning _string )
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
sem_Message_Error :: String ->
                     T_Message 
sem_Message_Error string_  =
    (let 
     in  ( ))
sem_Message_Notice :: String ->
                      T_Message 
sem_Message_Notice string_  =
    (let 
     in  ( ))
sem_Message_Warning :: String ->
                       T_Message 
sem_Message_Warning string_  =
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
type T_Natural  = Bool ->
                  ( ([Message]))
data Inh_Natural  = Inh_Natural {inLoop_Inh_Natural :: Bool}
data Syn_Natural  = Syn_Natural {messages_Syn_Natural :: [Message]}
wrap_Natural :: T_Natural  ->
                Inh_Natural  ->
                Syn_Natural 
wrap_Natural sem (Inh_Natural _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_Natural _lhsOmessages ))
sem_Natural_Natural :: T_Natural 
sem_Natural_Natural  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_Natural_Unnatural :: T_Natural 
sem_Natural_Unnatural  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
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
                   ( ([Message]))
data Inh_ParamDef  = Inh_ParamDef {inLoop_Inh_ParamDef :: Bool}
data Syn_ParamDef  = Syn_ParamDef {messages_Syn_ParamDef :: [Message]}
wrap_ParamDef :: T_ParamDef  ->
                 Inh_ParamDef  ->
                 Syn_ParamDef 
wrap_ParamDef sem (Inh_ParamDef _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_ParamDef _lhsOmessages ))
sem_ParamDef_ParamDef :: String ->
                         T_TypeName  ->
                         T_ParamDef 
sem_ParamDef_ParamDef string_ typeName_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _typeNameOinLoop :: Bool
              _typeNameImessages :: ([Message])
              _lhsOmessages =
                  _typeNameImessages
              _typeNameOinLoop =
                  _lhsIinLoop
              ( _typeNameImessages) =
                  (typeName_ _typeNameOinLoop )
          in  ( _lhsOmessages)))
sem_ParamDef_ParamDefTp :: T_TypeName  ->
                           T_ParamDef 
sem_ParamDef_ParamDefTp typeName_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _typeNameOinLoop :: Bool
              _typeNameImessages :: ([Message])
              _lhsOmessages =
                  _typeNameImessages
              _typeNameOinLoop =
                  _lhsIinLoop
              ( _typeNameImessages) =
                  (typeName_ _typeNameOinLoop )
          in  ( _lhsOmessages)))
-- ParamDefList ------------------------------------------------
type ParamDefList  = [(ParamDef)]
-- cata
sem_ParamDefList :: ParamDefList  ->
                    T_ParamDefList 
sem_ParamDefList list  =
    (Prelude.foldr sem_ParamDefList_Cons sem_ParamDefList_Nil (Prelude.map sem_ParamDef list) )
-- semantic domain
type T_ParamDefList  = Bool ->
                       ( ([Message]))
data Inh_ParamDefList  = Inh_ParamDefList {inLoop_Inh_ParamDefList :: Bool}
data Syn_ParamDefList  = Syn_ParamDefList {messages_Syn_ParamDefList :: [Message]}
wrap_ParamDefList :: T_ParamDefList  ->
                     Inh_ParamDefList  ->
                     Syn_ParamDefList 
wrap_ParamDefList sem (Inh_ParamDefList _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_ParamDefList _lhsOmessages ))
sem_ParamDefList_Cons :: T_ParamDef  ->
                         T_ParamDefList  ->
                         T_ParamDefList 
sem_ParamDefList_Cons hd_ tl_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _hdOinLoop :: Bool
              _tlOinLoop :: Bool
              _hdImessages :: ([Message])
              _tlImessages :: ([Message])
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _hdOinLoop =
                  _lhsIinLoop
              _tlOinLoop =
                  _lhsIinLoop
              ( _hdImessages) =
                  (hd_ _hdOinLoop )
              ( _tlImessages) =
                  (tl_ _tlOinLoop )
          in  ( _lhsOmessages)))
sem_ParamDefList_Nil :: T_ParamDefList 
sem_ParamDefList_Nil  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
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
                    ( ([Message]))
data Inh_RaiseType  = Inh_RaiseType {inLoop_Inh_RaiseType :: Bool}
data Syn_RaiseType  = Syn_RaiseType {messages_Syn_RaiseType :: [Message]}
wrap_RaiseType :: T_RaiseType  ->
                  Inh_RaiseType  ->
                  Syn_RaiseType 
wrap_RaiseType sem (Inh_RaiseType _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_RaiseType _lhsOmessages ))
sem_RaiseType_RError :: T_RaiseType 
sem_RaiseType_RError  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_RaiseType_RException :: T_RaiseType 
sem_RaiseType_RException  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_RaiseType_RNotice :: T_RaiseType 
sem_RaiseType_RNotice  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
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
                          ( ([Message]))
data Inh_RestartIdentity  = Inh_RestartIdentity {inLoop_Inh_RestartIdentity :: Bool}
data Syn_RestartIdentity  = Syn_RestartIdentity {messages_Syn_RestartIdentity :: [Message]}
wrap_RestartIdentity :: T_RestartIdentity  ->
                        Inh_RestartIdentity  ->
                        Syn_RestartIdentity 
wrap_RestartIdentity sem (Inh_RestartIdentity _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_RestartIdentity _lhsOmessages ))
sem_RestartIdentity_ContinueIdentity :: T_RestartIdentity 
sem_RestartIdentity_ContinueIdentity  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_RestartIdentity_RestartIdentity :: T_RestartIdentity 
sem_RestartIdentity_RestartIdentity  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
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
                        ( ([Message]))
data Inh_RowConstraint  = Inh_RowConstraint {inLoop_Inh_RowConstraint :: Bool}
data Syn_RowConstraint  = Syn_RowConstraint {messages_Syn_RowConstraint :: [Message]}
wrap_RowConstraint :: T_RowConstraint  ->
                      Inh_RowConstraint  ->
                      Syn_RowConstraint 
wrap_RowConstraint sem (Inh_RowConstraint _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_RowConstraint _lhsOmessages ))
sem_RowConstraint_NotNullConstraint :: T_RowConstraint 
sem_RowConstraint_NotNullConstraint  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_RowConstraint_NullConstraint :: T_RowConstraint 
sem_RowConstraint_NullConstraint  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_RowConstraint_RowCheckConstraint :: T_Expression  ->
                                        T_RowConstraint 
sem_RowConstraint_RowCheckConstraint expression_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _expressionOinLoop :: Bool
              _expressionImessages :: ([Message])
              _lhsOmessages =
                  _expressionImessages
              _expressionOinLoop =
                  _lhsIinLoop
              ( _expressionImessages) =
                  (expression_ _expressionOinLoop )
          in  ( _lhsOmessages)))
sem_RowConstraint_RowPrimaryKeyConstraint :: T_RowConstraint 
sem_RowConstraint_RowPrimaryKeyConstraint  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_RowConstraint_RowReferenceConstraint :: String ->
                                            (Maybe String) ->
                                            T_Cascade  ->
                                            T_Cascade  ->
                                            T_RowConstraint 
sem_RowConstraint_RowReferenceConstraint table_ att_ onUpdate_ onDelete_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _onUpdateOinLoop :: Bool
              _onDeleteOinLoop :: Bool
              _onUpdateImessages :: ([Message])
              _onDeleteImessages :: ([Message])
              _lhsOmessages =
                  _onUpdateImessages ++ _onDeleteImessages
              _onUpdateOinLoop =
                  _lhsIinLoop
              _onDeleteOinLoop =
                  _lhsIinLoop
              ( _onUpdateImessages) =
                  (onUpdate_ _onUpdateOinLoop )
              ( _onDeleteImessages) =
                  (onDelete_ _onDeleteOinLoop )
          in  ( _lhsOmessages)))
sem_RowConstraint_RowUniqueConstraint :: T_RowConstraint 
sem_RowConstraint_RowUniqueConstraint  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
-- RowConstraintList -------------------------------------------
type RowConstraintList  = [(RowConstraint)]
-- cata
sem_RowConstraintList :: RowConstraintList  ->
                         T_RowConstraintList 
sem_RowConstraintList list  =
    (Prelude.foldr sem_RowConstraintList_Cons sem_RowConstraintList_Nil (Prelude.map sem_RowConstraint list) )
-- semantic domain
type T_RowConstraintList  = Bool ->
                            ( ([Message]))
data Inh_RowConstraintList  = Inh_RowConstraintList {inLoop_Inh_RowConstraintList :: Bool}
data Syn_RowConstraintList  = Syn_RowConstraintList {messages_Syn_RowConstraintList :: [Message]}
wrap_RowConstraintList :: T_RowConstraintList  ->
                          Inh_RowConstraintList  ->
                          Syn_RowConstraintList 
wrap_RowConstraintList sem (Inh_RowConstraintList _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_RowConstraintList _lhsOmessages ))
sem_RowConstraintList_Cons :: T_RowConstraint  ->
                              T_RowConstraintList  ->
                              T_RowConstraintList 
sem_RowConstraintList_Cons hd_ tl_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _hdOinLoop :: Bool
              _tlOinLoop :: Bool
              _hdImessages :: ([Message])
              _tlImessages :: ([Message])
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _hdOinLoop =
                  _lhsIinLoop
              _tlOinLoop =
                  _lhsIinLoop
              ( _hdImessages) =
                  (hd_ _hdOinLoop )
              ( _tlImessages) =
                  (tl_ _tlOinLoop )
          in  ( _lhsOmessages)))
sem_RowConstraintList_Nil :: T_RowConstraintList 
sem_RowConstraintList_Nil  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
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
                     ( ([Message]))
data Inh_SelectItem  = Inh_SelectItem {inLoop_Inh_SelectItem :: Bool}
data Syn_SelectItem  = Syn_SelectItem {messages_Syn_SelectItem :: [Message]}
wrap_SelectItem :: T_SelectItem  ->
                   Inh_SelectItem  ->
                   Syn_SelectItem 
wrap_SelectItem sem (Inh_SelectItem _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_SelectItem _lhsOmessages ))
sem_SelectItem_SelExp :: T_Expression  ->
                         T_SelectItem 
sem_SelectItem_SelExp expression_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _expressionOinLoop :: Bool
              _expressionImessages :: ([Message])
              _lhsOmessages =
                  _expressionImessages
              _expressionOinLoop =
                  _lhsIinLoop
              ( _expressionImessages) =
                  (expression_ _expressionOinLoop )
          in  ( _lhsOmessages)))
sem_SelectItem_SelectItem :: T_Expression  ->
                             String ->
                             T_SelectItem 
sem_SelectItem_SelectItem expression_ string_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _expressionOinLoop :: Bool
              _expressionImessages :: ([Message])
              _lhsOmessages =
                  _expressionImessages
              _expressionOinLoop =
                  _lhsIinLoop
              ( _expressionImessages) =
                  (expression_ _expressionOinLoop )
          in  ( _lhsOmessages)))
-- SelectItemList ----------------------------------------------
type SelectItemList  = [(SelectItem)]
-- cata
sem_SelectItemList :: SelectItemList  ->
                      T_SelectItemList 
sem_SelectItemList list  =
    (Prelude.foldr sem_SelectItemList_Cons sem_SelectItemList_Nil (Prelude.map sem_SelectItem list) )
-- semantic domain
type T_SelectItemList  = Bool ->
                         ( ([Message]))
data Inh_SelectItemList  = Inh_SelectItemList {inLoop_Inh_SelectItemList :: Bool}
data Syn_SelectItemList  = Syn_SelectItemList {messages_Syn_SelectItemList :: [Message]}
wrap_SelectItemList :: T_SelectItemList  ->
                       Inh_SelectItemList  ->
                       Syn_SelectItemList 
wrap_SelectItemList sem (Inh_SelectItemList _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_SelectItemList _lhsOmessages ))
sem_SelectItemList_Cons :: T_SelectItem  ->
                           T_SelectItemList  ->
                           T_SelectItemList 
sem_SelectItemList_Cons hd_ tl_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _hdOinLoop :: Bool
              _tlOinLoop :: Bool
              _hdImessages :: ([Message])
              _tlImessages :: ([Message])
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _hdOinLoop =
                  _lhsIinLoop
              _tlOinLoop =
                  _lhsIinLoop
              ( _hdImessages) =
                  (hd_ _hdOinLoop )
              ( _tlImessages) =
                  (tl_ _tlOinLoop )
          in  ( _lhsOmessages)))
sem_SelectItemList_Nil :: T_SelectItemList 
sem_SelectItemList_Nil  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
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
                     ( ([Message]))
data Inh_SelectList  = Inh_SelectList {inLoop_Inh_SelectList :: Bool}
data Syn_SelectList  = Syn_SelectList {messages_Syn_SelectList :: [Message]}
wrap_SelectList :: T_SelectList  ->
                   Inh_SelectList  ->
                   Syn_SelectList 
wrap_SelectList sem (Inh_SelectList _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_SelectList _lhsOmessages ))
sem_SelectList_SelectList :: T_SelectItemList  ->
                             T_StringList  ->
                             T_SelectList 
sem_SelectList_SelectList selectItemList_ stringList_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _selectItemListOinLoop :: Bool
              _stringListOinLoop :: Bool
              _selectItemListImessages :: ([Message])
              _stringListImessages :: ([Message])
              _lhsOmessages =
                  _selectItemListImessages ++ _stringListImessages
              _selectItemListOinLoop =
                  _lhsIinLoop
              _stringListOinLoop =
                  _lhsIinLoop
              ( _selectItemListImessages) =
                  (selectItemList_ _selectItemListOinLoop )
              ( _stringListImessages) =
                  (stringList_ _stringListOinLoop )
          in  ( _lhsOmessages)))
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
                    ( ([Message]))
data Inh_SetClause  = Inh_SetClause {inLoop_Inh_SetClause :: Bool}
data Syn_SetClause  = Syn_SetClause {messages_Syn_SetClause :: [Message]}
wrap_SetClause :: T_SetClause  ->
                  Inh_SetClause  ->
                  Syn_SetClause 
wrap_SetClause sem (Inh_SetClause _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_SetClause _lhsOmessages ))
sem_SetClause_RowSetClause :: T_StringList  ->
                              T_ExpressionList  ->
                              T_SetClause 
sem_SetClause_RowSetClause stringList_ expressionList_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _stringListOinLoop :: Bool
              _expressionListOinLoop :: Bool
              _stringListImessages :: ([Message])
              _expressionListImessages :: ([Message])
              _lhsOmessages =
                  _stringListImessages ++ _expressionListImessages
              _stringListOinLoop =
                  _lhsIinLoop
              _expressionListOinLoop =
                  _lhsIinLoop
              ( _stringListImessages) =
                  (stringList_ _stringListOinLoop )
              ( _expressionListImessages) =
                  (expressionList_ _expressionListOinLoop )
          in  ( _lhsOmessages)))
sem_SetClause_SetClause :: String ->
                           T_Expression  ->
                           T_SetClause 
sem_SetClause_SetClause string_ expression_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _expressionOinLoop :: Bool
              _expressionImessages :: ([Message])
              _lhsOmessages =
                  _expressionImessages
              _expressionOinLoop =
                  _lhsIinLoop
              ( _expressionImessages) =
                  (expression_ _expressionOinLoop )
          in  ( _lhsOmessages)))
-- SetClauseList -----------------------------------------------
type SetClauseList  = [(SetClause)]
-- cata
sem_SetClauseList :: SetClauseList  ->
                     T_SetClauseList 
sem_SetClauseList list  =
    (Prelude.foldr sem_SetClauseList_Cons sem_SetClauseList_Nil (Prelude.map sem_SetClause list) )
-- semantic domain
type T_SetClauseList  = Bool ->
                        ( ([Message]))
data Inh_SetClauseList  = Inh_SetClauseList {inLoop_Inh_SetClauseList :: Bool}
data Syn_SetClauseList  = Syn_SetClauseList {messages_Syn_SetClauseList :: [Message]}
wrap_SetClauseList :: T_SetClauseList  ->
                      Inh_SetClauseList  ->
                      Syn_SetClauseList 
wrap_SetClauseList sem (Inh_SetClauseList _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_SetClauseList _lhsOmessages ))
sem_SetClauseList_Cons :: T_SetClause  ->
                          T_SetClauseList  ->
                          T_SetClauseList 
sem_SetClauseList_Cons hd_ tl_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _hdOinLoop :: Bool
              _tlOinLoop :: Bool
              _hdImessages :: ([Message])
              _tlImessages :: ([Message])
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _hdOinLoop =
                  _lhsIinLoop
              _tlOinLoop =
                  _lhsIinLoop
              ( _hdImessages) =
                  (hd_ _hdOinLoop )
              ( _tlImessages) =
                  (tl_ _tlOinLoop )
          in  ( _lhsOmessages)))
sem_SetClauseList_Nil :: T_SetClauseList 
sem_SetClauseList_Nil  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
-- Statement ---------------------------------------------------
data Statement  = Assignment (String) (Expression) 
                | CaseStatement (Expression) (ExpressionListStatementListPairList) (StatementList) 
                | CombineSelect (CombineType) (Statement) (Statement) 
                | ContinueStatement 
                | Copy (String) (StringList) (CopySource) 
                | CopyData (String) 
                | CreateDomain (String) (TypeName) (Maybe Expression) 
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
    (sem_Statement_CaseStatement (sem_Expression _val ) (sem_ExpressionListStatementListPairList _cases ) (sem_StatementList _els ) )
sem_Statement (CombineSelect _ctype _sel1 _sel2 )  =
    (sem_Statement_CombineSelect (sem_CombineType _ctype ) (sem_Statement _sel1 ) (sem_Statement _sel2 ) )
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
type T_Statement  = Bool ->
                    ( ([Message]))
data Inh_Statement  = Inh_Statement {inLoop_Inh_Statement :: Bool}
data Syn_Statement  = Syn_Statement {messages_Syn_Statement :: [Message]}
wrap_Statement :: T_Statement  ->
                  Inh_Statement  ->
                  Syn_Statement 
wrap_Statement sem (Inh_Statement _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_Statement _lhsOmessages ))
sem_Statement_Assignment :: String ->
                            T_Expression  ->
                            T_Statement 
sem_Statement_Assignment target_ value_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _valueOinLoop :: Bool
              _valueImessages :: ([Message])
              _lhsOmessages =
                  _valueImessages
              _valueOinLoop =
                  _lhsIinLoop
              ( _valueImessages) =
                  (value_ _valueOinLoop )
          in  ( _lhsOmessages)))
sem_Statement_CaseStatement :: T_Expression  ->
                               T_ExpressionListStatementListPairList  ->
                               T_StatementList  ->
                               T_Statement 
sem_Statement_CaseStatement val_ cases_ els_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _valOinLoop :: Bool
              _casesOinLoop :: Bool
              _elsOinLoop :: Bool
              _valImessages :: ([Message])
              _casesImessages :: ([Message])
              _elsImessages :: ([Message])
              _lhsOmessages =
                  _valImessages ++ _casesImessages ++ _elsImessages
              _valOinLoop =
                  _lhsIinLoop
              _casesOinLoop =
                  _lhsIinLoop
              _elsOinLoop =
                  _lhsIinLoop
              ( _valImessages) =
                  (val_ _valOinLoop )
              ( _casesImessages) =
                  (cases_ _casesOinLoop )
              ( _elsImessages) =
                  (els_ _elsOinLoop )
          in  ( _lhsOmessages)))
sem_Statement_CombineSelect :: T_CombineType  ->
                               T_Statement  ->
                               T_Statement  ->
                               T_Statement 
sem_Statement_CombineSelect ctype_ sel1_ sel2_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _ctypeOinLoop :: Bool
              _sel1OinLoop :: Bool
              _sel2OinLoop :: Bool
              _ctypeImessages :: ([Message])
              _sel1Imessages :: ([Message])
              _sel2Imessages :: ([Message])
              _lhsOmessages =
                  _ctypeImessages ++ _sel1Imessages ++ _sel2Imessages
              _ctypeOinLoop =
                  _lhsIinLoop
              _sel1OinLoop =
                  _lhsIinLoop
              _sel2OinLoop =
                  _lhsIinLoop
              ( _ctypeImessages) =
                  (ctype_ _ctypeOinLoop )
              ( _sel1Imessages) =
                  (sel1_ _sel1OinLoop )
              ( _sel2Imessages) =
                  (sel2_ _sel2OinLoop )
          in  ( _lhsOmessages)))
sem_Statement_ContinueStatement :: T_Statement 
sem_Statement_ContinueStatement  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  if not _lhsIinLoop
                    then [Error ("continue not in loop")]
                    else []
          in  ( _lhsOmessages)))
sem_Statement_Copy :: String ->
                      T_StringList  ->
                      T_CopySource  ->
                      T_Statement 
sem_Statement_Copy table_ targetCols_ source_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _targetColsOinLoop :: Bool
              _sourceOinLoop :: Bool
              _targetColsImessages :: ([Message])
              _sourceImessages :: ([Message])
              _lhsOmessages =
                  _targetColsImessages ++ _sourceImessages
              _targetColsOinLoop =
                  _lhsIinLoop
              _sourceOinLoop =
                  _lhsIinLoop
              ( _targetColsImessages) =
                  (targetCols_ _targetColsOinLoop )
              ( _sourceImessages) =
                  (source_ _sourceOinLoop )
          in  ( _lhsOmessages)))
sem_Statement_CopyData :: String ->
                          T_Statement 
sem_Statement_CopyData insData_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_Statement_CreateDomain :: String ->
                              T_TypeName  ->
                              (Maybe Expression) ->
                              T_Statement 
sem_Statement_CreateDomain name_ typ_ check_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _typOinLoop :: Bool
              _typImessages :: ([Message])
              _lhsOmessages =
                  _typImessages
              _typOinLoop =
                  _lhsIinLoop
              ( _typImessages) =
                  (typ_ _typOinLoop )
          in  ( _lhsOmessages)))
sem_Statement_CreateFunction :: T_Language  ->
                                String ->
                                T_ParamDefList  ->
                                T_TypeName  ->
                                String ->
                                T_FnBody  ->
                                T_Volatility  ->
                                T_Statement 
sem_Statement_CreateFunction lang_ name_ params_ rettype_ bodyQuote_ body_ vol_  =
    (\ _lhsIinLoop ->
         (let _bodyOinLoop :: Bool
              _lhsOmessages :: ([Message])
              _langOinLoop :: Bool
              _paramsOinLoop :: Bool
              _rettypeOinLoop :: Bool
              _volOinLoop :: Bool
              _langImessages :: ([Message])
              _paramsImessages :: ([Message])
              _rettypeImessages :: ([Message])
              _bodyImessages :: ([Message])
              _volImessages :: ([Message])
              _bodyOinLoop =
                  False
              _lhsOmessages =
                  _langImessages ++ _paramsImessages ++ _rettypeImessages ++ _bodyImessages ++ _volImessages
              _langOinLoop =
                  _lhsIinLoop
              _paramsOinLoop =
                  _lhsIinLoop
              _rettypeOinLoop =
                  _lhsIinLoop
              _volOinLoop =
                  _lhsIinLoop
              ( _langImessages) =
                  (lang_ _langOinLoop )
              ( _paramsImessages) =
                  (params_ _paramsOinLoop )
              ( _rettypeImessages) =
                  (rettype_ _rettypeOinLoop )
              ( _bodyImessages) =
                  (body_ _bodyOinLoop )
              ( _volImessages) =
                  (vol_ _volOinLoop )
          in  ( _lhsOmessages)))
sem_Statement_CreateTable :: String ->
                             T_AttributeDefList  ->
                             T_ConstraintList  ->
                             T_Statement 
sem_Statement_CreateTable name_ atts_ cons_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _attsOinLoop :: Bool
              _consOinLoop :: Bool
              _attsImessages :: ([Message])
              _consImessages :: ([Message])
              _lhsOmessages =
                  _attsImessages ++ _consImessages
              _attsOinLoop =
                  _lhsIinLoop
              _consOinLoop =
                  _lhsIinLoop
              ( _attsImessages) =
                  (atts_ _attsOinLoop )
              ( _consImessages) =
                  (cons_ _consOinLoop )
          in  ( _lhsOmessages)))
sem_Statement_CreateTableAs :: String ->
                               T_Statement  ->
                               T_Statement 
sem_Statement_CreateTableAs name_ expr_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _exprOinLoop :: Bool
              _exprImessages :: ([Message])
              _lhsOmessages =
                  _exprImessages
              _exprOinLoop =
                  _lhsIinLoop
              ( _exprImessages) =
                  (expr_ _exprOinLoop )
          in  ( _lhsOmessages)))
sem_Statement_CreateType :: String ->
                            T_TypeAttributeDefList  ->
                            T_Statement 
sem_Statement_CreateType name_ atts_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _attsOinLoop :: Bool
              _attsImessages :: ([Message])
              _lhsOmessages =
                  _attsImessages
              _attsOinLoop =
                  _lhsIinLoop
              ( _attsImessages) =
                  (atts_ _attsOinLoop )
          in  ( _lhsOmessages)))
sem_Statement_CreateView :: String ->
                            T_Statement  ->
                            T_Statement 
sem_Statement_CreateView name_ expr_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _exprOinLoop :: Bool
              _exprImessages :: ([Message])
              _lhsOmessages =
                  _exprImessages
              _exprOinLoop =
                  _lhsIinLoop
              ( _exprImessages) =
                  (expr_ _exprOinLoop )
          in  ( _lhsOmessages)))
sem_Statement_Delete :: String ->
                        (Maybe Expression) ->
                        (Maybe SelectList) ->
                        T_Statement 
sem_Statement_Delete table_ whr_ returning_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_Statement_DropFunction :: T_IfExists  ->
                              T_StringStringListPairList  ->
                              T_Cascade  ->
                              T_Statement 
sem_Statement_DropFunction ifE_ sigs_ cascade_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _ifEOinLoop :: Bool
              _sigsOinLoop :: Bool
              _cascadeOinLoop :: Bool
              _ifEImessages :: ([Message])
              _sigsImessages :: ([Message])
              _cascadeImessages :: ([Message])
              _lhsOmessages =
                  _ifEImessages ++ _sigsImessages ++ _cascadeImessages
              _ifEOinLoop =
                  _lhsIinLoop
              _sigsOinLoop =
                  _lhsIinLoop
              _cascadeOinLoop =
                  _lhsIinLoop
              ( _ifEImessages) =
                  (ifE_ _ifEOinLoop )
              ( _sigsImessages) =
                  (sigs_ _sigsOinLoop )
              ( _cascadeImessages) =
                  (cascade_ _cascadeOinLoop )
          in  ( _lhsOmessages)))
sem_Statement_DropSomething :: T_DropType  ->
                               T_IfExists  ->
                               T_StringList  ->
                               T_Cascade  ->
                               T_Statement 
sem_Statement_DropSomething dropType_ ifE_ names_ cascade_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _dropTypeOinLoop :: Bool
              _ifEOinLoop :: Bool
              _namesOinLoop :: Bool
              _cascadeOinLoop :: Bool
              _dropTypeImessages :: ([Message])
              _ifEImessages :: ([Message])
              _namesImessages :: ([Message])
              _cascadeImessages :: ([Message])
              _lhsOmessages =
                  _dropTypeImessages ++ _ifEImessages ++ _namesImessages ++ _cascadeImessages
              _dropTypeOinLoop =
                  _lhsIinLoop
              _ifEOinLoop =
                  _lhsIinLoop
              _namesOinLoop =
                  _lhsIinLoop
              _cascadeOinLoop =
                  _lhsIinLoop
              ( _dropTypeImessages) =
                  (dropType_ _dropTypeOinLoop )
              ( _ifEImessages) =
                  (ifE_ _ifEOinLoop )
              ( _namesImessages) =
                  (names_ _namesOinLoop )
              ( _cascadeImessages) =
                  (cascade_ _cascadeOinLoop )
          in  ( _lhsOmessages)))
sem_Statement_Execute :: T_Expression  ->
                         T_Statement 
sem_Statement_Execute expr_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _exprOinLoop :: Bool
              _exprImessages :: ([Message])
              _lhsOmessages =
                  _exprImessages
              _exprOinLoop =
                  _lhsIinLoop
              ( _exprImessages) =
                  (expr_ _exprOinLoop )
          in  ( _lhsOmessages)))
sem_Statement_ExecuteInto :: T_Expression  ->
                             T_StringList  ->
                             T_Statement 
sem_Statement_ExecuteInto expr_ targets_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _exprOinLoop :: Bool
              _targetsOinLoop :: Bool
              _exprImessages :: ([Message])
              _targetsImessages :: ([Message])
              _lhsOmessages =
                  _exprImessages ++ _targetsImessages
              _exprOinLoop =
                  _lhsIinLoop
              _targetsOinLoop =
                  _lhsIinLoop
              ( _exprImessages) =
                  (expr_ _exprOinLoop )
              ( _targetsImessages) =
                  (targets_ _targetsOinLoop )
          in  ( _lhsOmessages)))
sem_Statement_ForIntegerStatement :: String ->
                                     T_Expression  ->
                                     T_Expression  ->
                                     T_StatementList  ->
                                     T_Statement 
sem_Statement_ForIntegerStatement var_ from_ to_ sts_  =
    (\ _lhsIinLoop ->
         (let _stsOinLoop :: Bool
              _lhsOmessages :: ([Message])
              _fromOinLoop :: Bool
              _toOinLoop :: Bool
              _fromImessages :: ([Message])
              _toImessages :: ([Message])
              _stsImessages :: ([Message])
              _stsOinLoop =
                  True
              _lhsOmessages =
                  _fromImessages ++ _toImessages ++ _stsImessages
              _fromOinLoop =
                  _lhsIinLoop
              _toOinLoop =
                  _lhsIinLoop
              ( _fromImessages) =
                  (from_ _fromOinLoop )
              ( _toImessages) =
                  (to_ _toOinLoop )
              ( _stsImessages) =
                  (sts_ _stsOinLoop )
          in  ( _lhsOmessages)))
sem_Statement_ForSelectStatement :: String ->
                                    T_Statement  ->
                                    T_StatementList  ->
                                    T_Statement 
sem_Statement_ForSelectStatement var_ sel_ sts_  =
    (\ _lhsIinLoop ->
         (let _stsOinLoop :: Bool
              _lhsOmessages :: ([Message])
              _selOinLoop :: Bool
              _selImessages :: ([Message])
              _stsImessages :: ([Message])
              _stsOinLoop =
                  True
              _lhsOmessages =
                  _selImessages ++ _stsImessages
              _selOinLoop =
                  _lhsIinLoop
              ( _selImessages) =
                  (sel_ _selOinLoop )
              ( _stsImessages) =
                  (sts_ _stsOinLoop )
          in  ( _lhsOmessages)))
sem_Statement_If :: T_ExpressionStatementListPairList  ->
                    T_StatementList  ->
                    T_Statement 
sem_Statement_If cases_ els_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _casesOinLoop :: Bool
              _elsOinLoop :: Bool
              _casesImessages :: ([Message])
              _elsImessages :: ([Message])
              _lhsOmessages =
                  _casesImessages ++ _elsImessages
              _casesOinLoop =
                  _lhsIinLoop
              _elsOinLoop =
                  _lhsIinLoop
              ( _casesImessages) =
                  (cases_ _casesOinLoop )
              ( _elsImessages) =
                  (els_ _elsOinLoop )
          in  ( _lhsOmessages)))
sem_Statement_Insert :: String ->
                        T_StringList  ->
                        T_Statement  ->
                        (Maybe SelectList) ->
                        T_Statement 
sem_Statement_Insert table_ targetCols_ insData_ returning_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _targetColsOinLoop :: Bool
              _insDataOinLoop :: Bool
              _targetColsImessages :: ([Message])
              _insDataImessages :: ([Message])
              _lhsOmessages =
                  _targetColsImessages ++ _insDataImessages
              _targetColsOinLoop =
                  _lhsIinLoop
              _insDataOinLoop =
                  _lhsIinLoop
              ( _targetColsImessages) =
                  (targetCols_ _targetColsOinLoop )
              ( _insDataImessages) =
                  (insData_ _insDataOinLoop )
          in  ( _lhsOmessages)))
sem_Statement_NullStatement :: T_Statement 
sem_Statement_NullStatement  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_Statement_Perform :: T_Expression  ->
                         T_Statement 
sem_Statement_Perform expr_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _exprOinLoop :: Bool
              _exprImessages :: ([Message])
              _lhsOmessages =
                  _exprImessages
              _exprOinLoop =
                  _lhsIinLoop
              ( _exprImessages) =
                  (expr_ _exprOinLoop )
          in  ( _lhsOmessages)))
sem_Statement_Raise :: T_RaiseType  ->
                       String ->
                       T_ExpressionList  ->
                       T_Statement 
sem_Statement_Raise level_ message_ args_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _levelOinLoop :: Bool
              _argsOinLoop :: Bool
              _levelImessages :: ([Message])
              _argsImessages :: ([Message])
              _lhsOmessages =
                  _levelImessages ++ _argsImessages
              _levelOinLoop =
                  _lhsIinLoop
              _argsOinLoop =
                  _lhsIinLoop
              ( _levelImessages) =
                  (level_ _levelOinLoop )
              ( _argsImessages) =
                  (args_ _argsOinLoop )
          in  ( _lhsOmessages)))
sem_Statement_Return :: (Maybe Expression) ->
                        T_Statement 
sem_Statement_Return value_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_Statement_ReturnNext :: T_Expression  ->
                            T_Statement 
sem_Statement_ReturnNext expr_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _exprOinLoop :: Bool
              _exprImessages :: ([Message])
              _lhsOmessages =
                  _exprImessages
              _exprOinLoop =
                  _lhsIinLoop
              ( _exprImessages) =
                  (expr_ _exprOinLoop )
          in  ( _lhsOmessages)))
sem_Statement_ReturnQuery :: T_Statement  ->
                             T_Statement 
sem_Statement_ReturnQuery sel_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _selOinLoop :: Bool
              _selImessages :: ([Message])
              _lhsOmessages =
                  _selImessages
              _selOinLoop =
                  _lhsIinLoop
              ( _selImessages) =
                  (sel_ _selOinLoop )
          in  ( _lhsOmessages)))
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
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _selDistinctOinLoop :: Bool
              _selSelectListOinLoop :: Bool
              _selGroupByOinLoop :: Bool
              _selOrderByOinLoop :: Bool
              _selDirOinLoop :: Bool
              _selDistinctImessages :: ([Message])
              _selSelectListImessages :: ([Message])
              _selGroupByImessages :: ([Message])
              _selOrderByImessages :: ([Message])
              _selDirImessages :: ([Message])
              _lhsOmessages =
                  _selDistinctImessages ++ _selSelectListImessages ++ _selGroupByImessages ++ _selOrderByImessages ++ _selDirImessages
              _selDistinctOinLoop =
                  _lhsIinLoop
              _selSelectListOinLoop =
                  _lhsIinLoop
              _selGroupByOinLoop =
                  _lhsIinLoop
              _selOrderByOinLoop =
                  _lhsIinLoop
              _selDirOinLoop =
                  _lhsIinLoop
              ( _selDistinctImessages) =
                  (selDistinct_ _selDistinctOinLoop )
              ( _selSelectListImessages) =
                  (selSelectList_ _selSelectListOinLoop )
              ( _selGroupByImessages) =
                  (selGroupBy_ _selGroupByOinLoop )
              ( _selOrderByImessages) =
                  (selOrderBy_ _selOrderByOinLoop )
              ( _selDirImessages) =
                  (selDir_ _selDirOinLoop )
          in  ( _lhsOmessages)))
sem_Statement_Truncate :: T_StringList  ->
                          T_RestartIdentity  ->
                          T_Cascade  ->
                          T_Statement 
sem_Statement_Truncate tables_ restartIdentity_ cascade_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _tablesOinLoop :: Bool
              _restartIdentityOinLoop :: Bool
              _cascadeOinLoop :: Bool
              _tablesImessages :: ([Message])
              _restartIdentityImessages :: ([Message])
              _cascadeImessages :: ([Message])
              _lhsOmessages =
                  _tablesImessages ++ _restartIdentityImessages ++ _cascadeImessages
              _tablesOinLoop =
                  _lhsIinLoop
              _restartIdentityOinLoop =
                  _lhsIinLoop
              _cascadeOinLoop =
                  _lhsIinLoop
              ( _tablesImessages) =
                  (tables_ _tablesOinLoop )
              ( _restartIdentityImessages) =
                  (restartIdentity_ _restartIdentityOinLoop )
              ( _cascadeImessages) =
                  (cascade_ _cascadeOinLoop )
          in  ( _lhsOmessages)))
sem_Statement_Update :: String ->
                        T_SetClauseList  ->
                        (Maybe Expression) ->
                        (Maybe SelectList) ->
                        T_Statement 
sem_Statement_Update table_ assigns_ whr_ returning_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _assignsOinLoop :: Bool
              _assignsImessages :: ([Message])
              _lhsOmessages =
                  _assignsImessages
              _assignsOinLoop =
                  _lhsIinLoop
              ( _assignsImessages) =
                  (assigns_ _assignsOinLoop )
          in  ( _lhsOmessages)))
sem_Statement_Values :: T_ExpressionListList  ->
                        T_Statement 
sem_Statement_Values expressionListList_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _expressionListListOinLoop :: Bool
              _expressionListListImessages :: ([Message])
              _lhsOmessages =
                  _expressionListListImessages
              _expressionListListOinLoop =
                  _lhsIinLoop
              ( _expressionListListImessages) =
                  (expressionListList_ _expressionListListOinLoop )
          in  ( _lhsOmessages)))
sem_Statement_WhileStatement :: T_Expression  ->
                                T_StatementList  ->
                                T_Statement 
sem_Statement_WhileStatement expr_ sts_  =
    (\ _lhsIinLoop ->
         (let _stsOinLoop :: Bool
              _lhsOmessages :: ([Message])
              _exprOinLoop :: Bool
              _exprImessages :: ([Message])
              _stsImessages :: ([Message])
              _stsOinLoop =
                  True
              _lhsOmessages =
                  _exprImessages ++ _stsImessages
              _exprOinLoop =
                  _lhsIinLoop
              ( _exprImessages) =
                  (expr_ _exprOinLoop )
              ( _stsImessages) =
                  (sts_ _stsOinLoop )
          in  ( _lhsOmessages)))
-- StatementList -----------------------------------------------
type StatementList  = [(Statement)]
-- cata
sem_StatementList :: StatementList  ->
                     T_StatementList 
sem_StatementList list  =
    (Prelude.foldr sem_StatementList_Cons sem_StatementList_Nil (Prelude.map sem_Statement list) )
-- semantic domain
type T_StatementList  = Bool ->
                        ( ([Message]))
data Inh_StatementList  = Inh_StatementList {inLoop_Inh_StatementList :: Bool}
data Syn_StatementList  = Syn_StatementList {messages_Syn_StatementList :: [Message]}
wrap_StatementList :: T_StatementList  ->
                      Inh_StatementList  ->
                      Syn_StatementList 
wrap_StatementList sem (Inh_StatementList _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_StatementList _lhsOmessages ))
sem_StatementList_Cons :: T_Statement  ->
                          T_StatementList  ->
                          T_StatementList 
sem_StatementList_Cons hd_ tl_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _hdOinLoop :: Bool
              _tlOinLoop :: Bool
              _hdImessages :: ([Message])
              _tlImessages :: ([Message])
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _hdOinLoop =
                  _lhsIinLoop
              _tlOinLoop =
                  _lhsIinLoop
              ( _hdImessages) =
                  (hd_ _hdOinLoop )
              ( _tlImessages) =
                  (tl_ _tlOinLoop )
          in  ( _lhsOmessages)))
sem_StatementList_Nil :: T_StatementList 
sem_StatementList_Nil  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
-- StringList --------------------------------------------------
type StringList  = [(String)]
-- cata
sem_StringList :: StringList  ->
                  T_StringList 
sem_StringList list  =
    (Prelude.foldr sem_StringList_Cons sem_StringList_Nil list )
-- semantic domain
type T_StringList  = Bool ->
                     ( ([Message]))
data Inh_StringList  = Inh_StringList {inLoop_Inh_StringList :: Bool}
data Syn_StringList  = Syn_StringList {messages_Syn_StringList :: [Message]}
wrap_StringList :: T_StringList  ->
                   Inh_StringList  ->
                   Syn_StringList 
wrap_StringList sem (Inh_StringList _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_StringList _lhsOmessages ))
sem_StringList_Cons :: String ->
                       T_StringList  ->
                       T_StringList 
sem_StringList_Cons hd_ tl_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _tlOinLoop :: Bool
              _tlImessages :: ([Message])
              _lhsOmessages =
                  _tlImessages
              _tlOinLoop =
                  _lhsIinLoop
              ( _tlImessages) =
                  (tl_ _tlOinLoop )
          in  ( _lhsOmessages)))
sem_StringList_Nil :: T_StringList 
sem_StringList_Nil  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
-- StringStringListPair ----------------------------------------
type StringStringListPair  = ( (String),(StringList))
-- cata
sem_StringStringListPair :: StringStringListPair  ->
                            T_StringStringListPair 
sem_StringStringListPair ( x1,x2)  =
    (sem_StringStringListPair_Tuple x1 (sem_StringList x2 ) )
-- semantic domain
type T_StringStringListPair  = Bool ->
                               ( ([Message]))
data Inh_StringStringListPair  = Inh_StringStringListPair {inLoop_Inh_StringStringListPair :: Bool}
data Syn_StringStringListPair  = Syn_StringStringListPair {messages_Syn_StringStringListPair :: [Message]}
wrap_StringStringListPair :: T_StringStringListPair  ->
                             Inh_StringStringListPair  ->
                             Syn_StringStringListPair 
wrap_StringStringListPair sem (Inh_StringStringListPair _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_StringStringListPair _lhsOmessages ))
sem_StringStringListPair_Tuple :: String ->
                                  T_StringList  ->
                                  T_StringStringListPair 
sem_StringStringListPair_Tuple x1_ x2_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _x2OinLoop :: Bool
              _x2Imessages :: ([Message])
              _lhsOmessages =
                  _x2Imessages
              _x2OinLoop =
                  _lhsIinLoop
              ( _x2Imessages) =
                  (x2_ _x2OinLoop )
          in  ( _lhsOmessages)))
-- StringStringListPairList ------------------------------------
type StringStringListPairList  = [(StringStringListPair)]
-- cata
sem_StringStringListPairList :: StringStringListPairList  ->
                                T_StringStringListPairList 
sem_StringStringListPairList list  =
    (Prelude.foldr sem_StringStringListPairList_Cons sem_StringStringListPairList_Nil (Prelude.map sem_StringStringListPair list) )
-- semantic domain
type T_StringStringListPairList  = Bool ->
                                   ( ([Message]))
data Inh_StringStringListPairList  = Inh_StringStringListPairList {inLoop_Inh_StringStringListPairList :: Bool}
data Syn_StringStringListPairList  = Syn_StringStringListPairList {messages_Syn_StringStringListPairList :: [Message]}
wrap_StringStringListPairList :: T_StringStringListPairList  ->
                                 Inh_StringStringListPairList  ->
                                 Syn_StringStringListPairList 
wrap_StringStringListPairList sem (Inh_StringStringListPairList _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_StringStringListPairList _lhsOmessages ))
sem_StringStringListPairList_Cons :: T_StringStringListPair  ->
                                     T_StringStringListPairList  ->
                                     T_StringStringListPairList 
sem_StringStringListPairList_Cons hd_ tl_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _hdOinLoop :: Bool
              _tlOinLoop :: Bool
              _hdImessages :: ([Message])
              _tlImessages :: ([Message])
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _hdOinLoop =
                  _lhsIinLoop
              _tlOinLoop =
                  _lhsIinLoop
              ( _hdImessages) =
                  (hd_ _hdOinLoop )
              ( _tlImessages) =
                  (tl_ _tlOinLoop )
          in  ( _lhsOmessages)))
sem_StringStringListPairList_Nil :: T_StringStringListPairList 
sem_StringStringListPairList_Nil  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
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
type T_TableRef  = Bool ->
                   ( ([Message]))
data Inh_TableRef  = Inh_TableRef {inLoop_Inh_TableRef :: Bool}
data Syn_TableRef  = Syn_TableRef {messages_Syn_TableRef :: [Message]}
wrap_TableRef :: T_TableRef  ->
                 Inh_TableRef  ->
                 Syn_TableRef 
wrap_TableRef sem (Inh_TableRef _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_TableRef _lhsOmessages ))
sem_TableRef_JoinedTref :: T_TableRef  ->
                           T_Natural  ->
                           T_JoinType  ->
                           T_TableRef  ->
                           (Maybe JoinExpression) ->
                           T_TableRef 
sem_TableRef_JoinedTref tref_ nat_ joinType_ jtref_ onExpr_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _trefOinLoop :: Bool
              _natOinLoop :: Bool
              _joinTypeOinLoop :: Bool
              _jtrefOinLoop :: Bool
              _trefImessages :: ([Message])
              _natImessages :: ([Message])
              _joinTypeImessages :: ([Message])
              _jtrefImessages :: ([Message])
              _lhsOmessages =
                  _trefImessages ++ _natImessages ++ _joinTypeImessages ++ _jtrefImessages
              _trefOinLoop =
                  _lhsIinLoop
              _natOinLoop =
                  _lhsIinLoop
              _joinTypeOinLoop =
                  _lhsIinLoop
              _jtrefOinLoop =
                  _lhsIinLoop
              ( _trefImessages) =
                  (tref_ _trefOinLoop )
              ( _natImessages) =
                  (nat_ _natOinLoop )
              ( _joinTypeImessages) =
                  (joinType_ _joinTypeOinLoop )
              ( _jtrefImessages) =
                  (jtref_ _jtrefOinLoop )
          in  ( _lhsOmessages)))
sem_TableRef_SubTref :: T_Statement  ->
                        String ->
                        T_TableRef 
sem_TableRef_SubTref statement_ string_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _statementOinLoop :: Bool
              _statementImessages :: ([Message])
              _lhsOmessages =
                  _statementImessages
              _statementOinLoop =
                  _lhsIinLoop
              ( _statementImessages) =
                  (statement_ _statementOinLoop )
          in  ( _lhsOmessages)))
sem_TableRef_Tref :: String ->
                     T_TableRef 
sem_TableRef_Tref string_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_TableRef_TrefAlias :: String ->
                          String ->
                          T_TableRef 
sem_TableRef_TrefAlias tref_ alias_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_TableRef_TrefFun :: T_Expression  ->
                        T_TableRef 
sem_TableRef_TrefFun expression_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _expressionOinLoop :: Bool
              _expressionImessages :: ([Message])
              _lhsOmessages =
                  _expressionImessages
              _expressionOinLoop =
                  _lhsIinLoop
              ( _expressionImessages) =
                  (expression_ _expressionOinLoop )
          in  ( _lhsOmessages)))
sem_TableRef_TrefFunAlias :: T_Expression  ->
                             String ->
                             T_TableRef 
sem_TableRef_TrefFunAlias expression_ string_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _expressionOinLoop :: Bool
              _expressionImessages :: ([Message])
              _lhsOmessages =
                  _expressionImessages
              _expressionOinLoop =
                  _lhsIinLoop
              ( _expressionImessages) =
                  (expression_ _expressionOinLoop )
          in  ( _lhsOmessages)))
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
                           ( ([Message]))
data Inh_TypeAttributeDef  = Inh_TypeAttributeDef {inLoop_Inh_TypeAttributeDef :: Bool}
data Syn_TypeAttributeDef  = Syn_TypeAttributeDef {messages_Syn_TypeAttributeDef :: [Message]}
wrap_TypeAttributeDef :: T_TypeAttributeDef  ->
                         Inh_TypeAttributeDef  ->
                         Syn_TypeAttributeDef 
wrap_TypeAttributeDef sem (Inh_TypeAttributeDef _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_TypeAttributeDef _lhsOmessages ))
sem_TypeAttributeDef_TypeAttDef :: String ->
                                   T_TypeName  ->
                                   T_TypeAttributeDef 
sem_TypeAttributeDef_TypeAttDef name_ typ_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _typOinLoop :: Bool
              _typImessages :: ([Message])
              _lhsOmessages =
                  _typImessages
              _typOinLoop =
                  _lhsIinLoop
              ( _typImessages) =
                  (typ_ _typOinLoop )
          in  ( _lhsOmessages)))
-- TypeAttributeDefList ----------------------------------------
type TypeAttributeDefList  = [(TypeAttributeDef)]
-- cata
sem_TypeAttributeDefList :: TypeAttributeDefList  ->
                            T_TypeAttributeDefList 
sem_TypeAttributeDefList list  =
    (Prelude.foldr sem_TypeAttributeDefList_Cons sem_TypeAttributeDefList_Nil (Prelude.map sem_TypeAttributeDef list) )
-- semantic domain
type T_TypeAttributeDefList  = Bool ->
                               ( ([Message]))
data Inh_TypeAttributeDefList  = Inh_TypeAttributeDefList {inLoop_Inh_TypeAttributeDefList :: Bool}
data Syn_TypeAttributeDefList  = Syn_TypeAttributeDefList {messages_Syn_TypeAttributeDefList :: [Message]}
wrap_TypeAttributeDefList :: T_TypeAttributeDefList  ->
                             Inh_TypeAttributeDefList  ->
                             Syn_TypeAttributeDefList 
wrap_TypeAttributeDefList sem (Inh_TypeAttributeDefList _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_TypeAttributeDefList _lhsOmessages ))
sem_TypeAttributeDefList_Cons :: T_TypeAttributeDef  ->
                                 T_TypeAttributeDefList  ->
                                 T_TypeAttributeDefList 
sem_TypeAttributeDefList_Cons hd_ tl_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _hdOinLoop :: Bool
              _tlOinLoop :: Bool
              _hdImessages :: ([Message])
              _tlImessages :: ([Message])
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _hdOinLoop =
                  _lhsIinLoop
              _tlOinLoop =
                  _lhsIinLoop
              ( _hdImessages) =
                  (hd_ _hdOinLoop )
              ( _tlImessages) =
                  (tl_ _tlOinLoop )
          in  ( _lhsOmessages)))
sem_TypeAttributeDefList_Nil :: T_TypeAttributeDefList 
sem_TypeAttributeDefList_Nil  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
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
type T_TypeName  = Bool ->
                   ( ([Message]))
data Inh_TypeName  = Inh_TypeName {inLoop_Inh_TypeName :: Bool}
data Syn_TypeName  = Syn_TypeName {messages_Syn_TypeName :: [Message]}
wrap_TypeName :: T_TypeName  ->
                 Inh_TypeName  ->
                 Syn_TypeName 
wrap_TypeName sem (Inh_TypeName _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_TypeName _lhsOmessages ))
sem_TypeName_ArrayType :: T_TypeName  ->
                          T_TypeName 
sem_TypeName_ArrayType typeName_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _typeNameOinLoop :: Bool
              _typeNameImessages :: ([Message])
              _lhsOmessages =
                  _typeNameImessages
              _typeNameOinLoop =
                  _lhsIinLoop
              ( _typeNameImessages) =
                  (typeName_ _typeNameOinLoop )
          in  ( _lhsOmessages)))
sem_TypeName_PrecType :: String ->
                         Integer ->
                         T_TypeName 
sem_TypeName_PrecType string_ integer_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_TypeName_SetOfType :: T_TypeName  ->
                          T_TypeName 
sem_TypeName_SetOfType typeName_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _typeNameOinLoop :: Bool
              _typeNameImessages :: ([Message])
              _lhsOmessages =
                  _typeNameImessages
              _typeNameOinLoop =
                  _lhsIinLoop
              ( _typeNameImessages) =
                  (typeName_ _typeNameOinLoop )
          in  ( _lhsOmessages)))
sem_TypeName_SimpleType :: String ->
                           T_TypeName 
sem_TypeName_SimpleType string_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
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
type T_UnOp  = Bool ->
               ( ([Message]))
data Inh_UnOp  = Inh_UnOp {inLoop_Inh_UnOp :: Bool}
data Syn_UnOp  = Syn_UnOp {messages_Syn_UnOp :: [Message]}
wrap_UnOp :: T_UnOp  ->
             Inh_UnOp  ->
             Syn_UnOp 
wrap_UnOp sem (Inh_UnOp _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_UnOp _lhsOmessages ))
sem_UnOp_Abs :: T_UnOp 
sem_UnOp_Abs  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_UnOp_IsNotNull :: T_UnOp 
sem_UnOp_IsNotNull  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_UnOp_IsNull :: T_UnOp 
sem_UnOp_IsNull  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_UnOp_Neg :: T_UnOp 
sem_UnOp_Neg  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_UnOp_Not :: T_UnOp 
sem_UnOp_Not  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_UnOp_SetOf :: T_UnOp 
sem_UnOp_SetOf  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
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
                 ( ([Message]))
data Inh_VarDef  = Inh_VarDef {inLoop_Inh_VarDef :: Bool}
data Syn_VarDef  = Syn_VarDef {messages_Syn_VarDef :: [Message]}
wrap_VarDef :: T_VarDef  ->
               Inh_VarDef  ->
               Syn_VarDef 
wrap_VarDef sem (Inh_VarDef _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_VarDef _lhsOmessages ))
sem_VarDef_VarDef :: String ->
                     T_TypeName  ->
                     (Maybe Expression) ->
                     T_VarDef 
sem_VarDef_VarDef name_ typ_ value_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _typOinLoop :: Bool
              _typImessages :: ([Message])
              _lhsOmessages =
                  _typImessages
              _typOinLoop =
                  _lhsIinLoop
              ( _typImessages) =
                  (typ_ _typOinLoop )
          in  ( _lhsOmessages)))
-- VarDefList --------------------------------------------------
type VarDefList  = [(VarDef)]
-- cata
sem_VarDefList :: VarDefList  ->
                  T_VarDefList 
sem_VarDefList list  =
    (Prelude.foldr sem_VarDefList_Cons sem_VarDefList_Nil (Prelude.map sem_VarDef list) )
-- semantic domain
type T_VarDefList  = Bool ->
                     ( ([Message]))
data Inh_VarDefList  = Inh_VarDefList {inLoop_Inh_VarDefList :: Bool}
data Syn_VarDefList  = Syn_VarDefList {messages_Syn_VarDefList :: [Message]}
wrap_VarDefList :: T_VarDefList  ->
                   Inh_VarDefList  ->
                   Syn_VarDefList 
wrap_VarDefList sem (Inh_VarDefList _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_VarDefList _lhsOmessages ))
sem_VarDefList_Cons :: T_VarDef  ->
                       T_VarDefList  ->
                       T_VarDefList 
sem_VarDefList_Cons hd_ tl_  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _hdOinLoop :: Bool
              _tlOinLoop :: Bool
              _hdImessages :: ([Message])
              _tlImessages :: ([Message])
              _lhsOmessages =
                  _hdImessages ++ _tlImessages
              _hdOinLoop =
                  _lhsIinLoop
              _tlOinLoop =
                  _lhsIinLoop
              ( _hdImessages) =
                  (hd_ _hdOinLoop )
              ( _tlImessages) =
                  (tl_ _tlOinLoop )
          in  ( _lhsOmessages)))
sem_VarDefList_Nil :: T_VarDefList 
sem_VarDefList_Nil  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
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
                     ( ([Message]))
data Inh_Volatility  = Inh_Volatility {inLoop_Inh_Volatility :: Bool}
data Syn_Volatility  = Syn_Volatility {messages_Syn_Volatility :: [Message]}
wrap_Volatility :: T_Volatility  ->
                   Inh_Volatility  ->
                   Syn_Volatility 
wrap_Volatility sem (Inh_Volatility _lhsIinLoop )  =
    (let ( _lhsOmessages) =
             (sem _lhsIinLoop )
     in  (Syn_Volatility _lhsOmessages ))
sem_Volatility_Immutable :: T_Volatility 
sem_Volatility_Immutable  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_Volatility_Stable :: T_Volatility 
sem_Volatility_Stable  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))
sem_Volatility_Volatile :: T_Volatility 
sem_Volatility_Volatile  =
    (\ _lhsIinLoop ->
         (let _lhsOmessages :: ([Message])
              _lhsOmessages =
                  []
          in  ( _lhsOmessages)))