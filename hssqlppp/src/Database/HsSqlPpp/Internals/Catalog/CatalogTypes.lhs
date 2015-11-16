
> {-# LANGUAGE DeriveDataTypeable,OverloadedStrings #-}
>
> module Database.HsSqlPpp.Internals.Catalog.CatalogTypes
>     (
>      -- catalog type plus values
>      Catalog(..)
>     ,emptyCatalog
>     ,NameComponent(..)
>     ,ncStr
>     ,ncStrT
>     ,CompositeFlavour(..)
>     ,CatName
>     ,CatNameExtra(..)
>     ,mkCatNameExtra
>     ,mkCatNameExtraNN
>      -- catalog updates
>     ,CatalogUpdate(..)
>      -- temp stuff for old typeconversion
>     ,OperatorPrototype

>     ,CastContext(..)
>     ) where

> --import Control.Monad
> --import Data.List
> import Data.Data
> import Data.Char
> --import Data.Maybe

> import qualified Data.Map as M
> import qualified Data.Set as S
> import Database.HsSqlPpp.Internals.TypesInternal
> --import Database.HsSqlPpp.Utils.Utils
> import Data.Text (Text)
> import qualified Data.Text as T
> --import qualified Data.Text.Lazy as LT

> -- | represents the name of something in the catalog, when schema
> -- support is added then this will change to (String,String)
> type CatName = Text
> -- | type name and precision and nullability
> data CatNameExtra = CatNameExtra {
>   catName:: CatName,
>   catPrecision:: Maybe Int,
>   catScale:: Maybe Int,
>   catNullable:: Bool
> } deriving (Eq,Ord,Show,Typeable,Data)
> mkCatNameExtra:: CatName -> CatNameExtra
> mkCatNameExtra cn = CatNameExtra cn Nothing Nothing True
> mkCatNameExtraNN:: CatName -> CatNameExtra
> mkCatNameExtraNN cn = CatNameExtra cn Nothing Nothing False

> data CompositeFlavour = Composite | TableComposite | ViewComposite
>                         deriving (Eq,Show,Ord,Data,Typeable)


---------------------------------------------------------------

operators

operators is used as the generic term for all sorts of function things:
prefix, postfix, binary operators, functions, aggregate functions and
window functions

The information currently stored is:
name, parameter types, return type and variadic flag

> -- | name, inparams, outtype, is variadic?
> type OperatorPrototype = (CatName, [Type], Type, Bool)

> -- | The main datatype, this holds the catalog and context
> -- information to type check against.
> data Catalog = Catalog
>     {catSchemas :: S.Set CatName
>     ,catScalarTypeNames :: S.Set CatName -- one name component per type
>     ,catDomainTypes :: M.Map CatName CatName -- stores the base type name
>                                              -- constraint is stored separately
>      --,catEnumTypes :: {[(String,[String])]}
>     ,catCompositeTypes :: M.Map CatName
>                                 (CompositeFlavour
>                                 ,[(Text,CatNameExtra)] -- public attrs
>                                 ,[(Text,CatName)])-- system columns
>     ,catArrayTypes :: M.Map CatName CatName --pg array type name, base type name
>     ,catPrefixOps :: M.Map CatName [OperatorPrototype]
>     ,catPostfixOps :: M.Map CatName [OperatorPrototype]
>     ,catBinaryOps :: M.Map CatName [OperatorPrototype]
>     ,catFunctions :: M.Map CatName [OperatorPrototype]
>     ,catAggregateFunctions :: M.Map CatName [OperatorPrototype]
>     ,catWindowFunctions :: M.Map CatName [OperatorPrototype]
>     ,catTables :: M.Map (CatName,CatName)
>                   ([(Text,TypeExtra)] -- public attrs
>                   ,[(Text,Type)]) -- system columns
>     -- needs more work:
>     ,catCasts :: S.Set (Type,Type,CastContext)
>     ,catTypeCategories :: M.Map Type (Text,Bool)
>      -- save the updates
>     ,catUpdates :: [CatalogUpdate]
>     }
>                deriving (Eq,Show,Data,Typeable)


> -- | Use to note what the flavour of a cast is, i.e. if/when it can
> -- be used implicitly.
> data CastContext = ImplicitCastContext
>                  | AssignmentCastContext
>                  | ExplicitCastContext
>                    deriving (Eq,Show,Ord,Typeable,Data)


> -- | Represents an empty catalog.
> emptyCatalog :: Catalog
> emptyCatalog = Catalog S.empty S.empty M.empty M.empty M.empty
>                        M.empty M.empty
>                        M.empty M.empty M.empty M.empty M.empty
>                        S.empty M.empty
>                        []


--------------------------------------------------


name component - this represents quoted and unquoted
possibly-qualified names (so names of things are lists of
namecomponents). Perhaps should be a syntactic namecomponent which is
in AstInternal, and a semantic namecomponent which is used here, but I
am lazy so the same type is shared.

The name components are only used here so that the logic for ignoring
or respecting case is in one place, these are only used in the query
functions and not in catalog values themselves.

> data NameComponent = Nmc String
>                    | QNmc String -- quoted
>                    | AntiNameComponent String
>                      deriving (Data,Eq,Show,Typeable,Ord)
> -- this is a transition function
> -- it should be removed when ready, since all the code
> -- should be working with NameComponents directly
> ncStr :: NameComponent -> String
> ncStr (Nmc n) = map toLower n
> ncStr (QNmc n) = n
> ncStr (AntiNameComponent _n) =
>   error "tried to get the name component string of an anti name component"

> ncStrT :: NameComponent -> Text
> ncStrT (Nmc n) = T.pack $ map toLower n
> ncStrT (QNmc n) = T.pack n
> ncStrT (AntiNameComponent _n) =
>   error "tried to get the name component string of an anti name component"


todo: use left or something instead of error

------------------------------------------------------

 updates

> data CatalogUpdate =
>     -- | register a schema with the given name
>     CatCreateSchema CatName
>     -- | register a base scalar type with the given name
>   | CatCreateScalarType CatName
>     -- | register a domain type with name and base type
>   | CatCreateDomainType CatName CatName
>     -- | register an array type with name and base type
>   | CatCreateArrayType CatName CatName
>     -- | register a prefix op, opname, param type, return type
>   | CatCreatePrefixOp CatName CatName CatName
>     -- | register a postfix op, opname, param type, return type
>   | CatCreatePostfixOp CatName CatName CatName
>     -- | register a binary op, opname, the two param types, return type
>   | CatCreateBinaryOp CatName CatName CatName CatName
>     -- | register a function: name, param types, retsetof, return type
>   | CatCreateFunction CatName [CatName] Bool CatName
>     -- | register a variadic function: name, param types, retsetof, return type
>     -- the last parameter will be wrapped in an array type
>   | CatCreateVariadicFunction CatName [CatName] Bool CatName
>     -- | special ops include between, substring, position, basically
>     -- all operators/functions which use mixfix or extra syntax
>     -- (not including non scalar functions like aggregates)
>   | CatCreateSpecialOp CatName [CatName] Bool CatName

>     -- | register a aggregate: name, param types, return type
>   | CatCreateAggregate CatName [CatName] CatName
>     -- | register a table only: name, (colname,typename) pairs
>   | CatCreateTable (CatName,CatName) [(CatName,CatNameExtra)]
>     -- | register a cast in the catalog
>   | CatCreateCast CatName CatName CastContext
>     -- | register a type category for a type (used in the implicit cast resolution)
>   | CatCreateTypeCategoryEntry CatName (Text,Bool)
>     deriving (Eq,Ord,Typeable,Data,Show)
