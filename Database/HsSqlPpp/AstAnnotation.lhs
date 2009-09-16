Copyright 2009 Jake Wheat

The annotation data types and utilities for working with them.

Annotations are used to store source positions, types, errors,
warnings, scope deltas, information, and other stuff a client might
want to use when looking at an ast. Internal annotations which are
used in the type-checking/ annotation process use the attribute
grammar code and aren't exposed.

> module Database.HsSqlPpp.AstAnnotation where

> import Database.HsSqlPpp.TypeType

> type Annotation = [AnnotationElement]

> data AnnotationElement = SourcePos String Int Int
>                        | TypeAnnotation
>                        | TypeErrorA TypeErrorInfo
>                          deriving (Eq, Show)

> class Annotated a where
>     ann :: a -> Annotation
>     setAnn :: a -> Annotation -> a
>     changeAnn :: a -> (Annotation -> Annotation) -> a
>     changeAnn a f = setAnn a $ f $ ann a
>     changeAnnRecurse :: (Annotation -> Annotation) -> a -> a

 > changeAnnotations :: Annotated a => (Annotation -> Annotation) -> [a] -> [a]
 > changeAnnotations f as = map (changeAnnRecurse f) as

> stripAnnotations :: Annotated a => a -> a
> stripAnnotations a = changeAnnRecurse (const []) a

sourcepos
types
errors
warnings
scope deltas
info


Annotation planning
add an annotation field to most nodes in the ast
something like
data Annotation = NonAnnotation
                | SourcePosAnnotation SourcePos
                | CheckedAnnotation SourcePos Type [Messages]

alter the parser to add sourcepositions to these nodes - assume that
   one source position per node will be enough for now (not
   necessarily a good assumption with weird sql syntax), and see if
   this gives us enough for good error messages, etc..

question:
if a node has no source position e.g. the all in select all or select
   distinct may correspond to a token or may be synthesized as the
   default if neither all or distinct is present. Should this have the
   source position of where the token would have appeared, should it
   inherit it from its parent, should there be a separate ctor to
   represent a fake node with no source position?

The way the type checking will then work is that instead of producing
   some attribute values it will produce a transformed ast tree with
   the type and message fields filled in. Then supply some utility
   functions to e.g. extract all the messages, extract all the type
   errors, extract the top level types, etc. Use some sort of tree
   walker to implement these utils

The way types and type errors will work is that instead of / in
   addition to the types being passed in attributes, they'll be saved
   in the transformed tree. Type errors won't percolate up to the top
   level, but sit with the node that is in error. Any parent nodes
   which need this type to calculate their own type, will use a
   separate error to say type unknown. If they can calculate their
   type without depending on a type erroring child node, then they do
   that, so e.g. typing a set of statements with create functions and
   views which use those functions: if the statements inside the
   functions have type errors, we can still find the types of the
   views, assuming that the function params and return type check
   properly, and are correct.
