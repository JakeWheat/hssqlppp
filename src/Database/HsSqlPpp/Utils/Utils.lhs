Copyright 2009 Jake Wheat

This file contains some generic utility stuff

> {-# OPTIONS_HADDOCK hide #-}
> {-# LANGUAGE FlexibleContexts #-}


> module Database.HsSqlPpp.Utils.Utils where

> import Data.List
> import Data.Either
> import Control.Arrow
> import Control.Monad.Error
> import Control.Applicative
> import qualified Language.Haskell.Exts as Exts

used to mix regular function composition and >>= in monads, so the
order of application stays the same instead of going backwards when
(.) is used

> infixl 9 |>
> (|>) :: (a -> b) -> (b -> c) -> a -> c
> (|>) = flip (.)

> errorWhen :: (Error a) => Bool -> a -> Either a ()
> errorWhen cond = when cond . Left

> returnWhen :: (Monad m) => Bool -> a -> m a -> m a
> returnWhen c t t1 = if c then return t else t1

> liftME :: a -> Maybe b -> Either a b
> liftME d m = case m of
>                Nothing -> Left d
>                Just b -> Right b

> both :: (a->b) -> (a,a) -> (b,b)
> both fn = fn *** fn

> (<:>) :: (Applicative f) =>
>          f a -> f [a] -> f [a]
> (<:>) a b = (:) <$> a <*> b

> eitherToMaybe :: Either a b -> Maybe b
> eitherToMaybe (Left _) = Nothing
> eitherToMaybe (Right b) = Just b

> fromRight :: b -> Either a b -> b
> fromRight b (Left _) = b
> fromRight _ (Right r) = r

> fromLeft :: a -> Either a b -> a
> fromLeft _ (Left l) = l
> fromLeft a (Right _) = a

> mapEither :: (a->c) -> (b->d) -> Either a b -> Either c d
> mapEither l _ (Left a) = Left $ l a
> mapEither _ r (Right b) = Right $ r b

> mapRight :: (b -> c) -> Either a b -> Either a c
> mapRight = mapEither id

> mapLeft :: (a -> c) -> Either a b -> Either c b
> mapLeft l = mapEither l id

> isRight :: Either a b -> Bool
> isRight (Right _) = True
> isRight (Left _) = False

> leftToEmpty :: (r -> [a]) -> Either l r -> [a]
> leftToEmpty = either (const [])

> replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
> replace _ _ [] = []
> replace old new xs@(y:ys) =
>   case stripPrefix old xs of
>     Nothing -> y : replace old new ys
>     Just ys' -> new ++ replace old new ys'


> split :: Char -> String -> [String]
> split _ ""                =  []
> split c s                 =  let (l, s') = break (== c) s
>                            in  l : case s' of
>                                            [] -> []
>                                            (_:s'') -> split c s''

> liftThrows :: (MonadError t m) => Either t a -> m a
> liftThrows (Left err) = throwError err
> liftThrows (Right val) = return val

run in errort monad, throw error as io error

> wrapET :: (Show e, Monad m) => ErrorT e m a -> m a
> wrapET c = runErrorT c >>= \x ->
>          case x of
>            Left er -> error $ show er
>            Right l -> return l

> wrapETs :: (Monad m) => ErrorT String m a -> m a
> wrapETs c = runErrorT c >>= \x ->
>          case x of
>            Left er -> error er
>            Right l -> return l

error utility - convert either to ErrorT String

> tsl :: (MonadError String m, Show t) => Either t a -> m a
> tsl x = case x of
>                Left s -> throwError $ show s
>                Right b -> return b


> listEither :: [Either a b] -> Either [a] [b]
> listEither es = let (l,r) = partitionEithers es
>                in if null l
>                   then Right r
>                   else Left l


> forceRight :: Show e => Either e a -> a
> forceRight (Left x) = error $ show x
> forceRight (Right x) = x


dodgy code to pretty print a value using haskell-src-exts to try and format it nicely

> ppExpr :: Show s => s -> String
> ppExpr s =
>   case Exts.parseExp (show s) of
>     Exts.ParseOk ast -> Exts.prettyPrint ast
>     x -> error $ show x


> npartition :: (Eq a, Eq b) => (a -> b) -> [a] -> [(b,[a])]
> npartition keyf l =
>   np [] l
>   where
>     np acc (p:ps) = let k = keyf p
>                     in np (insertWith (++) k [p] acc) ps
>     np acc [] = acc

> insertWith :: (Eq k, Eq a) => (a -> a -> a) -> k -> a -> [(k,a)] -> [(k,a)]
> insertWith ac k v m =
>     case lookup k m of
>       Nothing -> m ++ [(k,v)]
>       Just v' -> let nv = ac v' v
>                  in map (\p@(k1,_) -> if k1 == k
>                                       then (k1,nv)
>                                       else p) m
