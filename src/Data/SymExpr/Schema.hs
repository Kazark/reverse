{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
module Data.SymExpr.Schema
  ( Schema, Record, DUnion(..)
  , accept, pair, impossible
  , dUnion, enum
  , refine, liftParser
  , listOf, nonEmptyOf
  , (<^>), record, alist
  , isSymbol, isText, isUnit
  , deserialize
  ) where

import           Data.Either.Combinators
import           Data.Function ((&))
import           Data.Functor (($>))
import           Data.List (foldr, foldl')
import           Data.List.NonEmpty (NonEmpty, nonEmpty)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.SymExpr.Racket
import           Data.SymExpr.Types
import           Data.Void (Void)

-- | The help message mentions Racket, but some error messages should be valid
-- | in Scheme dialects, e.g. Chez.
help :: String
help =
  [ "This quasiquote represents the S-expression that failed to deserialize,"
  , "augmented with failures marked within the structure itself by unquotes"
  , "(marked with a backtick). This simultaneously provides the context of"
  , "the failures, allows them to be reported together in a large batch, and"
  , "presents them in a well-structured fashion (as in, this whole file is"
  , "valid Racket code!) with little danger of confusing what is part of the"
  , "original S-expression and what marks failure. The output of running it"
  , "has less information than the code itself, but may help you understand it."
  , ""
  , "The presence of a predicate function (one which ends in a question mark,"
  , "as well as memq) marks the falsity of that condition. The void function"
  , "marks the presence of an S-expression where none was expected. The displan"
  , "function is used when an error message from a parse function without a"
  , "Racket built-in equivalent needs to be included."
  ] & unlines

newtype Schema s a
  = Schema { _check :: SymExpr s -> Either (RExpr s) a }
  deriving Functor

failed' :: KnownSymbol -> RExpr s -> RExpr s
failed' f = Unquote . apK f

maybeQuote :: RExpr s -> RExpr s
maybeQuote = \case
  r@(ConsC _ _) -> Quote r
  Nil -> Quote Nil
  r@(Symbol _) -> Quote r
  r -> r

failed :: KnownSymbol -> SymExpr s -> RExpr s
failed f = failed' f . maybeQuote . toRExpr

-- | "This S-expression is the very thing I was trying to construct. It is
-- | therefore, by definition, valid."
accept :: Schema s (SymExpr s)
accept = Schema Right

isSymbol :: Schema s s
isSymbol =
  Schema \case
    Symb s -> Right s
    e -> Left $ failed SymbolQ e

refine :: (SymExpr s -> a -> Either (RExpr s) b) -> Schema s a -> Schema s b
refine parse (Schema check) = Schema \e -> do
  x <- check e
  parse e x

liftParser :: (a -> Either String b) -> Schema s a -> Schema s b
liftParser parse =
  refine (const $ mapLeft (failed Display . Text . (\s -> s ++ "\n")) . parse)

-- | Parse the symbolic expression to a list.
symExprToList :: SymExpr s -> Maybe [SymExpr s]
symExprToList = \case
  Unit -> Just []
  Cons x xs -> do
    xs' <- symExprToList xs
    return (x : xs')
  _ -> Nothing

listOf' :: Schema s a -> [SymExpr s] -> Either (RExpr s) [(SymExpr s, a)]
listOf' (Schema check) =
  foldr (\e ->
    let x = check e
    in \case
      Left xs -> Left $ ConsC (fromLeft (toRExpr e) x) xs
      Right xs ->
        case x of
          Left x' ->
            -- Since we are already in a foldr, this should be a foldl'. A
            -- second foldr would undo the effects of the first.
            let goods = foldl' (\es (e', _) -> ConsC (toRExpr e') es) Nil xs
            in Left (ConsC x' goods)
          Right x' -> Right ((e, x') : xs)
  ) (Right [])

listOf :: Schema s a -> Schema s [a]
listOf schema = Schema \e ->
  case symExprToList e of
    Nothing -> Left $ failed ListQ e
    Just es -> fmap (fmap snd) $ listOf' schema es

lengthAtLeast :: Integer -> SymExpr s -> RExpr s
lengthAtLeast x s = apN LessThan [Integer x, Quote $ toRExpr s]

nonEmptyOf :: Schema s a -> Schema s (NonEmpty a)
nonEmptyOf =
  refine (\e x ->
    case nonEmpty x of
      Nothing -> Left $ lengthAtLeast 1 e
      Just x' -> Right x'
  ) . listOf

isText :: Schema s String
isText = Schema \case
  Text x -> Right x
  e -> Left $ failed StringQ e

isUnit :: Schema s ()
isUnit = Schema \case
  Unit -> Right ()
  e -> Left $ failed EmptyQ e

memqFailed :: s -> RExpr s -> RExpr s
memqFailed k e = apN Memq [Quote (Symbol k), maybeQuote e]

newtype DUnion s a
  = DUnion [(s, Schema s a)]
  -- | These can be open because we don't have to do type inference; we always
  -- | know the type ahead of time.
  deriving Semigroup via [(s, Schema s a)]
  deriving Functor

-- | Discriminated union
dUnion :: Eq s => DUnion s a -> Schema s a
dUnion (DUnion cases) = Schema \case
  Cons k' v ->
    case k' of
      Symb k ->
        case lookup k cases of
          Nothing ->
            let cases' = toRExpr $ foldr Cons Unit $ fmap (Symb . fst) cases
            in Left $ Unquote $ memqFailed k $cases'
          Just (Schema check) ->
            mapLeft (ConsC (Symbol k)) $ check v
      _ -> Left $ failed SymbolQ k'
  e -> Left $ failed ConsQ e

enum :: Eq s => [(s, a)] -> DUnion s a
enum = DUnion . fmap (fmap (\x -> isUnit $> x))

pair :: Schema s a -> Schema s b -> Schema s (a, b)
pair (Schema check1) (Schema check2) = Schema \case
  Cons car cdr ->
    case (check1 car, check2 cdr) of
      (Right x, Right y) -> Right (x, y)
      (Left x, Left y) -> Left (ConsC x y)
      (Left x, Right _) -> Left (ConsC x (toRExpr cdr))
      (Right _, Left x) -> Left (ConsC (toRExpr car) x)
  e -> Left $ failed ConsQ e

impossible :: Schema s Void
impossible = Schema $ Left . failed Void

data Record s a where
  EmptyR :: a -> Record s a
  Row :: s -> Schema s a -> Record s (a -> b) -> Record s b

record :: (a -> b) -> Record s (a -> b)
record = EmptyR

-- | Infix alias for `Row`, analogous to <*>
(<^>) :: Record s (a -> b) -> (s, Schema s a) -> Record s b
-- Is there a more elegant way to do a ternary operator in Haskell?
r <^> (k, s) = Row k s r

-- | Record errors
data RErs s = RErs { _inPlace :: Map s (RExpr s), _missing :: [s] }

insertInPlace :: Ord s => s -> RExpr s -> RErs s -> RErs s
insertInPlace k x (RErs p m) = RErs (Map.insert k x p) m

insertMissing :: Ord s => s -> RErs s -> RErs s
insertMissing k (RErs p m) = RErs p $ k : m

rowErred :: Ord s => s -> RExpr s -> RErs s -> Either (RErs s) b
rowErred k x = Left . insertInPlace k x

toRErs :: Map s (SymExpr s) -> RErs s
toRErs m = RErs (Map.map toRExpr m) []

missingRow :: Ord s => s -> RErs s -> Either (RErs s) b
missingRow k = Left . insertMissing k

checkRecord :: Ord s => Record s a -> Map s (SymExpr s) -> Either (RErs s) a
checkRecord (EmptyR x) _ = Right x
checkRecord (Row k (Schema sCheck) f) es =
  let v = fmap (\x -> (x, sCheck x)) $ Map.lookup k es
  in case (v, checkRecord f es) of
    (Just (_, Right x), Right f') -> Right $ f' x
    (Just (_, Left x), Right _) -> rowErred k x $ toRErs es
    (Just (_, Right _), Left xs) -> Left xs
    (Just (_, Left x), Left xs) -> rowErred k x xs
    (Nothing, Right _) -> missingRow k $ toRErs es
    (Nothing, Left xs) -> missingRow k xs

alist' :: Ord s => Schema s (Map s (SymExpr s))
alist' = Map.fromList <$> listOf (pair isSymbol accept)

formatInPlaceRErs :: Map s (RExpr s) -> RExpr s
formatInPlaceRErs =
  rExprList . fmap (\(x, y) -> ConsC (Symbol x) y) . Map.toList

formatMissing :: s -> RExpr s
formatMissing k = memqFailed k $ KnownSymbol Record

formatRecordErrors :: RErs s -> RExpr s
formatRecordErrors (RErs p m) =
  let x = formatInPlaceRErs p
  in case m of
    [] -> x
    ms ->
      let
        body = fmap formatMissing ms
        assn = ConsC (ConsC (KnownSymbol Record) $ ConsC (Quasiquote x) Nil) Nil
      in Unquote $ apN Let $ assn : body

alist :: Ord s => Record s a -> Schema s a
alist r = Schema \e -> do
  let (Schema check) = alist'
  x <- check e
  mapLeft formatRecordErrors $ checkRecord r x

wrapFailure :: RExpr s -> RExpr s
wrapFailure = BlockComment help . Quasiquote

deserialize :: Schema s a -> SymExpr s -> Either (RExpr s) a
deserialize (Schema check) = mapLeft wrapFailure . check
