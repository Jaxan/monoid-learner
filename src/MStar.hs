{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module MStar where

-- Copyright Joshua Moerman 2020, 2021
-- M*: an algorithm to query learn the syntactic monoid
-- for a regular language. I hope it works correctly.
-- This is a rough sketch, and definitely not cleaned up.

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (Word)

type Word a = Seq a
type Alphabet a = Set a
type MembershipQuery m a = Word a -> m Bool

squares :: Ord a => Set (Word a) -> Set (Word a)
squares l = Set.map (uncurry (<>)) (Set.cartesianProduct l l)

setPlus :: Ord a => Set a -> Set (Word a) -> Set (Word a)
setPlus alph rows = Set.map pure alph `Set.union` squares rows

-- Left and Right concats, these are like columns, but they act
-- both left and right. Maybe a better word would be "tests".
type Context a = (Word a, Word a)

type Index a = Word a

-- State of the M* algorithm
data State a = State
  { rows :: Set (Index a),
    contexts :: Set (Context a),
    cache :: Map (Word a) Bool,
    alphabet :: Set a
  }
  deriving (Show, Eq)

-- Row data for an index
row :: Ord a => State a -> Index a -> Map (Context a) Bool
row State {..} m = Map.fromSet (\(l, r) -> cache Map.! (l <> m <> r)) contexts

-- Difference of two rows (i.e., all contexts in which they differ)
difference :: Ord a => State a -> Index a -> Index a -> [Context a]
difference State {..} m1 m2 = [(l, r) | (l, r) <- Set.toList contexts, cache Map.! (l <> m1 <> r) /= cache Map.! (l <> m2 <> r)]

-- Initial state of the algorithm
initialState :: (Monad m, Ord a) => Alphabet a -> MembershipQuery m a -> m (State a)
initialState alphabet mq = do
  let rows = Set.singleton Seq.empty
      contexts = Set.singleton (Seq.empty, Seq.empty)
      initialQueries = Set.map (\(m, (l, r)) -> l <> m <> r) $ Set.cartesianProduct (setPlus alphabet rows) contexts
      initialQueriesL = Set.toList initialQueries
  results <- mapM mq initialQueriesL
  let cache = Map.fromList (zip initialQueriesL results)
  return $
    State
      { rows = rows,
        contexts = contexts,
        cache = cache,
        alphabet = alphabet
      }


-- CLOSED --
-- Returns all pairs which are not closed
closed :: Ord a => State a -> Set (Index a)
closed s@State {..} = Set.filter notExists (setPlus alphabet rows `Set.difference` rows)
  where
    allRows = Set.map (row s) rows
    notExists m = not (row s m `Set.member` allRows)

-- Returns a fix for the non-closedness. (Some element)
fixClosed1 :: Set (Index a) -> Maybe (Word a)
fixClosed1 = listToMaybe . Set.toList

-- Returns a fix for the non-closedness. (Shortest element)
fixClosed2 :: Set (Index a) -> Maybe (Word a)
fixClosed2 = listToMaybe . List.sortOn Seq.length . Set.toList

-- Adds a new element
addRow :: (Monad m, Ord a) => Index a -> MembershipQuery m a -> State a -> m (State a)
addRow m mq s@State {..} = do
  let newRows = Set.insert m rows
      queries = Set.map (\(mi, (l, r)) -> l <> mi <> r) $ Set.cartesianProduct (setPlus alphabet newRows) contexts
      queriesRed = queries `Set.difference` Map.keysSet cache
      queriesRedL = Set.toList queriesRed
  results <- mapM mq queriesRedL
  let dCache = Map.fromList (zip queriesRedL results)
  return $ s {rows = newRows, cache = cache <> dCache}


-- CONSISTENT --
-- Not needed when counterexamples are added as columns, the table
-- then remains sharp. There is a more efficient way of testing this
-- property, see https://doi.org/10.1007/978-3-030-71995-1_26
-- Returns all inconsistencies
consistent :: Ord a => State a -> [(Index a, Index a, Index a, Index a, [Context a])]
consistent s@State {..} = [(m1, m2, n1, n2, d) | (m1, m2) <- equalRowPairs, (n1, n2) <- equalRowPairs, let d = difference s (m1 <> n1) (m2 <> n2), not (Prelude.null d)]
  where
    equalRowPairs = Set.toList . Set.filter (\(m1, m2) -> row s m1 == row s m2) $ Set.cartesianProduct rows rows

-- Returns a fix for consistency.
fixConsistent :: Ord a => State a -> [(Index a, Index a, Index a, Index a, [Context a])] -> Maybe (Context a)
fixConsistent _ [] = Nothing
fixConsistent _ ((_, _, _, _, []) : _) = error "Cannot happen cons"
fixConsistent s ((m1, m2, n1, n2, (l, r) : _) : _) = Just . head . Prelude.filter valid $ [(l <> m1, r), (l <> m2, r), (l, n1 <> r), (l, n2 <> r)] -- Many choices here
  where
    valid c = not (Set.member c (contexts s))

-- Adds a test
addContext :: (Monad m, Ord a) => Context a -> MembershipQuery m a -> State a -> m (State a)
addContext lr mq s@State {..} = do
  let newContexts = Set.insert lr contexts
      queries = Set.map (\(m, (l, r)) -> l <> m <> r) $ Set.cartesianProduct (setPlus alphabet rows) newContexts
      queriesRed = queries `Set.difference` Map.keysSet cache
      queriesRedL = Set.toList queriesRed
  results <- mapM mq queriesRedL
  let dCache = Map.fromList (zip queriesRedL results)
  return $ s {contexts = newContexts, cache = cache <> dCache}


-- ASSOCIATIVITY --
-- Returns non-associativity results. Implemented in a brute force way
-- This is something new in M*, it's obviously not needed in L*
associative :: Ord a => State a -> [(Index a, Index a, Index a, Index a, Index a, [Context a])]
associative s@State {..} = [(m1, m2, m3, m12, m23, d) | (m1, m2, m3, m12, m23) <- allCandidates, let d = difference s (m12 <> m3) (m1 <> m23), not (Prelude.null d)]
  where
    rs = Set.toList rows
    allTriples = [(m1, m2, m3) | m1 <- rs, m2 <- rs, m3 <- rs]
    allCandidates = [(m1, m2, m3, m12, m23) | (m1, m2, m3) <- allTriples, m12 <- rs, row s m12 == row s (m1 <> m2), m23 <- rs, row s m23 == row s (m2 <> m3)]

-- Fix for associativity, needs a membership query
-- See https://doi.org/10.1007/978-3-030-71995-1_26
fixAssociative :: (Monad m, Ord a) => [(Index a, Index a, Index a, Index a, Index a, [Context a])] -> MembershipQuery m a -> State a -> m (Maybe (Context a))
fixAssociative [] _ _ = return Nothing
fixAssociative ((_, _, _, _, _, []) : _) _ _ = error "Cannot happen assoc"
fixAssociative ((m1, m2, m3, m12, m23, e@(l, r) : _) : _) mq table = do
  b <- mq (l <> m1 <> m2 <> m3 <> r)
  if row table (m12 <> m3) Map.! e /= b
    then return (Just (l, m3 <> r))
    else return (Just (l <> m1, r))


-- Abstract data type for a monoid. The map from the alphabet
-- determines the homomorphism from Words to this monoid
data MonoidAcceptor a q = MonoidAcceptor
  { elements :: [q], -- set of elements
    unit :: q, -- the unit element
    multiplication :: q -> q -> q, -- multiplication functions
    accept :: q -> Bool, -- accepting subset
    alph :: a -> q -- map from alphabet
  }

-- Given a word, is it accepted by the monoid?
acceptMonoid :: MonoidAcceptor a q -> Word a -> Bool
acceptMonoid MonoidAcceptor {..} w = accept (foldr multiplication unit (fmap alph w))

-- HYPOTHESIS --
-- Syntactic monoid construction
constructMonoid :: Ord a => State a -> MonoidAcceptor a Int
constructMonoid s@State {..} =
  MonoidAcceptor
    { elements = [0 .. Set.size allRows - 1],
      unit = unit,
      multiplication = curry (multMap Map.!),
      accept = (accMap Map.!),
      alph = rowToInt . Seq.singleton -- incorrect if symbols behave trivially
    }
  where
    allRows = Set.map (row s) rows
    rowMap = Map.fromList (zip (Set.toList allRows) [0 ..])
    rowToInt m = rowMap Map.! row s m
    unit = rowToInt Seq.empty
    accMap = Map.fromList [(rowMap Map.! r, r Map.! (Seq.empty, Seq.empty)) | r <- Set.toList allRows]
    multList = [((rowToInt m1, rowToInt m2), rowToInt (m1 <> m2)) | m1 <- Set.toList rows, m2 <- Set.toList rows]
    multMap = Map.fromList multList


-- Learns until it can construct a monoid
-- Please do counterexample handling yourself
learn :: (Monad m, Ord a) => MembershipQuery m a -> State a -> m (State a)
learn mq = makeClosedAndConsistentAndAssoc
  where
    makeClosed s = do
      case fixClosed2 $ closed s of
        Just m -> do
          s2 <- addRow m mq s
          makeClosed s2
        Nothing -> return s
    makeClosedAndConsistent s = do
      s2 <- makeClosed s
      case fixConsistent s2 $ consistent s2 of
        Just c -> do
          s3 <- addContext c mq s2
          makeClosedAndConsistent s3
        Nothing -> return s2
    makeClosedAndConsistentAndAssoc s = do
      s2 <- makeClosedAndConsistent s
      result <- fixAssociative (associative s2) mq s2
      case result of
        Just a -> do
          s3 <- addContext a mq s2
          makeClosedAndConsistentAndAssoc s3
        Nothing -> return s2
