{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module MStar where

-- Copyright Joshua Moerman 2020
-- M*: an algorithm to query learn the syntactic monoid
-- for a regular language. I hope it works correctly.
-- This is a rough sketch, and definitely not cleaned up.

import Control.Applicative (Applicative ((<*>)), (<$>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq, empty, singleton)
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (Word)

type Word a = Seq a
type Alphabet a = Set a

type MembershipQuery a = Word a -> Bool

-- If l includes the empty word, then this set also includes l
squares :: _ => Set (Word a) -> Set (Word a)
squares l = Set.map (uncurry (<>)) (Set.cartesianProduct l l)

-- Left and Right concats, these are like columns, but they act
-- both left and right. Maybe a better word would be "tests".
type Context a = (Word a, Word a)
type Contexts a = Set (Context a)

initialContexts :: Contexts a
initialContexts = Set.singleton (empty, empty)

type Row a = Word a
type Rows a = Set (Row a)

initialRows :: Ord a => Alphabet a -> Rows a
initialRows alphabet = Set.singleton empty `Set.union` Set.map singleton alphabet

-- State of the M* algorithm
data State a = State
  { rows :: Rows a,
    contexts :: Contexts a,
    cache :: Map (Word a) Bool
  }
  deriving (Show, Eq)

-- Row data for an index
row :: _ => State a -> Row a -> Map (Context a) Bool
row State {..} m = Map.fromSet (\(l, r) -> cache Map.! (l <> m <> r)) contexts

-- Difference of two rows (i.e., all contexts in which they differ)
difference :: _ => State a -> Row a -> Row a -> [Context a]
difference State {..} m1 m2 = [(l, r) | (l, r) <- Set.toList contexts, cache Map.! (l <> m1 <> r) /= cache Map.! (l <> m2 <> r)]

-- Initial state of the algorithm
initialState :: Ord a => Alphabet a -> MembershipQuery a -> State a
initialState alphabet mq =
  State
    { rows = rows,
      contexts = contexts,
      cache = cache
    }
  where
    rows = initialRows alphabet
    contexts = initialContexts
    initialQueries =
      Set.map (\(m, (l, r)) -> l <> m <> r) $
        Set.cartesianProduct (squares rows) contexts
    cache = Map.fromSet mq initialQueries


-- CLOSED --
-- Returns all pairs which are not closed
closed :: _ => State a -> [(Row a, Row a)]
closed s@State {..} = [(m1, m2) | (m1, m2) <- Set.toList rowPairs, notExists (row s (m1 <> m2))]
  where
    rowPairs = Set.cartesianProduct rows rows
    allRows = Set.map (row s) rows
    notExists m = not (m `Set.member` allRows)

-- Returns a fix for the non-closedness.
fixClosed :: _ => _ -> Maybe (Word a)
fixClosed [] = Nothing
fixClosed ((a, b) : _) = Just (a <> b)

-- Adds a new element
addRow :: Ord a => Row a -> MembershipQuery a -> State a -> State a
addRow m mq s@State {..} = s {rows = newRows, cache = cache <> dCache}
  where
    newRows = Set.insert m rows
    queries = Set.map (\(mi, (l, r)) -> l <> mi <> r) $ Set.cartesianProduct (squares newRows) contexts
    queriesRed = queries `Set.difference` Map.keysSet cache
    dCache = Map.fromSet mq queriesRed


-- CONSISTENT --
-- Returns all inconsistencies
consistent :: _ => State a -> _
consistent s@State {..} = [(m1, m2, n1, n2, d) | (m1, m2) <- equalRowPairs, (n1, n2) <- equalRowPairs, let d = difference s (m1 <> n1) (m2 <> n2), not (Prelude.null d)]
  where
    equalRowPairs = Prelude.filter (\(m1, m2) -> row s m1 == row s m2) $ (,) <$> Set.toList rows <*> Set.toList rows

-- Returns a fix for consistency.
fixConsistent :: _ => State a -> _ -> Maybe (Context a)
fixConsistent _ [] = Nothing
fixConsistent _ ((_, _, _, _, []) : _) = error "Cannot happen cons"
fixConsistent s ((m1, m2, n1, n2, (l, r) : _) : _) = Just . head . Prelude.filter valid $ [(l <> m1, r), (l <> m2, r), (l, n1 <> r), (l, n2 <> r)] -- Many choices here
  where
    valid c = not (Set.member c (contexts s))

-- Adds a test
addContext :: Ord a => Context a -> MembershipQuery a -> State a -> State a
addContext lr mq s@State {..} = s {contexts = newContexts, cache = cache <> dCache}
  where
    newContexts = Set.insert lr contexts
    queries = Set.map (\(m, (l, r)) -> l <> m <> r) $ Set.cartesianProduct (squares rows) newContexts
    queriesRed = queries `Set.difference` Map.keysSet cache
    dCache = Map.fromSet mq queriesRed


-- ASSOCIATIVITY --
-- Returns non-associativity results. Implemented in a brute force way
-- This is something new in M*, it's obviously not needed in L*
associative :: _ => State a -> _
associative s@State {..} = [(m1, m2, m3, m12, m23, d) | (m1, m2, m3, m12, m23) <- allCandidates, let d = difference s (m12 <> m3) (m1 <> m23), not (Prelude.null d)]
  where
    rs = Set.toList rows
    allTriples = [(m1, m2, m3) | m1 <- rs, m2 <- rs, m3 <- rs]
    allCandidates = [(m1, m2, m3, m12, m23) | (m1, m2, m3) <- allTriples, m12 <- rs, row s m12 == row s (m1 <> m2), m23 <- rs, row s m23 == row s (m2 <> m3)]

-- Fix for associativity (hopefully)
fixAssociative :: _ => _ -> Maybe (Context a)
fixAssociative [] = Nothing
fixAssociative ((_, _, _, _, _, []) : _) = error "Cannot happen assoc"
fixAssociative ((_, _, m3, _, _, (l, r) : _) : _) = Just (l, m3 <> r) -- TODO: many choices


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
constructMonoid :: _ => State a -> MonoidAcceptor a Int
constructMonoid s@State {..} =
  MonoidAcceptor
    { elements = [0 .. Set.size allRows - 1],
      unit = unit,
      multiplication = curry (multMap Map.!),
      accept = (accMap Map.!),
      alph = rowToInt . singleton
    }
  where
    allRows = Set.map (row s) rows
    rowMap = Map.fromList (zip (Set.toList allRows) [0 ..])
    rowToInt m = rowMap Map.! row s m
    unit = rowToInt empty
    accMap = Map.fromList [(rowMap Map.! r, r Map.! (empty, empty)) | r <- Set.toList allRows]
    multList = [((rowToInt m1, rowToInt m2), rowToInt (m1 <> m2)) | m1 <- Set.toList rows, m2 <- Set.toList rows]
    multMap = Map.fromList multList


-- Learns until it can construct a monoid
-- Please do counterexample handling yourself
learn :: _ => MembershipQuery a -> State a -> (MonoidAcceptor a _, State a)
learn mq s =
  case fixClosed $ closed s of
    Just m -> learn mq (addRow m mq s)
    Nothing ->
      case fixConsistent s $ consistent s of
        Just c -> learn mq (addContext c mq s)
        Nothing ->
          case fixAssociative $ associative s of
            Just c -> learn mq (addContext c mq s)
            Nothing -> (constructMonoid s, s)
