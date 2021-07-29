{-# LANGUAGE PartialTypeSignatures #-}

module Equivalence where

import qualified Data.Map as Map
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Monoid (MonoidAcceptor (..))

equivalent :: (Ord q1, Ord q2) => Set a -> MonoidAcceptor a q1 -> MonoidAcceptor a q2 -> Bool
equivalent alphabet m1 m2 = case searchCounterexample alphabet m1 m2 of
  Just _ -> False
  Nothing -> True

searchCounterexample :: (Ord q1, Ord q2) => Set a -> MonoidAcceptor a q1 -> MonoidAcceptor a q2 -> Maybe (Seq a)
searchCounterexample alphabet m1 m2 = go workingSet Map.empty
  where
    workingSet = (unit m1, unit m2, Seq.Empty) :<| fmap (\a -> (alph m1 a, alph m2 a, Seq.singleton a)) (foldMap Seq.singleton alphabet)
    go Empty visited = Nothing
    go ((e1, e2, w) :<| todo) visited
      -- If the pair is already visited, we skip it
      | (e1, e2) `Map.member` visited = go todo visited
      -- If the pair shows an inconsistency, return False
      | accept m1 e1 /= accept m2 e2 = Just w
      -- Otherwise, keep searching
      | otherwise = go (todo <> extend (e1, e2) w visited) (Map.insert (e1, e2) w visited)
    -- We could sort this set on length to get shortest counterexamples
    -- For now, we don't do that and keep everything lazy
    extend (e1, e2) w visited =
      (multiplication m1 e1 e1, multiplication m2 e2 e2, w <> w)
        :<| Map.foldMapWithKey
          ( \(f1, f2) v ->
              (multiplication m1 e1 f1, multiplication m2 e2 f2, w <> v)
                :<| (multiplication m1 f1 e1, multiplication m2 f2 e2, v <> w)
                :<| Empty
          )
          visited

searchShortestCounterexample :: (Ord q1, Ord q2) => Set a -> MonoidAcceptor a q1 -> MonoidAcceptor a q2 -> Maybe (Seq a)
searchShortestCounterexample alphabet m1 m2 = go workingSet Map.empty
  where
    workingSet = (unit m1, unit m2, Seq.Empty) :<| fmap (\a -> (alph m1 a, alph m2 a, Seq.singleton a)) (foldMap Seq.singleton alphabet)
    go Empty visited = Nothing
    go ((e1, e2, w) :<| todo) visited
      -- If the pair is already visited, we skip it
      | (e1, e2) `Map.member` visited = go todo visited
      -- If the pair shows an inconsistency, return False
      | accept m1 e1 /= accept m2 e2 = Just w
      -- Otherwise, keep searching
      | otherwise = go (Seq.unstableSortOn (\(_, _, w) -> length w) $ todo <> extend (e1, e2) w visited) (Map.insert (e1, e2) w visited)
    -- We could sort this set on length to get shortest counterexamples
    -- For now, we don't do that and keep everything lazy
    extend (e1, e2) w visited =
      (multiplication m1 e1 e1, multiplication m2 e2 e2, w <> w)
        :<| Map.foldMapWithKey
          ( \(f1, f2) v ->
              (multiplication m1 e1 f1, multiplication m2 e2 f2, w <> v)
                :<| (multiplication m1 f1 e1, multiplication m2 f2 e2, v <> w)
                :<| Empty
          )
          visited

equivalent0 :: (Ord q1, Ord q2) => Set a -> MonoidAcceptor a q1 -> MonoidAcceptor a q2 -> Bool
equivalent0 alphabet m1 m2 = go workingSet Set.empty
  where
    workingSet = (unit m1, unit m2) :<| fmap (\a -> (alph m1 a, alph m2 a)) (foldMap Seq.singleton alphabet)
    go Empty visited = True
    go ((e1, e2) :<| todo) visited
      -- If the pair is already visited, we skip it
      | (e1, e2) `Set.member` visited = go todo visited
      -- If the pair shows an inconsistency, return False
      | accept m1 e1 /= accept m2 e2 = False
      -- Otherwise, keep searching
      | otherwise = go (todo <> extend (e1, e2) visited) (Set.insert (e1, e2) visited)
    extend (e1, e2) visited =
      (multiplication m1 e1 e1, multiplication m2 e2 e2)
        :<| foldMap
          ( \(f1, f2) ->
              (multiplication m1 e1 f1, multiplication m2 e2 f2)
                :<| (multiplication m1 f1 e1, multiplication m2 f2 e2)
                :<| Empty
          )
          visited
