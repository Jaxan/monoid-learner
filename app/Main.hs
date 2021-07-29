{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main where

import Data.Foldable (Foldable (toList))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Equivalence (searchCounterexample)
import Examples.Examples
import KnuthBendix (knuthBendix, rewrite)
import MStar
import Monoid
import Word (Word)
import Prelude hiding (Word)

-- We use the alphabet {a, b} as always
symbols :: Set.Set Char
symbols = Set.fromList "ab"

example :: MonoidAcceptor Char _
example =
  -- 1. the first example I tried:
  mainExample
  -- 2. a* or b*:
  -- predSymbol (== 'a') `union` predSymbol (== 'b')
  -- 3. just the word abba:
  -- finiteLanguage (Set.fromList ["abba"])

-- Let's count the number of membership queries
-- and print all the queries
languageM :: IORef Int -> Word Char -> IO Bool
languageM count w = do
  modifyIORef' count succ
  n <- readIORef count
  let nstr = show n
      wstr = toList w
      r = acceptMonoid example w
  putStr "m "
  putStr nstr
  putStr (replicate (6 - length nstr) ' ')
  putStr wstr
  putStr (replicate (24 - length wstr) ' ')
  print r
  return r

-- Let's count the number of equivalence queries
equiv :: _ => IORef Int -> MonoidAcceptor Char _ -> IO (Maybe (Word Char))
equiv count m = do
  modifyIORef' count succ
  n <- readIORef count
  let nstr = show n
      r = searchCounterexample symbols m example
  putStr "e "
  putStr nstr
  putStr (replicate (6 - length nstr) ' ')
  print r
  return r

logger :: Show a => LogMessage a -> IO ()
logger (NewRow w) = putStr "Adding row " >> print (toList w)
logger (NewContext (l, r)) = putStr "Adding context " >> print (toList l, toList r)
logger (Stage str) = putStr "Stage: " >> putStrLn str

main :: IO ()
main = do
  putStrLn "Welcome to the monoid learner"
  mqCount <- newIORef 0
  eqCount <- newIORef 0
  let lang = languageM mqCount
  let equi = equiv eqCount

  -- Initialise
  s <- initialState symbols lang

  -- learn
  (s2, m2) <- learn logger lang equi s

  -- Hypothesis is now correct
  let sFinal = s2
      mFinal = m2

  -- Print as multiplication table

  putStrLn ""
  putStrLn "Monoid with the elements:"
  putStr "    "
  print (elements mFinal)

  putStrLn "accepting elements:"
  putStr "    "
  print (filter (accept mFinal) $ elements mFinal)

  putStrLn "unit element"
  putStr "    "
  print (unit mFinal)

  putStrLn "multiplication table"
  let multTable = (\x y -> (x, y, multiplication mFinal x y)) <$> elements mFinal <*> elements mFinal
  mapM_
    ( \(x, y, z) -> do
        putStr "    "
        putStr (show x)
        putStr " x "
        putStr (show y)
        putStr " = "
        print z
    )
    multTable

  -- Print as monoid presentation

  let -- Extract the rewrite rules from the table
      -- For this we simply look at products r1 r2 and see which row is equivalent to it
      rowPairs = Set.filter (\w -> not (w `Set.member` rows sFinal)) . Set.map (uncurry (<>)) $ Set.cartesianProduct (rows sFinal) (rows sFinal)
      representatives = Map.fromList (fmap (\w -> (row sFinal w, w)) (Set.toList (rows sFinal)))
      rules0 = Map.fromSet (\w -> representatives Map.! row sFinal w) rowPairs
      rules = Map.toList rules0
      kbRules = knuthBendix rules
      -- Also extract final set
      accRows0 = Set.filter (\m -> row sFinal m Map.! (Seq.empty, Seq.empty)) $ rows sFinal
      accRows = Set.map (rewrite kbRules) accRows0

  putStrLn ""
  putStrLn "Monoid on the generators:"
  putStr "    "
  print (fmap (: []) (toList symbols))

  putStrLn "accepting strings:"
  putStr "    "
  print (fmap toList (toList accRows))

  putStrLn "with equations:"
  mapM_
    ( \(l, r) -> do
        putStr "    "
        putStr (toList l)
        putStr " = "
        putStrLn (toList r)
    )
    kbRules