module Main where

import Data.Foldable (Foldable (toList))
import Data.IORef
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import KnuthBendix
import MStar

-- We use the alphabet {a, b} as always
symbols :: Set.Set Char
symbols = Set.fromList "ab"

-- Example language L = { w | nonempty && even number of as && triple numbers of bs }
language :: MStar.Word Char -> Bool
language w = not (null w) && length aa `mod` 2 == 0 && length bb `mod` 3 == 0
  where
    (aa, bb) = Seq.partition (== 'a') w

-- Let's count the number of membership queries
-- and print all the queries
languageM :: IORef Int -> MStar.Word Char -> IO Bool
languageM count w = do
  modifyIORef' count succ
  n <- readIORef count
  let nstr = show n
  putStr nstr
  putStr (replicate (8 - length nstr) ' ')
  putStrLn $ toList w
  return $ language w

main :: IO ()
main = do
  putStrLn "Welcome to the monoid learner"
  count <- newIORef 0
  let lang = languageM count

  -- Initialise
  s <- initialState symbols lang
  -- make closed, consistent and associative
  s2 <- learn lang s

  -- Above hypothesis is trivial (accepts nothing)
  -- Let's add a column so that aa can be reached
  putStrLn "Adding counterexample aa"
  s3 <- addContext (Seq.empty, Seq.singleton 'a') lang s2
  -- Make closed, consistent and associative again
  s5 <- learn lang s3

  -- Still wrong, on bbb
  -- Let's add a column to reach it
  putStrLn "Adding counterexample bbb"
  s6 <- addContext (Seq.singleton 'b', Seq.singleton 'b') lang s5
  -- Make closed, consistent and associative again
  s7 <- learn lang s6

  -- Hypothesis is now correct
  let sFinal = s7
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

  putStrLn "Monoid on the generators:"
  putStr "    "
  print symbols

  putStrLn "with equations:"
  putStr "    "
  print kbRules

  putStrLn "and accepting strings:"
  putStr "    "
  print accRows
