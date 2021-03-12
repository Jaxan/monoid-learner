module Main where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import KnuthBendix
import MStar

-- We use the alphabet {a, b} as always
alphabet :: Set.Set Char
alphabet = Set.fromList "ab"

-- Example language L = { w | nonempty && even number of as && triple numbers of bs }
language :: MStar.Word Char -> Bool
language w = not (null w) && length aa `mod` 2 == 0 && length bb `mod` 3 == 0
  where
    (aa, bb) = Seq.partition (== 'a') w

main :: IO ()
main = do
  let -- Initialise
      s = initialState alphabet language
      -- make closed, consistent and associative
      (_, s2) = learn language s
      -- The corresponding hypothesis is wrong on the string bbb
      -- So we add a row bb
      s3 = addRow (Seq.fromList "bb") language s2
      -- Make closed, consistent and associative again
      (_, s4) = learn language s3
      -- Extract the rewrite rules from the table
      -- For this we simply look at products r1 r2 and see which row is equivalent to it
      rowPairs = Set.filter (\w -> not (w `Set.member` rows s4)) . Set.map (uncurry (<>)) $ Set.cartesianProduct (rows s4) (rows s4)
      representatives = Map.fromList (fmap (\w -> (row s4 w, w)) (Set.toList (rows s4)))
      rules0 = Map.fromSet (\w -> representatives Map.! row s4 w) rowPairs
      rules = Map.toList rules0

  putStrLn "Inferred rules: (generators are a, b and the unit)"
  print rules

  putStrLn "After KB:"
  print (knuthBendix rules)
