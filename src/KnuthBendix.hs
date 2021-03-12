module KnuthBendix where

import Data.Sequence (Seq, fromList)
import Data.Foldable (Foldable (toList))
import Data.Bifunctor (Bifunctor (bimap))
import qualified Math.Algebra.Group.StringRewriting as Rew

knuthBendix :: Ord a => [(Seq a, Seq a)] -> [(Seq a, Seq a)] 
knuthBendix = fmap (bimap fromList fromList) . Rew.knuthBendix . fmap (bimap toList toList)
