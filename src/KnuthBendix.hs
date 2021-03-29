module KnuthBendix where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Foldable (Foldable (toList))
import Data.Sequence (Seq, fromList)
import qualified Math.Algebra.Group.StringRewriting as Rew

knuthBendix :: Ord a => [(Seq a, Seq a)] -> [(Seq a, Seq a)]
knuthBendix = fmap (bimap fromList fromList) . Rew.knuthBendix . fmap (bimap toList toList)

rewrite :: Ord a => [(Seq a, Seq a)] -> Seq a -> Seq a
rewrite system = fromList . Rew.rewrite (fmap (bimap toList toList) system) . toList
