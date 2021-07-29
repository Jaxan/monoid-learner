module Word where

import Data.Maybe (maybeToList)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Prelude hiding (Word)

-- The sequence type has efficient concatenation
-- It provides all the useful instances such as Monoid
type Word a = Seq a

-- Left and Right concats, these are like columns, but they act
-- both left and right. Maybe a better word would be "tests".
type Context a = (Seq a, Seq a)

apply :: Context a -> Word a -> Word a
apply (l, r) w = l <> w <> r

zips :: Word a -> [Context a]
zips = go Empty
  where
    go left Empty = pure (left, Empty)
    go left (h :<| hs) = (left, h :<| hs) : go (left :|> h) hs

leftResidual :: Eq a => Word a -> Word a -> Maybe (Context a)
leftResidual Empty w = Just (Empty, w)
leftResidual _ Empty = Nothing
leftResidual (a :<| as) (b :<| bs)
  | a == b = leftResidual as bs
  | otherwise = Nothing

-- Takes out an infix at all possible places
-- Probably inefficient
infixResiduals :: Eq a => Word a -> Word a -> [Context a]
infixResiduals infx word = [(l, r2) | (l, r) <- zips word, (Empty, r2) <- maybeToList (leftResidual infx r)]
