{-# LANGUAGE LambdaCase #-}

module Examples.Examples where

import Data.Semigroup (Max (..), Semigroup (..))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Monoid (MonoidAcceptor (..), complement, intersection)
import Word (Word)
import Prelude hiding (Word)

emptyLanguage :: MonoidAcceptor a ()
emptyLanguage = MonoidAcceptor [()] () (\a b -> ()) (const False) (const ())

emptyLanguageConvoluted :: MonoidAcceptor a Bool
emptyLanguageConvoluted = MonoidAcceptor [False, True] True (const not) (const False) (const False)

fullLanguage :: MonoidAcceptor a ()
fullLanguage = MonoidAcceptor [()] () (\a b -> ()) (const True) (const ())

-- accepts all words of length exactly n
lengthIsN :: Int -> MonoidAcceptor a Int
lengthIsN n = MonoidAcceptor [0 .. n + 1] 0 (saturate (n + 1) (+)) (== n) (const 1)

-- accepts all words of length at most n
lengthUptoN :: Int -> MonoidAcceptor a Int
lengthUptoN n = MonoidAcceptor [0 .. n + 1] 0 (saturate (n + 1) (+)) (<= n) (const 1)

emptyWord :: MonoidAcceptor a Bool
emptyWord =
  MonoidAcceptor
    { elements = [False, True],
      unit = True,
      multiplication = (&&),
      accept = id,
      alph = const False
    }

-- accepts exactly the given word (not a minimal monoid)
singletonLanguage :: Eq a => Word a -> MonoidAcceptor a (Word a)
singletonLanguage w =
  MonoidAcceptor
    { elements = undefined,
      unit = Seq.empty,
      multiplication = saturateW bound (<>),
      accept = (==) w,
      alph = Seq.singleton
    }
  where
    bound = Seq.length w + 1

finiteLanguage :: Ord a => Set.Set (Word a) -> MonoidAcceptor a (Word a)
finiteLanguage ws =
  MonoidAcceptor
    { elements = undefined,
      unit = Seq.empty,
      multiplication = saturateW bound (<>),
      accept = (`Set.member` ws),
      alph = Seq.singleton
    }
  where
    bound = 1 + getMax (foldMap (Max . length) ws)

predSymbol :: (a -> Bool) -> MonoidAcceptor a Bool
predSymbol pred =
  MonoidAcceptor
    { elements = [False, True],
      unit = True,
      multiplication = (&&),
      accept = id,
      alph = pred
    }

-- requires count >= 2
modnumber :: (a -> Bool) -> Int -> MonoidAcceptor a Int
modnumber pred count =
  MonoidAcceptor
    { elements = [0 .. count -1],
      unit = 0,
      multiplication = \x y -> (x + y) `mod` count,
      accept = (== 0),
      alph = \a -> if pred a then 1 else 0
    }

evenAs :: MonoidAcceptor Char Int
evenAs = modnumber (== 'a') 2

mod3Bs :: MonoidAcceptor Char Int
mod3Bs = modnumber (== 'b') 3

mainExample :: MonoidAcceptor Char ((Int, Int), Bool)
mainExample = evenAs `intersection` mod3Bs `intersection` complement emptyWord

-- Helper functions --
saturate :: Int -> (Int -> Int -> Int) -> Int -> Int -> Int
saturate bound op x y =
  let r = x `op` y
   in if r >= bound
        then bound
        else r

saturateW :: Int -> (Word a -> Word a -> Word a) -> Word a -> Word a -> Word a
saturateW l op x y =
  let r = x `op` y
   in if length r >= l
        then Seq.take l r
        else r
