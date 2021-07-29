{-# LANGUAGE RecordWildCards #-}

module Monoid where

import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Word (Word)
import Prelude hiding (Word)

type Alphabet a = Set.Set a

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

union :: MonoidAcceptor a q1 -> MonoidAcceptor a q2 -> MonoidAcceptor a (q1, q2)
union m1 m2 =
  MonoidAcceptor
    { elements = undefined,
      unit = (unit m1, unit m2),
      multiplication = \(e1, e2) (f1, f2) -> (multiplication m1 e1 f1, multiplication m2 e2 f2),
      accept = \(e1, e2) -> accept m1 e1 || accept m2 e2,
      alph = \a -> (alph m1 a, alph m2 a)
    }

intersection :: MonoidAcceptor a q1 -> MonoidAcceptor a q2 -> MonoidAcceptor a (q1, q2)
intersection m1 m2 =
  MonoidAcceptor
    { elements = undefined,
      unit = (unit m1, unit m2),
      multiplication = \(e1, e2) (f1, f2) -> (multiplication m1 e1 f1, multiplication m2 e2 f2),
      accept = \(e1, e2) -> accept m1 e1 && accept m2 e2,
      alph = \a -> (alph m1 a, alph m2 a)
    }

complement :: MonoidAcceptor a q -> MonoidAcceptor a q
complement m = m {accept = not . accept m}

-- Todo: concatenation and Kleene star
