{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Equivalence
import Examples.Examples
import Monoid
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

data ExMonoid a = forall q. Ord q => ExMonoid {monoid :: MonoidAcceptor a q}

tests :: TestTree
tests =
  testGroup
    "unit tests"
    [ equivalences,
      languages
    ]

equivalences :: TestTree
equivalences =
  testGroup
    "equivalences"
    [ testCase "0 == 0" $ emptyLanguage `shouldBeEquivalentTo` emptyLanguage,
      testCase "0 == 0" $ emptyLanguage `shouldBeEquivalentTo` emptyLanguageConvoluted,
      testCase "0 /= 1" $ emptyLanguage `shouldNotBeEquivalentTo` fullLanguage,
      testCase "upto is union n=0" $ lengthUptoN 0 `shouldBeEquivalentToE` uptoAsUnion 0,
      testCase "upto is union n=1" $ lengthUptoN 1 `shouldBeEquivalentToE` uptoAsUnion 1,
      testCase "upto is union n=2" $ lengthUptoN 2 `shouldBeEquivalentToE` uptoAsUnion 2,
      testCase "upto is union n=3" $ lengthUptoN 3 `shouldBeEquivalentToE` uptoAsUnion 3,
      testCase "intersection" $ (lengthIsN 4 `intersection` lengthIsN 5) `shouldBeEquivalentTo` emptyLanguage,
      testCase "upto and union" $ (lengthUptoN 4 `intersection` lengthUptoN 7) `shouldBeEquivalentTo` lengthUptoN 4,
      testCase "empty word" $ lengthIsN 0 `shouldBeEquivalentTo` singletonLanguage Seq.empty,
      testCase "empty lang" $ emptyLanguage `shouldBeEquivalentTo` finiteLanguage Set.empty
    ]
  where
    shouldBeEquivalentTo x y = equivalent (Set.fromList "ab") x y @?= True
    shouldNotBeEquivalentTo x y = equivalent (Set.fromList "ab") x y @?= False
    shouldBeEquivalentToE x (ExMonoid y) = equivalent (Set.fromList "ab") x y @?= True

    uptoAsUnion 0 = ExMonoid (lengthIsN 0)
    uptoAsUnion n = case uptoAsUnion (n -1) of
      ExMonoid m -> ExMonoid (lengthIsN n `union` m)

languages :: TestTree
languages =
  testGroup
    "languages"
    [ shouldReject "fin lang empty" (finiteLanguage (Set.fromList [])) ["", "a", "b", "abba"],
      shouldAccept "fin lang 1" (finiteLanguage (Set.fromList ["abba"])) ["abba"],
      shouldReject "fin lang 1" (finiteLanguage (Set.fromList ["abba"])) ["aba", "abbab", "babba", ""],
      shouldAccept "fin lang 2" (finiteLanguage (Set.fromList ["abba", "b", "aaa"])) ["abba", "b", "aaa"],
      shouldReject "fin lang 2" (finiteLanguage (Set.fromList ["abba", "b", "aaa"])) ["aba", "abbab", "babba", "", "a", "aa", "aaaa"]
    ]
  where
    shouldAccept name m ls = testGroup (name <> " accepts") [testCase (show w) $ acceptMonoid m w @?= True | w <- ls]
    shouldReject name m ls = testGroup (name <> " rejects") [testCase (show w) $ acceptMonoid m w @?= False | w <- ls]
