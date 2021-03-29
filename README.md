monoid-learner
==============

Learns the minimal monoid accepting an unknown language through an orcale.
Similar to Lstar, but for monoids instead of automata. The output is a monoid
presentation which is furthermore minimised by the Knuth-Bendix completion.
Only works for regular languages.

[Original](https://gist.github.com/Jaxan/d9bb9e3223e8fe8266fe4fe84d357088)

This algorithm is made more precise and generalised to bimonoids (in the
sense of a finite set with two binary operations) in
[this paper.](https://doi.org/10.1007/978-3-030-71995-1_26)


## To run

Install Haskell and its cabal tool and clone this repo. Then run:

```
cabal run monoid-learner
```


## Example output for the example in `app/Main.hs`:

For the language of non-empty words with an even number of as and a triple
number of bs. Note that the equations tell us that the language is
commutative.

```
Monoid on the generators:
    fromList "ab"
with equations:
    [(fromList "ba",fromList "ab"),(fromList "aaa",fromList "a"),(fromList "aab",fromList "b"),(fromList "bbb",fromList "aa")]
and accepting strings:
    fromList [fromList "aa"]
```

For the language where a occurs on position 3 on the right and the empty
word.

```
... (many membership queries) ...
Monoid on the generators:
    fromList "ab"
with equations:
    [(fromList "bbbb",fromList "bbb"),(fromList "bbba",fromList "bba"),(fromList "bbab",fromList "bab"),(fromList "bbaa",fromList "baa"),(fromList "babb",fromList "abb"),(fromList "baba",fromList "aba"),(fromList "baab",fromList "aab"),(fromList "baaa",fromList "aaa"),(fromList "abbb",fromList "bbb"),(fromList "abba",fromList "bba"),(fromList "abab",fromList "bab"),(fromList "abaa",fromList "baa"),(fromList "aabb",fromList "abb"),(fromList "aaba",fromList "aba"),(fromList "aaab",fromList "aab"),(fromList "aaaa",fromList "aaa")]
and accepting strings:
    fromList [fromList "",fromList "aaa",fromList "aab",fromList "aba",fromList "abb"]
```