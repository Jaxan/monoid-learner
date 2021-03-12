monoid-learner
==============

Learns the minimal monoid accepting an unknown language through an orcale.
Similar to Lstar, but for monoids instead of automata. The output is a monoid
representation which is furthermore minimised by the Knuth-Bendix completion.

[Original](https://gist.github.com/Jaxan/d9bb9e3223e8fe8266fe4fe84d357088)

Output for the example in `app/Main.hs`:

```
Inferred rules: (generators are a, b and the unit)
[(fromList "aaa",fromList "a"),(fromList "aaaa",fromList "aa"),(fromList "aaab",fromList "ab"),(fromList "aaabb",fromList "abb"),(fromList "aab",fromList "b"),(fromList "aabb",fromList "bb"),(fromList "aba",fromList "b"),(fromList "abaa",fromList "ab"),(fromList "abab",fromList "bb"),(fromList "ababb",fromList "aa"),(fromList "abba",fromList "bb"),(fromList "abbaa",fromList "abb"),(fromList "abbab",fromList "aa"),(fromList "abbabb",fromList "b"),(fromList "abbb",fromList "a"),(fromList "abbbb",fromList "ab"),(fromList "ba",fromList "ab"),(fromList "baa",fromList "b"),(fromList "bab",fromList "abb"),(fromList "babb",fromList "a"),(fromList "bba",fromList "abb"),(fromList "bbaa",fromList "bb"),(fromList "bbab",fromList "a"),(fromList "bbabb",fromList "ab"),(fromList "bbb",fromList "aa"),(fromList "bbbb",fromList "b")]
After KB:
[(fromList "ba",fromList "ab"),(fromList "aaa",fromList "a"),(fromList "aab",fromList "b"),(fromList "bbb",fromList "aa")]
```