# Aho-Corasick algorithm in Haskell

`AC.hs`, used as `ac :: [String] -> String -> [String]` standing for `ac :: [Searched Words] -> Text to search -> [Occurences]`. Occurences are listed in order of finding them. This implementation is pure Haskell, no libraries added or needed.

**Example usage:**  
`*AC> ac ["baraba","arab","ar","barabas","ab"] "barabarabas"
["ar","ab","arab","baraba","ar","ab","arab","baraba","barabas"]`  
For words b**ar**abarabas, bar**ab**arabas, b**arab**arabas, **baraba**rabas, barab**ar**abas, barabar**ab**as, barab**arab**as, bara**baraba**s, bara**barabas**.

You can check `test.hs` for more example usage and also use this Haskell script as a test if the algorithm works correctly as it contains some "hard" tests that require backtracking and using the shorts.

Made for a _Haskell&Prolog_ course of nonprocedural programming for Charles University, Prague.

### `KMP.hs`

I also implemented a Knuth-Morris-Pratt algorithm in Haskell, so it is added in `KMP.hs`. Note that I didn't refactor or modified this version too much, it is in a pretty raw and factory-new state.
