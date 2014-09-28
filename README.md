lambda-calculus-parser
======================

A parser for lambda calculus written in Haskell.

# Usage:
```
Prelude> :l lambda.hs

[1 of 1] Compiling Main             ( lambda.hs, interpreted )

Ok, modules loaded: Main.


*Main> let a = strToGTree "( lambda x y : x ) ( lambda z : z ) b"


*Main> betaReduction a

LBranch [Var "z"] (GLeaf (Var "z"))


*Main> toStr $ betaReduction a

"lambda z: z"


*Main> let a = strToGTree "( lambda x y : x ) a b"


*Main> toStr $ betaReduction a

"a"
```
