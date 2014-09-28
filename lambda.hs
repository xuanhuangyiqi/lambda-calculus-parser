data Lex a = Le | Ri | Lambda | Var a | Col deriving (Show, Eq)
data Tree a = Branch (Tree a) (Tree a) | Node a deriving Show
data Node a = Lam [a] [Node a] | Leaf a | NBranch [Node a] deriving (Show, Eq)
data GTree a = GBranch (GTree a) (GTree a) | GLeaf a | Empty | LBranch [a] (GTree a) deriving Show

-- generating grammar tree

toItems :: [Char] -> [[Char]]
toItems []          = [[]]
toItems (' ':xs)    = []:(toItems xs)
toItems (x:xs)      = (x:(head $ toItems xs)):(tail $ toItems xs)

toLex :: [[Char]] -> [Lex [Char]]
toLex [] = []
toLex ("(":xs)      = Le:(toLex xs)
toLex (")":xs)      = Ri:(toLex xs)
toLex ("lambda":xs) = Lambda:(toLex xs)
toLex (":":xs)      = Col:(toLex xs)
toLex (x:xs)        = (Var x):(toLex xs)

find l r z y x
    | (x == z) && ((fst y) == 0) && ((fst $ snd y) < (snd $ snd y)) = (fst y, ((fst $ snd y) + 1, (fst $ snd y) + 1)) 
    | x == l                                                        = ((fst y) + 1, ((fst $ snd y) + 1, (snd $ snd y)))
    | x == r                                                        = ((fst y) - 1, ((fst $ snd y) + 1, (snd $ snd y)))
    | otherwise                                                     = (fst y, ((fst $ snd y) + 1, snd $ snd y))

toTree :: [Lex [Char]] -> [Node (Lex [Char])]
toTree []               = []
toTree ((Var a):xs)     = (Leaf (Var a)):(toTree xs)
toTree (Le:xs)          = (Leaf Le):(toTree xs)
toTree (Ri:xs)          = (Leaf Ri):(toTree xs)
toTree (Lambda:xs)      = (Lam (take (i-1) xs) (toTree (take (j - i - 1) $ drop i xs))):(toTree $ drop (j-1) xs) where
    (_, (_, i)) = foldl (find Le Ri Col) (0, (0, l+1)) xs
    (_, (_, j)) = foldl (find Le Ri Ri) (0, (0, l+1)) xs
    l = length xs

toLTree :: [Node (Lex [Char])] -> [Node (Lex [Char])]
toLTree []              = []
toLTree ((Leaf Le):xs)  = (NBranch (toLTree $ take (k-1) xs)):(toLTree $ drop k xs) where
    (_, (_, k)) = foldl (find (Leaf Le) (Leaf Ri) (Leaf Ri)) (0, (0, 1+(length xs))) xs
toLTree ((Lam a b):xs)  = (Lam a (toLTree b)):(toLTree xs)
toLTree (x:xs)          = x:(toLTree xs)

nodeToGTree = foldl f Empty where
    f Empty a   = g a
    f a b       = GBranch a (g b)
    g (Lam a b)     = LBranch a (nodeToGTree b)
    g (Leaf a)      = GLeaf a
    g (NBranch a)   = nodeToGTree a

strToGTree = nodeToGTree.toLTree.toTree.toLex.toItems

-- Beta-Reduction
betaReduction :: GTree (Lex [Char]) -> GTree (Lex [Char])
betaReduction (GBranch (LBranch (x:xs) b) c)    = betaReduction $ LBranch xs (replace (betaReduction b) x (betaReduction c))
betaReduction (GBranch a b)                     = betaReduction $ GBranch (betaReduction a) (betaReduction b)
betaReduction (LBranch [] b)                    = betaReduction b
betaReduction (LBranch a b)                     = LBranch a (betaReduction b)
betaReduction (GLeaf a)                         = GLeaf a

replace :: GTree (Lex [Char]) -> (Lex [Char]) -> GTree (Lex [Char]) -> GTree (Lex [Char])
replace (GLeaf a) c d
    | a == c    = d
    | otherwise = GLeaf a
replace (GBranch a b) c d = GBranch (replace a c d) (replace b c d)
replace (LBranch a b) c d
    | c `elem` a    = b
    | otherwise     = LBranch a (replace b c d)

-- GTree output
toStr :: GTree (Lex [Char]) -> [Char]
toStr Empty                     = ""
toStr (GBranch a (GBranch b c)) = (toStr a) ++ " (" ++ (toStr $ GBranch b c) ++ ")"
toStr (GBranch a (LBranch b c)) = (toStr a) ++ " (" ++ (toStr $ LBranch b c) ++ ")"
toStr (GBranch a b)             = (toStr a) ++ " " ++ (toStr b)
toStr (LBranch a b)             = "lambda" ++ (lex2char a) ++ ": " ++ (toStr b)         
toStr (GLeaf (Var a))           = a 

lex2char :: [Lex [Char]] -> [Char]
lex2char [] = ""
lex2char ((Var a):xs)   = " " ++ a ++ (lex2char xs)

