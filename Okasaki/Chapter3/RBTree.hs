module Okasaki.Chapter3.RBTree(
    RBTree(..)
  , ROSet(..)
  , IOSet(..)
  , RBTreeShortInsert
  , RBTreeFaster
  , RBTreeFastest
  , fromOrdList  
) where

import Okasaki.Chapter2.Set

import Data.List hiding (insert)

{- Exercise 3.8 -}
{-
    The number of elements in the RBTree of black height $h$ is greater or equal $2^h-1$.
    The black height of the RBTree containing $n$ elements is less or equal $log(n+1)$.
    Maximum node height in the RBTree of black height $h$ is less or equal $2h$.
    Maximum node height in the RBTree containing $n$ elements is less or equal $2 log(n+1)$.
-}

data Color = R | B deriving Show
data RBTree a = Empty | Node {c :: Color, l :: RBTree a, x :: a, r :: RBTree a} deriving Show

instance ROSet RBTree where
    empty = Empty

    isEmpty Empty= True
    isEmpty _    = False

    size Empty = 0
    size (Node _ l _ r) = size l + 1 + size r

    member _ Empty = False
    member v (Node _ l x r)
        | v < x     = member v l
        | v > x     = member v r
        | otherwise = True

    toList Empty = []
    toList (Node _ l x r) = toList l ++ x : toList r

balance B (Node R (Node R n0 x1 n1) x2 n2) x3 n3 = balance' n0 x1 n1 x2 n2 x3 n3
balance B (Node R n0 x1 (Node R n1 x2 n2)) x3 n3 = balance' n0 x1 n1 x2 n2 x3 n3
balance B n0 x1 (Node R (Node R n1 x2 n2) x3 n3) = balance' n0 x1 n1 x2 n2 x3 n3
balance B n0 x1 (Node R n1 x2 (Node R n2 x3 n3)) = balance' n0 x1 n1 x2 n2 x3 n3
balance c l x r = Node c l x r
balance' n0 x1 n1 x2 n2 x3 n3 = Node R (Node B n0 x1 n1) x2 (Node B n2 x3 n3)

leaf v = Node R Empty v Empty
color c (Node _ l x r) = Node c l x r

instance IOSet RBTree where
    insert v n = color B $ insert' n where
        insert' Empty = leaf v
        insert' n@(Node c l x r)
            | v < x = balance c (insert' l) x r
            | v > x = balance c l x (insert' r)
            | otherwise = n


{- Exercise 2.2 -}
newtype RBTreeShortInsert a = RBTSI (RBTree a)
instance ROSet RBTreeShortInsert where
    empty = RBTSI empty
    isEmpty (RBTSI t) = isEmpty t
    size (RBTSI t) = size t
    member v (RBTSI t) = member v t
    toList (RBTSI t) = toList t
instance IOSet RBTreeShortInsert where
    insert v (RBTSI n) = RBTSI $ color B $ insert' n Nothing where
        insert' Empty cand
            | (Just v) == cand  = Empty
            | otherwise         = leaf v
        insert' n@(Node c l x r) cand
            | v < x     = balance c (insert' l cand) x r
            | otherwise = balance c l x (insert' r (Just x))


{- Excercise 3.9 -}
fromOrdList xs = let (t, _, _) = build xs (length xs) in t where
    build xs 0 = (Empty, 1, xs)
    build (x:xs) 1 = (Node B Empty x Empty, 2, xs)
    {- Size of the red-rooted subtree grows as $2^(2n)$, so no red node has red child -}
    build xs n = (Node B (if lh > rh then color R l else l) x r, rh + 1, xs'') where
        (l, lh, (x:xs')) = build xs mid
        (r, rh, xs'') = build xs' (n - mid - 1)
        mid = n `div` 2


{- Excercise 3.10 -}
newtype RBTreeFaster a = RBTF (RBTree a)
instance ROSet RBTreeFaster where
    empty = RBTF empty
    isEmpty (RBTF t) = isEmpty t
    size (RBTF t) = size t
    member v (RBTF t) = member v t
    toList (RBTF t) = toList t
instance IOSet RBTreeFaster where
    insert v (RBTF n) = RBTF $ color B $ insert' n where
        insert' Empty = leaf v
        insert' n@(Node c l x r)
            | v < x = balancel c (insert' l) x r
            | v > x = balancer c l x (insert' r)
            | otherwise = n

balancel B (Node R (Node R n0 x1 n1) x2 n2) x3 n3 = balance' n0 x1 n1 x2 n2 x3 n3
balancel B (Node R n0 x1 (Node R n1 x2 n2)) x3 n3 = balance' n0 x1 n1 x2 n2 x3 n3
balancel c l x r = Node c l x r

balancer B n0 x1 (Node R (Node R n1 x2 n2) x3 n3) = balance' n0 x1 n1 x2 n2 x3 n3
balancer B n0 x1 (Node R n1 x2 (Node R n2 x3 n3)) = balance' n0 x1 n1 x2 n2 x3 n3
balancer c l x r = Node c l x r

newtype RBTreeFastest a = RBTFT (RBTree a)
instance ROSet RBTreeFastest where
    empty = RBTFT empty
    isEmpty (RBTFT t) = isEmpty t
    size (RBTFT t) = size t
    member v (RBTFT t) = member v t
    toList (RBTFT t) = toList t
instance IOSet RBTreeFastest where
    insert v (RBTFT n) = RBTFT $ color B $ fst $ insert' n where
        insert' Empty = (leaf v, (nobalance, nobalance))
        insert' n@(Node c l x r)
            | v < x = let (t, b) = insert' l in ((fst b) c t x r, (balancell, balancerl))
            | v > x = let (t, b) = insert' r in ((snd b) c l x t, (balancelr, balancerr))
            | otherwise = (n, (nobalance, nobalance))

        balancell B (Node R (Node R n0 x1 n1) x2 n2) x3 n3 = balance' n0 x1 n1 x2 n2 x3 n3
        balancell c l x r = Node c l x r

        balancelr B (Node R n0 x1 (Node R n1 x2 n2)) x3 n3 = balance' n0 x1 n1 x2 n2 x3 n3
        balancelr c l x r = Node c l x r

        balancerl B n0 x1 (Node R (Node R n1 x2 n2) x3 n3) = balance' n0 x1 n1 x2 n2 x3 n3
        balancerl c l x r = Node c l x r

        balancerr B n0 x1 (Node R n1 x2 (Node R n2 x3 n3)) = balance' n0 x1 n1 x2 n2 x3 n3
        balancerr c l x r = Node c l x r

        nobalance c l x r = Node c l x r
