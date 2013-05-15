module Okasaki.Chapter3.BinomialHeap(BinomialHeap, Heap(..)) where

import Okasaki.Chapter3.Heap

data Tree a = Node {rank :: Int, root :: a, children :: [Tree a]} --deriving Show

instance Show a => Show (Tree a) where
    show (Node _ x c) = show x ++ ' ' : show c

link n1@(Node r x1 c1) n2@(Node _ x2 c2)
    | x1 <= x2  = Node (r + 1) x1 (n2 : c1)
    | otherwise = Node (r + 1) x2 (n1 : c2)

newtype BinomialHeap a = BinomialHeap [Tree a] deriving Show

insert' [] n = [n]
insert' ts'@(t:ts) n
    | rank t == rank n  = insert' ts $ link t n
    | otherwise = n : ts'

merge' [] ts2 = ts2
merge' ts1 [] = ts1
merge' ts1'@(t1:ts1) ts2'@(t2:ts2)
    | rank t1 < rank t2 = t1 : merge' ts1 ts2'
    | rank t1 > rank t2 = t2 : merge' ts1' ts2
    | otherwise         = insert' (merge' ts1 ts2) (link t1 t2)

removeMinTree [t] = (t, [])
removeMinTree (t:ts) = let (t', ts') = removeMinTree ts in
    if root t < root t' then (t, ts) else (t', t : ts')

instance Heap BinomialHeap where
    empty = BinomialHeap []

    isEmpty (BinomialHeap ts) = null ts

    insert (BinomialHeap ts) v = BinomialHeap $ insert' ts (Node 0 v []) where

    {- Exercise 3.5 -}
    findMin (BinomialHeap ts) = minimum $ map root ts

    deleteMin (BinomialHeap ts) = (m, BinomialHeap $ merge' ts' $ reverse c) where
        ((Node _ m c), ts') = removeMinTree ts

    merge (BinomialHeap ts1) (BinomialHeap ts2) = BinomialHeap $ merge' ts1 ts2
