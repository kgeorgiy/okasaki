{- Exercise 3.6 -}

module Okasaki.Chapter3.BinomialHeapOpt(BinomialHeapOpt, Heap(..)) where

import Okasaki.Chapter3.Heap

data Tree a = Node {root :: a, children :: [Tree a]}

instance Show a => Show (Tree a) where
    show (Node x c) = show x ++ ' ' : show c

link n1@(Node x1 c1) n2@(Node x2 c2)
    | x1 <= x2  = Node x1 (n2 : c1)
    | otherwise = Node x2 (n1 : c2)

newtype BinomialHeapOpt a = BinomialHeapOpt [(Int, Tree a)] deriving Show

insert' [] r n = [(r, n)]
insert' ts'@((tr, t):ts) r n
    | tr == r  = insert' ts (r + 1) $ link t n
    | otherwise = (r, n) : ts'

merge' [] ts2 = ts2
merge' ts1 [] = ts1
merge' ts1'@(t1'@(tr1, t1):ts1) ts2'@(t2'@(tr2, t2):ts2)
    | tr1 < tr2 = t1' : merge' ts1 ts2'
    | tr1 > tr2 = t2' : merge' ts1' ts2
    | otherwise = insert' (merge' ts1 ts2) (tr1 + 1) (link t1 t2)

removeMinTree [t] = (snd t, [])
removeMinTree (t:ts) = let (t', ts') = removeMinTree ts in
    if root (snd t) < root t' then (snd t, ts) else (t', t : ts')

instance Heap BinomialHeapOpt where
    empty = BinomialHeapOpt []

    isEmpty (BinomialHeapOpt ts) = null ts

    insert (BinomialHeapOpt ts) v = BinomialHeapOpt $ insert' ts 0 (Node v []) where

    findMin (BinomialHeapOpt ts) = minimum $ map (root . snd) ts
    deleteMin (BinomialHeapOpt ts) = (m, BinomialHeapOpt $ merge' ts' $ zip [0..] $ reverse c) where
        ((Node m c), ts') = removeMinTree ts

    toList (BinomialHeapOpt ts) = concatMap (tl . snd) ts where
        tl (Node r c) = r : concatMap tl c

    merge (BinomialHeapOpt ts1) (BinomialHeapOpt ts2) = BinomialHeapOpt $ merge' ts1 ts2
