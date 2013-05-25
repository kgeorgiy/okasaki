module Okasaki.Chapter3.LeftistHeap(LeftistHeap, Heap(..)) where

import Okasaki.Chapter3.Heap

data LeftistHeap a = Empty | Node Int a (LeftistHeap a) (LeftistHeap a) deriving Show

make x l r = if rank l < rank r then Node (rank l + 1) x r l else Node (rank r + 1) x l r where
    rank Empty = 0
    rank (Node rn _ _ _) = rn

instance Heap LeftistHeap where
    empty = Empty

    isEmpty Empty = True
    isEmpty _     = False

    singleton v = make v Empty Empty
    findMin (Node _ x _ _) = x
    deleteMin (Node _ x l r) = (x, merge l r)

    toList Empty = []
    toList (Node _ x l r) = toList l ++ x : toList r

    merge Empty h = h
    merge h Empty = h
    merge n1@(Node _ x1 l1 r1) n2@(Node _ x2 l2 r2)
        | x1 < x2   = make x1 l1 (merge r1 n2)
        | otherwise = make x2 l2 (merge r2 n1)

{- Exercise 3.2 -}
insertDirect Empty v = singleton v
insertDirect n@(Node _ x l r) v = make (min x v) l (insertDirect r (max x v))

{- Exercise 3.3: See Heap.hs -}
