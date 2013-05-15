{- Exercise 3.4b -}
module Okasaki.Chapter3.LeftistHeapWeight(LeftistHeapWeight, Heap(..)) where

import Okasaki.Chapter3.Heap

data LeftistHeapWeight a = Empty | Node Int a (LeftistHeapWeight a) (LeftistHeapWeight a) deriving Show

make x l r = if sl < sr then Node total x r l else Node total x l r where
    sl = size l
    sr = size r
    total = sl + sr + 1

size Empty = 0
size (Node s _ _ _) = s

instance Heap LeftistHeapWeight where
    empty = Empty

    isEmpty Empty = True
    isEmpty _     = False

    singleton v = make v Empty Empty
    findMin (Node _ x _ _) = x
    deleteMin (Node _ x l r) = (x, merge l r)

    {- Exercise 3.4c -}
    merge Empty h = h
    merge h Empty = h
    merge n1@(Node _ x1 l1 r1) n2@(Node _ x2 l2 r2)
        | x1 < x2   = makeMerge x1 l1 r1 n2
        | otherwise = makeMerge x2 l2 r2 n1
        where
            makeMerge x l r1 r2 = if sl < sr then Node total x r l else Node total x l r where
                r = merge r1 r2
                sl = size l
                sr = size r1 + size r2
                total = sl + sr + 1
