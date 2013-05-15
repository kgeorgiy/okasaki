{- Exercise 3.7 -}

module Okasaki.Chapter3.ExplicitMinHeap(ExplicitMinHeap, Heap(..)) where

import Okasaki.Chapter3.Heap

data Heap h => ExplicitMinHeap h a = Empty | Heap a (h a) deriving Show

instance Heap h => Heap (ExplicitMinHeap h) where
    empty = Empty
    isEmpty Empty  = True
    isEmpty _       = False

    insert Empty v = Heap v $ singleton v
    insert (Heap m h) v = Heap (min m v) $ insert h v

    findMin (Heap m _) = m
    deleteMin (Heap m h) = let h' = snd $ deleteMin h in (m, Heap (findMin h) h)
    merge Empty h      = h
    merge h      Empty = h
    merge (Heap m1 h1) (Heap m2 h2) = Heap (min m1 m2) $ merge h1 h2
