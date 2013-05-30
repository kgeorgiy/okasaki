module Okasaki.Chapter6.LazyPairingHeap(LazyPairingHeap, Heap(..)) where

import Okasaki.Chapter3.Heap

data LazyPairingHeap a = Empty | Node a (LazyPairingHeap a) (LazyPairingHeap a) deriving Show

instance Heap LazyPairingHeap where
    empty = Empty

    isEmpty Empty = True
    isEmpty _     = False

    insert h v = merge h $ Node v Empty Empty

    findMin (Node x _ _) = x

    deleteMin (Node x a b) = (x, merge a b)

    toList Empty = []
    toList (Node x a b) = x : toList a ++ toList b

    merge Empty h = h
    merge h Empty = h
    merge h1@(Node x1 _ _) h2@(Node x2 _ _)
        |x1 < x2   = link h1 h2
        |otherwise = link h2 h1
        where
            link (Node x Empty b) a = Node x a b
            link (Node x a b) h = Node x Empty (merge (merge h a) b)
