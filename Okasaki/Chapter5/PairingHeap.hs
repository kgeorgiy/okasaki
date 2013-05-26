module Okasaki.Chapter5.PairingHeap(PairingHeap, Heap(..)) where

import Okasaki.Chapter3.Heap

data PairingHeap a = Empty | Node a [PairingHeap a] deriving Show

instance Heap PairingHeap where
    empty = Empty

    isEmpty Empty = True
    isEmpty _     = False

    insert h v = merge h $ Node v []

    findMin (Node x _) = x

    deleteMin (Node a hs) = (a, mergePairs hs) where
        mergePairs [] = Empty
        mergePairs [h] = h
        mergePairs (h1:h2:hs) = merge (merge h1 h2) $ mergePairs hs

    toList Empty = []
    toList (Node a hs) = a : concatMap toList hs

    merge Empty h = h
    merge h Empty = h
    merge h1@(Node x1 hs1) h2@(Node x2 hs2) = if x1 < x2 
        then Node x1 (h2 : hs1)
        else Node x2 (h1 : hs2)
