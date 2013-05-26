module Okasaki.Chapter5.PairingHeap(
    PairingHeap
    , BinaryPairingHeap
    , Heap(..)
) where

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

{- Exercise 5.8a -}
data BinaryPairingHeap a 
    = BEmpty 
    | BNode a (BinaryPairingHeap a) (BinaryPairingHeap a)
    deriving Show

toBinary :: PairingHeap a -> BinaryPairingHeap a
toBinary Empty = BEmpty
toBinary h = toBinary' h BEmpty where
    toBinary' (Node x hs) s = BNode x (foldr toBinary' BEmpty hs) s

{- Exercise 5.8b -}

instance Heap BinaryPairingHeap where
    empty = BEmpty

    isEmpty BEmpty = True
    isEmpty _      = False

    insert h v = merge h $ BNode v BEmpty BEmpty

    findMin (BNode x _ _) = x

    deleteMin (BNode a c BEmpty) = (a, mergePairs c) where
        mergePairs BEmpty = BEmpty
        mergePairs (BNode x1 c1 (BNode x2 c2 s2)) = merge
            (merge (BNode x1 c1 BEmpty) (BNode x2 c2 BEmpty))
            (mergePairs s2)
        mergePairs h = h

    toList BEmpty = []
    toList (BNode x c s) = toList c ++ x : toList s

    merge BEmpty h = h
    merge h BEmpty = h
    merge h1@(BNode x1 c1 BEmpty) h2@(BNode x2 c2 BEmpty) = if x1 < x2 
        then BNode x1 (BNode x2 c2 c1) BEmpty
        else BNode x2 (BNode x1 c1 c2) BEmpty
