module Okasaki.Chapter5.SplayHeap(
    SplayHeap, 
    SplayHeapNB, 
    SplayHeapSB, 
    Heap(..)
) where

import Okasaki.Chapter3.Heap

data SplayHeap a = Empty | Node (SplayHeap a) a (SplayHeap a) deriving Show


instance Heap SplayHeap where
    empty = Empty

    isEmpty Empty = True
    isEmpty _     = False

    insert h v = Node smaller v bigger where 
        (smaller, bigger) = partition v h

    findMin (Node Empty x _) = x
    findMin (Node l _ _) = findMin l

    deleteMin (Node Empty x r) = (x, r)
    deleteMin (Node (Node Empty x' r') x r) = (x', Node r' x r)
    deleteMin (Node (Node l' x' r') x r) = (m, Node l'' x' (Node r' x r)) where
        (m, l'') = deleteMin l'

    toList Empty = []
    toList (Node l x r) = toList l ++ x : toList r

    fromList = foldl insert empty

    merge Empty h = h
    merge (Node l x r) h = Node (merge l s) x (merge r b) where
        (s, b) = partition x h

newtype SplayHeapNB a = SHNB (SplayHeap a) deriving Show
instance Heap SplayHeapNB where
    empty = SHNB empty
    isEmpty (SHNB h) = isEmpty h
    findMin (SHNB h) = findMin h
    toList (SHNB h) = toList h  
    merge (SHNB h1) (SHNB h2) = SHNB $ merge h1 h2

    insert (SHNB h) v = SHNB $ Node (smaller h) v (bigger h) where
        bigger Empty = Empty
        bigger (Node l x r) = if v <= x then Node (bigger l) x r else bigger r
        smaller Empty = Empty
        smaller (Node l x r) = if v <= x then smaller l else Node l x (smaller r) 

    deleteMin (SHNB (Node Empty x r)) = (x, SHNB r)
    deleteMin (SHNB (Node l x r)) = (m, SHNB $ Node l' x r) where 
        (m, l') = deleteMin l

{- Exercise 5.4 -}
newtype SplayHeapSB a = SHSB (SplayHeap a) deriving Show
instance Heap SplayHeapSB where
    empty = SHSB empty
    isEmpty (SHSB h) = isEmpty h
    findMin (SHSB h) = findMin h
    deleteMin (SHSB h) = let (m, h') = deleteMin h in (m, SHSB h')
    toList (SHSB h) = toList h  
    merge (SHSB h1) (SHSB h2) = SHSB $ merge h1 h2

    insert (SHSB h) v = SHSB $ Node (smaller h) v (bigger h) where
        bigger Empty = Empty
        bigger (Node l x r) = if x <= v 
            then bigger r
            else case l of                                            
                Empty -> Node Empty x r
                (Node l' x' r') -> if x' <= v
                    then Node (bigger r') x r
                    else Node (bigger l') x' (Node r' x r)
        smaller Empty = Empty
        smaller (Node l x r) = if v < x        
            then smaller l                     
            else case r of                     
                Empty -> Node l x Empty        
                (Node l' x' r') -> if v < x'
                    then Node l x (smaller l')
                    else Node (Node l x l') x' (smaller r')

partition _ Empty = (Empty, Empty)
partition v n@(Node l x r) = if x <= v
    then case r of 
        Empty -> (n, Empty)
        (Node l' x' r') -> if x' <= v
            then let (s, b) = partition v r' in (Node (Node l x l') x' s, b)
            else let (s, b) = partition v l' in (Node l x s, Node b x' r')
    else case l of 
        Empty -> (Empty, n)
        (Node l' x' r') -> if x' <= v
            then let (s, b) = partition v r' in (Node l' x' s, Node b x r)
            else let (s, b) = partition v l' in (s, Node b x' (Node r' x r))
