module Okasaki.Chapter2.Tree where

import Okasaki.Chapter2.Set

import Data.Maybe
import Data.Functor(fmap)

data Tree a = Leaf | Node {l :: Tree a, x :: a, r :: Tree a} deriving Show

instance ROSet Tree where
    empty = Leaf

    isEmpty Leaf = True
    isEmpty _    = False

    size Leaf = 0
    size (Node l _ r) = size l + 1 + size r

    member _ Leaf = False
    member v (Node l x r)
        | v < x     = member v l
        | v > x     = member v r
        | otherwise = True
    toList Leaf = []
    toList (Node l x r) = toList l ++ x : toList r

instance IOSet Tree where
    insert v n = insert' n where
        insert' Leaf = leaf v
        insert' n@(Node l x r)
            | v < x = setL n $ insert' l
            | x < v = setR n $ insert' r
            | otherwise = n

setL n l = n {l = l}
setR n r = n {r = r}
leaf v = Node Leaf v Leaf

{- Exercise 2.2 -}
newtype TreeShortInsert a = TSI (Tree a)
instance ROSet TreeShortInsert where
    empty = TSI empty
    isEmpty (TSI t) = isEmpty t
    size (TSI t) = size t
    member v (TSI t) = member v t
    toList (TSI t) = toList t
instance IOSet TreeShortInsert where
    insert v (TSI n) = TSI $ insert' n Nothing where
        insert' Leaf c
            | (Just v) == c = Leaf
            | otherwise     = leaf v
        insert' n@(Node l x r) c
            | v < x     = setL n $ insert' l c
            | otherwise = setR n $ insert' r (Just x)

{- Exercise 2.3 -}
newtype TreeCutInsert a = TCI (Tree a)
instance ROSet TreeCutInsert where
    empty = TCI empty
    isEmpty (TCI t) = isEmpty t
    size (TCI t) = size t
    member v (TCI t) = member v t
    toList (TCI t) = toList t
instance IOSet TreeCutInsert where
    insert v (TCI n) = TCI $ fromMaybe n (insert' n) where
        insert' Leaf = return $ leaf v
        insert' n@(Node l x r)
            | v < x = setL n `fmap` insert' l
            | x < v = setR n `fmap` insert' r
            | otherwise = fail "Found"

--{- Exercise 2.4 -}
newtype TreeShortCutInsert a = TCSI (Tree a)
instance ROSet TreeShortCutInsert where
    empty = TCSI empty
    isEmpty (TCSI t) = isEmpty t
    size (TCSI t) = size t
    member v (TCSI t) = member v t
    toList (TCSI t) = toList t
instance IOSet TreeShortCutInsert where
    insert v (TCSI n) = TCSI $ fromMaybe n (insert' n Nothing) where
        insert' Leaf c
            | (Just v) == c = fail "Found"
            | otherwise     = return $ leaf v
        insert' n@(Node l x r) c
            | v < x     = setL n `fmap` insert' l c
            | otherwise = setR n `fmap` insert' r (Just x)

{- Exercise 2.5a -}
complete depth v = iterate (\node -> Node node v node) Leaf !! depth

{- Exercise 2.5b -}
create s v = fst $ create' s where
    create' s
        | s == 0    = (Leaf, Node Leaf v Leaf)
        | odd s     = let (n0, n1) = create' (s `div` 2) in (Node n0 v n0, Node n0 v n1)
        | even s    = let (n0, n1) = create' (s `div` 2 - 1) in (Node n0 v n1, Node n1 v n1)
