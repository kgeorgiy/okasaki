module Okasaki.Chapter2.Tree where

import Data.Maybe
import Data.Functor(fmap)

data Tree a = Leaf | Node {l :: Tree a, x :: a, r :: Tree a} deriving Show

member Leaf _ = False
member (Node l x r) v
    | v < x = member l v
    | v > x = member r v
    | otherwise = True

setL n l = n {l = l}
setR n r = n {r = r}
leaf v = Node Leaf v Leaf

insert n v = insert' n where
    insert' Leaf = leaf v
    insert' n@(Node l x r)
        | v < x = setL n $ insert' l
        | x < v = setR n $ insert' r
        | otherwise = n

{- Excercise 2.2 -}
insertShort n v = insert' n Nothing where
    insert' Leaf c
        | (Just v) == c = Leaf
        | otherwise     = leaf v
    insert' n@(Node l x r) c
        | v < x     = setL n $ insert' l c
        | otherwise = setR n $ insert' r (Just x)

{- Excercise 2.3 -}
insertCut n v = fromMaybe n (insert' n) where
    insert' Leaf = return $ leaf v
    insert' n@(Node l x r)
        | v < x = setL n `fmap` insert' l
        | x < v = setR n `fmap` insert' r
        | otherwise = fail "Found"

{- Excercise 2.4 -}
insertCutShort n v = fromMaybe n (insert' n Nothing) where
    insert' Leaf c
        | (Just v) == c = fail "Found"
        | otherwise     = return $ leaf v
    insert' n@(Node l x r) c
        | v < x     = setL n `fmap` insert' l c
        | otherwise = setR n `fmap` insert' r (Just x)

{- Excercise 2.5a -}
complete depth v = iterate (\node -> Node node v node) Leaf !! depth

{- Excercise 2.5b -}
create s v = fst $ create' s where
    create' s
        | s == 0    = (Leaf, Node Leaf v Leaf)
        | odd s     = let (n0, n1) = create' (s `div` 2) in (Node n0 v n0, Node n0 v n1)
        | even s    = let (n0, n1) = create' (s `div` 2 - 1) in (Node n0 v n1, Node n1 v n1)
