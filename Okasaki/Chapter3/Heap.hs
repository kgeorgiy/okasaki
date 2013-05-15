module Okasaki.Chapter3.Heap(Heap(..)) where

import Data.List(find)

class Heap h where
    empty :: h a
    isEmpty :: h a -> Bool

    singleton :: Ord a => a -> h a
    singleton v = insert empty v

    insert :: Ord a => h a -> a -> h a
    insert h v = merge h $ singleton v

    findMin :: Ord a => h a -> a
    deleteMin :: Ord a => h a -> (a, h a)
    merge :: Ord a => h a -> h a -> h a

    {- Exercise 3.3 -}
    fromList :: (Heap h, Ord a) => [a] -> h a
    fromList [] = empty
    fromList xs = let (Just [r]) = find ((1 ==) . length) $ iterate pair $ map singleton xs in r where
        pair [] = []
        pair [x] = [x]
        pair (x1:x2:xs) = merge x1 x2 : pair xs
