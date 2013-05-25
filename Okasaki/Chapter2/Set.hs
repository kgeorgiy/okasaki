module Okasaki.Chapter2.Set where

class ROSet s where
    empty :: s a
    isEmpty :: s a -> Bool 
    size :: s a -> Int
    member :: Ord a => a -> s a -> Bool
    toList :: s a -> [a]

class ROSet s => IOSet s where
    insert :: Ord a => a -> s a -> s a

    fromList :: Ord a => [a] -> s a
    fromList = foldr insert empty 

