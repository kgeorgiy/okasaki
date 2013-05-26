{- Exercise 4.2 -}
module Okasaki.Chapter4.InsertionSort(insertionSort) where

insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert $ insertionSort xs where
    insert [] = [x]
    insert ys@(y:ys') = if x < y then x : ys else y : insert ys'
