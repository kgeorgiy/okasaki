module Okasaki.Chapter5.Queue(Queue(..), BatchedQueue) where

import Data.List as List

class Queue q where
    empty :: q a
    isEmpty :: q a -> Bool
    snoc :: a -> q a -> q a
    qhead :: q a -> a
    qtail :: q a -> q a
    fromList :: [a] -> q a
    fromList = foldl (flip snoc) empty 
    toList :: q a -> [a]
    toList = List.unfoldr (\q -> if isEmpty q then Nothing else Just (qhead q, qtail q))

data BatchedQueue a = BQ [a] [a] deriving Show

makeBQ [] ys = BQ (reverse ys) []
makeBQ xs ys = BQ xs ys

instance Queue BatchedQueue where
    empty = BQ [] []

    isEmpty (BQ [] []) = True
    isEmpty _ = False
    
    snoc v (BQ xs ys) = makeBQ xs (v : ys)
    qhead (BQ (x:_) _) = x
    qtail (BQ (_:xs) ys) = makeBQ xs ys
    
