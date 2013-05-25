module Okasaki.Chapter5.Queue(Queue(..), BatchedQueue) where

class Queue q where
    empty :: q a
    isEmpty :: q a -> Bool
    snoc :: a -> q a -> q a
    qhead :: q a -> a
    qtail :: q a -> q a

data BatchedQueue a = BQ [a] [a]

makeBQ [] ys = BQ (reverse ys) []
makeBQ xs ys = BQ xs ys

instance Queue BatchedQueue where
    empty = BQ [] []

    isEmpty (BQ [] []) = True
    isEmpty _ = False
    
    snoc v (BQ xs ys) = makeBQ xs (v : ys)
    qhead (BQ (x:_) _) = x
    qtail (BQ (_:xs) ys) = makeBQ xs ys
    
