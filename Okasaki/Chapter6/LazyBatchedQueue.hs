module Okasaki.Chapter6.LazyBatchedQueue(Queue(..), LazyBatchedQueue) where

import Okasaki.Chapter5.Queue 

data LazyBatchedQueue a = BQ Int [a] Int [a]

makeBQ xc xs yc ys
  | xc <= yc  = BQ (xc + yc) (xs ++ reverse ys) 0 []
  | otherwise = BQ xc xs yc ys

instance Queue LazyBatchedQueue where
    empty = BQ 0 [] 0 []

    isEmpty (BQ 0 _ _ _) = True
    isEmpty _ = False
    
    snoc v (BQ xc xs yc ys) = makeBQ xc xs (yc + 1) (v : ys)
    qhead (BQ _ (x:_) _ _) = x
    qtail (BQ xc (_:xs) yc ys) = makeBQ (xc - 1) xs yc ys
    