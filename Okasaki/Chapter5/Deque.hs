{- Exercise 5.1 -}
module Okasaki.Chapter5.Deque(Deque(..), BatchedDeque) where

import Okasaki.Chapter5.Queue

class Queue d => Deque d where
    cons :: a -> d a -> d a
    qlast :: d a -> a
    qinit :: d a -> d a

data BatchedDeque a = BD [a] [a]

balance xs = (take mid xs, reverse $ drop mid xs) where
    mid = length xs `div` 2

makeBD [] ys = uncurry (flip BD) $ balance ys
makeBD xs [] = uncurry BD $ balance xs
makeBD xs ys = BD xs ys

instance Queue BatchedDeque where
    empty = BD [] []

    isEmpty (BD [] []) = True
    isEmpty _ = False

    snoc v (BD xs ys) = makeBD xs (v : ys)

    qhead (BD (x:_) _) = x
    qhead (BD [] [y]) = y

    qtail (BD (_:xs) ys) = makeBD xs ys
    qtail (BD [] [_]) = empty

instance Deque BatchedDeque where
    cons v (BD xs ys) = makeBD (v : xs) ys

    qlast (BD _ (y:_)) = y
    qlast (BD [x] []) = x

    qinit (BD xs (_:ys)) = makeBD xs ys
    qinit (BD [_] []) = empty
    
