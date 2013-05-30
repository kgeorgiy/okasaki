module Okasaki.Chapter6.PhysicistsQueue(Queue(..), PhysicistsQueue) where

import Okasaki.Chapter5.Queue 

data PhysicistsQueue a = PQ [a] Int [[a]] Int [a] deriving Show

makePQ ws xc xss yc ys
    | xc <= yc  = makePQ' (head xss') (xc + yc) xss' 0 []
--    | xc <= yc  = makePQ' xs' (xc + yc) (xs' ++ reverse ys) 0 []
    | otherwise = makePQ' ws xc xss yc ys
  where
    xss' = xss ++ [reverse ys]
--    xs' = force xs
--    makePQ' [] xc xss yc ys = PQ (force xs) xc xs yc ys
    makePQ' [] xc xss yc ys = PQ (head xss') xc xss' yc ys where
        xss' = tail xss
    makePQ' ws xc xss yc ys = PQ ws xc xss yc ys
    force = id

instance Queue PhysicistsQueue where
    empty = PQ [] 0 [] 0 []

    isEmpty (PQ [] _ _ _ _) = True
    isEmpty _ = False
    
    snoc v (PQ ws xc xss yc ys) = makePQ ws xc xss (yc + 1) (v : ys)
    qhead (PQ (w:ws) _ _ _ _) = w
    qtail (PQ (_:ws) xc ((_:xs):xss) yc ys) = makePQ ws (xc - 1) (xs : xss) yc ys
    