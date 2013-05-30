{- Exercise 2.1 -}
module Okasaki.Chapter2.List where

data List a = Nil | Cons !a !(List a) deriving Show

headL (Cons x _) = x
tailL (Cons _ xs) = xs

lastL (Cons x Nil) = x
lastL (Cons _ xs) = lastL xs

foldrL f z Nil = z
foldrL f z (Cons x xs) = f x $ foldrL f z xs

toL = foldr Cons Nil
fromL = foldrL (:) []

lengthL = foldrL (const (1 +)) 0

mapL f = foldrL (Cons . f) Nil

suffixes1 = foldrL (\x ss -> Cons (Cons x $ headL ss) ss) $ Cons Nil Nil

suffixes2 Nil = Cons Nil Nil
suffixes2 xs'@(Cons _ xs) = Cons xs' $ suffixes2 xs

suffixes3 Nil = Cons Nil Nil
suffixes3 (Cons x xs) = Cons (Cons x $ headL ss) ss where ss = suffixes3 xs
