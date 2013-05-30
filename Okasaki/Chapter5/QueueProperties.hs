{-# LANGUAGE FlexibleInstances, RankNTypes, UndecidableInstances #-}
module Okasaki.Chapter5.QueueProperties(Queue(..), TestQueue(..)) where

import Okasaki.Chapter5.Queue
import Okasaki.Test

instance Eq a => Eq (B Queue a) where
    (B l) == (B r) = toList l == toList r
queue :: Queue q => q a -> B Queue a
queue = B

class (Queue q) => TestQueue q where
    pair :: q Int -> [Int] -> (q Int, [Int])
    pair _ xs = ((fromList xs), (fromList xs)) 

    v :: (Eq a) => q Int -> (forall q. (Queue q) => q Int -> a) -> [Int] -> Bool
    v w f xs = f h1 == f h2 where 
        (h1, h2) = pair w xs 

    prop :: q Int -> [Test] 
    prop w = [
        testProperty "fromList_toList"  $ v w queue
      , testProperty "isEmpty"          $ v w isEmpty
      , testProperty "snoc"             $ \x -> v w (queue . snoc x)
      , testProperty "qhead"            $ nonEmpty $ v w qhead
      , testProperty "qtail"            $ nonEmpty $ v w (queue . qtail)
      ] where
instance Queue q => TestQueue q where

instance Queue [] where
    empty = []
    isEmpty = null 
    snoc x = (++ [x])
    qhead = head
    qtail = tail
