{-# LANGUAGE FlexibleInstances, RankNTypes, UndecidableInstances #-}
module Okasaki.Chapter3.HeapProperties(Heap(..), TestHeap(..)) where

import Okasaki.Chapter3.Heap
import Okasaki.Test

import qualified Data.List as L
import Control.Arrow(second)

data Action = Insert | FindMin | DeleteMin deriving (Enum, Show)
instance Arbitrary Action where
    arbitrary = elements [Insert .. DeleteMin]

action :: (Heap h) => (Action, Int) -> (h Int, [Int]) -> (h Int, [Int])
action (Insert, x)    (h, l ) = (insert h x, insert l x)
action (FindMin, _)   (h, []) = (h, [])
action (FindMin, _)   (h, l ) = assert "FindMin" (findMin h == findMin l) (h, l)
action (DeleteMin, _) (h, []) = (h, [])
action (DeleteMin, _) (h, l ) = (snd $ deleteMin h, snd $ deleteMin l)

instance Ord a => Eq (B Heap a) where
    (B l) == (B r) = L.sort (toList l) == L.sort (toList r)

heap :: Heap h => h a -> B Heap a
heap = B

class (Heap h) => TestHeap h where
    pair :: h Int -> [Int] -> (h Int, [Int])
    pair _ xs = ((fromList xs), (fromList xs)) 

    v :: (Eq a) => h Int -> (forall h. (Heap h) => h Int -> a) -> [Int] -> Bool
    v w f xs = f h1 == f h2 where 
        (h1, h2) = pair w xs 

    v2 :: (Eq a) => h Int -> (forall h. (Heap h) => h Int -> h Int -> a) -> [Int] -> [Int] -> Bool
    v2 w f xs ys = f x1 y1 == f x2 y2 where 
        (x1, x2) = pair w xs 
        (y1, y2) = pair w ys 

    prop :: h Int -> [Test] 
    prop w = [
        testProperty "fromList_toList" $ v w heap
      , testProperty "isEmpty"  $ v w isEmpty
      , testProperty "insert"   $ \x -> v w (heap . (`insert` x))
      , testProperty "findMin"  $ nonEmpty (v w findMin)
      , testProperty "deleteMin"$ nonEmpty (v w (second heap . deleteMin))
      , testProperty "merge"    $ v2 w ((heap .) . merge)
      , testProperty "random"   $ t_random
      ] where
        t_random = t_rand action (\(l, r) -> heap l == heap r) . pair w

instance Heap h => TestHeap h where
    
instance Heap [] where
    empty = []
    isEmpty = null
    singleton x = [x]
    insert xs x = L.insert x xs
    findMin = head
    deleteMin xs = (head xs, tail xs)
    merge xs ys = L.sort $ xs ++ ys
    fromList = L.sort
    toList = id
