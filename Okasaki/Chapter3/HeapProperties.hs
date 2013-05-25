module Okasaki.Chapter3.HeapProperties where

import Okasaki.Chapter3.Heap
import Okasaki.Chapter3.LeftistHeap
import Okasaki.Chapter3.LeftistHeapWeight
import Okasaki.Chapter3.BinomialHeap
import Okasaki.Chapter3.BinomialHeapOpt
import Okasaki.Chapter3.ExplicitMinHeap
import Okasaki.Test

import qualified Data.List as L

properties :: [Test]
properties = [
    testGroup "LeftistHeap"         $ propertiesFor (fromList :: [Int] -> LeftistHeap       Int)
  , testGroup "LeftistHeapWeight"   $ propertiesFor (fromList :: [Int] -> LeftistHeapWeight Int)
  , testGroup "BinomialHeap"        $ propertiesFor (fromList :: [Int] -> BinomialHeapOpt   Int)
  , testGroup "BinomialHeapOpt"     $ propertiesFor (fromList :: [Int] -> BinomialHeapOpt   Int)
  , testGroup "ExplicitMinHeap"     $ propertiesFor (fromList :: [Int] -> ExplicitMinHeap LeftistHeap Int)
  ]

data Action = Insert | FindMin | DeleteMin deriving (Enum, Show)
instance Arbitrary Action where
    arbitrary = elements [Insert .. DeleteMin]

action :: (Heap h) => (Action, Int) -> (h Int, [Int]) -> (h Int, [Int])
action (Insert, x)    (h, l )
    | x `elem` l = (h, l) 
    | otherwise  = (insert h x, insert l x)
action (FindMin, _)   (h, []) = (h, [])
action (FindMin, _)   (h, l ) = assert "FindMin" (findMin h == findMin l) (h, l)
action (DeleteMin, _) (h, []) = (h, [])
action (DeleteMin, _) (h, l ) = (snd $ deleteMin h, snd $ deleteMin l)

propertiesFor :: (Heap h) => ([Int] -> h Int) -> [Test] 
propertiesFor fl' = [
    testProperty "fromList_toList" $ t_fromList_toList
  , testProperty "isEmpty"  $ t_isEmpty
  , testProperty "insert"   $ t_insert
  , testProperty "findMin"  $ t_findMin
  , testProperty "deleteMin"$ t_deleteMin
  , testProperty "merge"    $ t_merge
  , testProperty "random"   $ t_random
  ] where
    ll :: [Int] -> [Int] 
    ll = fromList . L.nub 
    fl = fl' . L.nub

    cmp xs ys = (L.sort (toList xs) == L.sort (toList ys)) && (isEmpty xs || findMin xs == findMin ys)

    t_fromList_toList :: [Int] -> Bool
    t_fromList_toList xs = cmp h l where
        h = fl xs
        l = ll xs

    t_isEmpty :: [Int] -> Bool
    t_isEmpty xs = h == l where
        h = isEmpty $ fl xs
        l = isEmpty $ ll xs

    t_insert x xs = x `elem` xs || cmp h l where
        h = insert (fl xs) x
        l = insert (fromList xs :: [Int]) x

    t_findMin (NonEmpty xs) = h == l where
        h = findMin $ fl xs
        l = findMin $ ll xs

    t_deleteMin (NonEmpty xs) = hm == lm && cmp h l where
        (hm, h) = deleteMin $ fl xs
        (lm, l) = deleteMin $ ll xs

    t_merge xs ys = cmp h l where
        h = merge (fl zs) (fl ys)
        l = merge (ll zs) (ll ys)
        zs = filter (not . (`elem` ys)) xs

    t_random xs = t_rand action (uncurry cmp) (h, l) where
        h = fl xs
        l = ll xs

instance Heap [] where
    empty = []
    isEmpty = null
    singleton x = [x]
    insert xs x = if x `elem` xs then xs else L.insert x xs
    findMin = head
    deleteMin xs = (head xs, tail xs)
    merge xs ys = L.sort $ L.nub $ xs ++ ys
    fromList = L.sort . L.nub
    toList = id

