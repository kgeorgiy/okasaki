module Okasaki.Chapter3.HeapProperties where

import Okasaki.Chapter3.Heap
import Okasaki.Chapter3.LeftistHeap
import Okasaki.Chapter3.LeftistHeapWeight
import Okasaki.Chapter3.BinomialHeap
import Okasaki.Chapter3.BinomialHeapOpt
import Okasaki.Chapter3.ExplicitMinHeap

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import qualified Data.List as L

-- newtype Z = ExplicitMinHeap (LeftistHeap Int) Int

properties :: [Test]
properties = [
    testGroup "LeftistHeap"         $ propertiesFor (fromList :: [Int] -> LeftistHeap       Int)
  , testGroup "LeftistHeapWeight"   $ propertiesFor (fromList :: [Int] -> LeftistHeapWeight Int)
  , testGroup "BinomialHeap"        $ propertiesFor (fromList :: [Int] -> BinomialHeapOpt   Int)
  , testGroup "BinomialHeapOpt"     $ propertiesFor (fromList :: [Int] -> BinomialHeapOpt   Int)
  , testGroup "ExplicitMinHeap"     $ propertiesFor (fromList :: [Int] -> ExplicitMinHeap LeftistHeap Int)
  ]

propertiesFor :: (Heap h) => ([Int] -> h Int) -> [Test] 
propertiesFor fl' = [
    testProperty "fromList_toList" $ t_fromList_toList
  , testProperty "isEmpty"  $ t_isEmpty
  , testProperty "insert"   $ t_insert
  , testProperty "findMin"  $ t_findMin
  , testProperty "deleteMin"$ t_deleteMin
  , testProperty "merge"    $ t_merge
  ] where
    ll :: [Int] -> [Int] 
    ll = fromList . L.nub 
    fl = fl' . L.nub

    cmp xs ys = L.sort (toList xs) == L.sort (toList ys) && (isEmpty xs || findMin xs == findMin ys)

    t_fromList_toList :: [Int] -> Bool
    t_fromList_toList xs = cmp t l where
        t = fl xs
        l = ll xs

    t_isEmpty :: [Int] -> Bool
    t_isEmpty xs = t == l where
        t = isEmpty $ fl xs
        l = isEmpty $ ll xs

    t_insert x xs = x `elem` xs || cmp t l where
        t = insert (fl xs) x
        l = insert (fromList xs :: [Int]) x

    t_findMin (NonEmpty xs) = t == l where
        t = findMin $ fl xs
        l = findMin $ ll xs

    t_deleteMin (NonEmpty xs) = tm == lm && cmp t l where
        (tm, t) = deleteMin $ fl xs
        (lm, l) = deleteMin $ ll xs

    t_merge xs ys = cmp t l where
        t = merge (fl zs) (fl ys)
        l = merge (ll zs) (ll ys)
        zs = filter (not . (`elem` ys)) xs

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

