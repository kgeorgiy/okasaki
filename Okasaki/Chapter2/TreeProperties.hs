module Okasaki.Chapter2.TreeProperties where

import Okasaki.Chapter2.Set
import Okasaki.Chapter2.Tree

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import qualified Data.List as L

properties = [
    testGroup "Tree" $ propertiesFor (fromList :: [Int] -> Tree                 Int)
  , testGroup "TSI"  $ propertiesFor (fromList :: [Int] -> TreeShortInsert      Int)
  , testGroup "TCI"  $ propertiesFor (fromList :: [Int] -> TreeCutInsert        Int)
  , testGroup "TSCI" $ propertiesFor (fromList :: [Int] -> TreeShortCutInsert   Int)
  , testProperty "complete" $ t_complete
  , testProperty "create" $ t_create
  ]


propertiesFor :: (IOSet s) => ([Int] -> s Int) -> [Test] 
propertiesFor fl = [
    testProperty "fromList_toList" $ t_fromList_toList
  , testProperty "isEmpty" $ t_isEmpty
  , testProperty "size"    $ t_size
  , testProperty "member"  $ t_member
  , testProperty "insert"  $ t_insert
  ] where
    ll :: [Int] -> [Int] 
    ll = fromList

    t_fromList_toList :: [Int] -> Bool
    t_fromList_toList xs = t == l where
        t = toList $ fl xs
        l = toList $ ll xs

    t_isEmpty :: [Int] -> Bool
    t_isEmpty xs = t == l where
        t = isEmpty $ fl xs
        l = isEmpty $ ll xs

    t_size xs = t == l where
        t = size $ fl xs
        l = size $ ll xs

    t_member x xs = t == l where
        t = member x $ fl xs
        l = member x $ ll xs

    t_insert x xs = t == l where
        t = toList $ insert x $ fl xs
        l = toList $ insert x $ ll xs

instance ROSet [] where
    empty = []
    isEmpty = null
    size = length
    member = elem
    toList = id

instance IOSet [] where
    insert x xs = if x `elem` xs then xs else L.insert x xs

t_complete :: Int -> Int -> Bool
t_complete n v = toList (complete (n `mod` 10) v) == take (2 ^ (n `mod` 10) - 1) (cycle [v])


t_create :: Int -> Int -> Bool
t_create n v = toList (create (n `mod` 1000) v) == take (n `mod` 1000) (cycle [v])
