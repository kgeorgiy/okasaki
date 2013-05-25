module Okasaki.Chapter2.TreeProperties where

import Okasaki.Chapter2.Set
import Okasaki.Chapter2.Tree
import Okasaki.Test

import qualified Data.List as L

data Action = Insert | Member deriving (Enum, Show)
instance Arbitrary Action where
    arbitrary = elements [Insert .. Member]

action :: (IOSet s) => (Action, Int) -> (s Int, [Int]) -> (s Int, [Int])
action (Insert, x) (s, l) = (insert x s, insert x l)
action (Member, x) (s, l) = assert "Member" (member x s == member x l) (s, l)


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
  , testProperty "random"  $ t_random
  ] where
    ll :: [Int] -> [Int] 
    ll = fromList

    cmp t l = toList t == toList l

    t_fromList_toList :: [Int] -> Bool
    t_fromList_toList xs = cmp t l where
        t = fl xs
        l = ll xs

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

    t_insert x xs = cmp t l where
        t = insert x $ fl xs
        l = insert x $ ll xs

    t_random xs = t_rand action (uncurry cmp) (t, l) where
        t = fl xs
        l = ll xs

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
