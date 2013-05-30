{-# LANGUAGE FlexibleInstances, RankNTypes, UndecidableInstances #-}
module Okasaki.Chapter2.TreeProperties(ROSet(..), IOSet(..), TestTree(..)) where

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

instance Eq a => Eq (B IOSet a) where
    (B l) == (B r) = toList l == toList r

tree :: IOSet t => t a -> B IOSet a
tree = B

class (IOSet t) => TestTree t where
    pair :: t Int -> [Int] -> (t Int, [Int])
    pair _ xs = ((fromList xs), (fromList xs)) 

    v :: (Eq a) => t Int -> (forall t. (IOSet t) => t Int -> a) -> [Int] -> Bool
    v w f xs = f h1 == f h2 where 
        (h1, h2) = pair w xs 

    prop :: t Int -> [Test] 
    prop w = [
        testProperty "fromList_toList" $ v w tree
      , testProperty "isEmpty"  $ v w isEmpty
      , testProperty "size"     $ v w size
      , testProperty "member"   $ \x -> v w (member x)
      , testProperty "insert"   $ \x -> v w (tree . (insert x))
      , testProperty "random"   $ t_random
      ] where
        t_random = t_rand action (\(l, r) -> tree l == tree r) . pair w
instance IOSet t => TestTree t where

instance ROSet [] where
    empty = []
    isEmpty = null
    size = length
    member = elem
    toList = id

instance IOSet [] where
    insert x xs = if x `elem` xs then xs else L.insert x xs
