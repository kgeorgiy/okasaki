module Okasaki.Chapter3.RBTreeProperties where

import Okasaki.Chapter2.Set
import Okasaki.Chapter3.RBTree
import Okasaki.Chapter2.TreeProperties(propertiesFor)
import Okasaki.Test

import qualified Data.List as L

properties = [
    testGroup "RBTree" $ propertiesFor (fromList :: [Int] -> RBTree Int)
  , testProperty "fromOrdList" $ t_fromOrdList
  , testGroup "RBTteeShortInsert" $ propertiesFor (fromList :: [Int] -> RBTreeShortInsert Int)
  , testGroup "RBTteeFaster" $ propertiesFor (fromList :: [Int] -> RBTreeFaster Int)
  , testGroup "RBTteeFastest" $ propertiesFor (fromList :: [Int] -> RBTreeFastest Int)
  ]

t_fromOrdList :: [Int] -> Bool
t_fromOrdList xs = toList (fromOrdList (L.sort xs)) == (L.sort xs)
