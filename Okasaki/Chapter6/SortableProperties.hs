{-# LANGUAGE FlexibleInstances, RankNTypes, UndecidableInstances #-}
module Okasaki.Chapter6.SortableProperties(Sortable(..), TestSortable(..)) where

import Okasaki.Chapter6.Sortable
import Okasaki.Test

import qualified Data.List as L

instance Ord a => Eq (B Sortable a) where
    (B l) == (B r) = sort l == sort r
sortable :: Sortable s => s a -> B Sortable a
sortable = B

class (Sortable s) => TestSortable s where
    pair :: s Int -> [Int] -> (s Int, [Int])
    pair _ xs = ((fromList xs), (fromList xs)) 

    v :: (Eq a) => s Int -> (forall s. (Sortable s) => s Int -> a) -> [Int] -> Bool
    v w f xs = f h1 == f h2 where 
        (h1, h2) = pair w xs 

    prop :: s Int -> [Test] 
    prop w = [
        testProperty "fromList_sort" $ v w sortable
      , testProperty "add"           $ \x -> v w (sortable . add x)
      ] where
instance Sortable s => TestSortable s where

instance Sortable [] where
    empty = []
    add x xs = if x `elem` xs then xs else L.insert x xs
    sort = id
