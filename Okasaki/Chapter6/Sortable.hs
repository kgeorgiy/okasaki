module Okasaki.Chapter6.Sortable(Sortable(..), T.RBTree, MergeSortable) where

import qualified Okasaki.Chapter3.RBTree as T

class Sortable s where
    empty :: s a
    add :: Ord a => a -> s a -> s a
    sort :: Ord a => s a -> [a]
    fromList :: Ord a => [a] -> s a
    fromList = foldr add empty

instance Sortable T.RBTree where
    empty = T.empty
    add = T.insert
    sort = T.toList

merge [] ys = ys
merge xs [] = xs
merge xs'@(x:xs) ys'@(y:ys) = case compare x y of
    LT -> x : merge xs ys'
    EQ -> x : merge xs ys
    GT -> y : merge xs' ys

data MergeSortable a = MergeSortable !Int [[a]] deriving Show

instance Sortable MergeSortable where
    empty = MergeSortable 0 []
    add v (MergeSortable size xss) = MergeSortable (size + 1) $ add' size [v] xss where
        add' size vs xss
            | size `mod` 2 == 0 = vs : xss
            | otherwise         = add' (size `div` 2) (merge vs (head xss)) (tail xss)
    sort (MergeSortable _ xss) = foldl merge [] xss
