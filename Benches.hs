{-# LANGUAGE TupleSections #-}
module Main(main) where

import Okasaki.Chapter2.Tree
import qualified Okasaki.Chapter2.Set as T

import Okasaki.Chapter3.BinomialHeap(BinomialHeap)
import qualified Okasaki.Chapter3.Heap as H

import Okasaki.Chapter3.RBTree

import Okasaki.Chapter4.InsertionSort
import qualified Okasaki.Chapter5.Queue as Q
import Okasaki.Chapter6.LazyBatchedQueue(LazyBatchedQueue)
import Okasaki.Chapter6.PhysicistsQueue(PhysicistsQueue)
import qualified Okasaki.Chapter6.Sortable as S

import qualified Data.List as L
import Control.Exception(evaluate)

import Okasaki.Bench

triangle xs = [(n, k) | n <- xs, k <- xs, k <= n]
powers from step to = takeWhile (to >=) $ map floor $ iterate (step *) from

factorsNK :: (Int, Int) -> ([String], [Double])
factorsNK (n', k') = let n = fromIntegral n'; k = fromIntegral k' in (
    ["1", "n", "k", "n^2", "nk", "k2"]
  , [ 1 ,  n ,  k ,  n*n ,  n*k,  k*k]
  )

factorsN :: Int -> ([String], [Double])
factorsN n' = let n = fromIntegral n' in (
    ["1", "log n", "n", "n log n", "n^2"]
  , [ 1,   log n,   n,   n*log n ,  n*n ]
  )

treeInsertRandomExperiment :: (T.IOSet t) => String -> t Int -> IO ()
treeInsertRandomExperiment name w = experiment ("Tree.insert random " ++ name) prepare (run w) factorsN inputs where
    prepare n = randomList n
    run :: (T.IOSet t) => t Int -> Int -> [Int] -> t Int
    run _ _ = foldr T.insert T.empty
    inputs = powers 30000 1.05 100000

treeInsertSortedExperiment :: (T.IOSet t) => String -> t Int -> IO ()
treeInsertSortedExperiment name w = experiment ("Tree.insert sorted " ++ name) prepare (run w) factorsN inputs where
    prepare n = randomList n >>= forceList . L.sort
    run :: (T.IOSet t) => t Int -> Int -> [Int] -> t Int
    run _ _ = foldr T.insert T.empty
    inputs = powers 2000 1.1 10000

insertionSortExperiment = experiment "insertionSort" prepare run factorsNK inputs where
    prepare (n, _) = randomList n
    run (_, k) = sum . take k . insertionSort
    inputs = [(n, k) | n <- powers, k <- powers, k <= n]
    powers = map (1 *) $ takeWhile (10000 >=) $ map floor $ iterate (1.05 *) 5000.0

repeatN n f z = iterate f z !! n

queueExperiment :: (Q.Queue q) => String -> q Int -> IO ()
queueExperiment name w = experiment ("Queue " ++ name) (prepare w) run factorsNK inputs where
    prepare :: (Q.Queue q) => q Int -> (Int, Int) -> IO (q Int)
    prepare w (n, _) = randomList 10 >>= return . Q.fromList . take n . cycle
    run (_, k) = Q.qhead . repeatN (k - 1) Q.qtail
    factors (n, k) = map fromIntegral [("1", 1), ("n", n), ("k", k), ("n^2", n * n), ("nk", n * k), ("k^2", k * k)]
    inputs = triangle $ powers 100000 1.2 500000

heapExperiment :: (H.Heap h) => String -> h Int -> IO ()
heapExperiment name w = experiment ("Heap " ++ name) (prepare w) run factorsN inputs where
    prepare :: (H.Heap h) => h Int -> Int -> IO (h Int)
    prepare w n = randomList 10 >>= return . H.fromList . take n . cycle
    run n = repeatN n (`H.insert` 10)
    inputs = powers 50000 1.1 500000

sortableAddExperiment :: (S.Sortable h) => String -> Int -> h Int -> IO ()
sortableAddExperiment name scale w = experiment ("Sortable.add " ++ name) prepare (run w) factorsN inputs where
    prepare n = randomList n
    run :: (S.Sortable h) => h Int -> Int -> [Int] -> h Int
    run _ _ xs = foldr S.add S.empty xs
    inputs = map (scale *) $ powers 30000 1.05 100000

main = do 
    treeInsertRandomExperiment "Tree"   (T.empty :: Tree Int)
    treeInsertSortedExperiment "Tree"   (T.empty :: Tree Int)
    treeInsertRandomExperiment "RBTree" (T.empty :: RBTree Int)
    treeInsertSortedExperiment "RBTree" (T.empty :: RBTree Int)
    insertionSortExperiment
    queueExperiment "BatchedQueue" (Q.empty :: Q.BatchedQueue Int)
    queueExperiment "LazyBatchedQueue" (Q.empty :: LazyBatchedQueue Int)
    queueExperiment "PhysicistsQueue" (Q.empty :: PhysicistsQueue Int)
    heapExperiment "BinomialHeapQueue" (H.empty :: BinomialHeap Int)
    sortableAddExperiment "RBTree"         1 (S.empty :: S.RBTree Int)
    sortableAddExperiment "MergeSortable" 10 (S.empty :: S.MergeSortable Int)
