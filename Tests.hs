{-# LANGUAGE CPP #-}
module Main(main) where

import Test.Framework

import Okasaki.Chapter2.ListProperties as C2List(properties)
import Okasaki.Chapter2.TreeProperties as C2Tree(properties)
import Okasaki.Chapter3.HeapProperties as C3Heap(properties)
import Okasaki.Chapter3.RBTreeProperties as C3RBTree(properties)
import Okasaki.Chapter4.SortProperties as C4Sort(properties)
import Okasaki.Chapter5.QueueProperties as C5Queue(properties)
import Okasaki.Chapter5.DequeProperties as C5Deque(properties)
import Okasaki.Chapter5.HeapProperties as C5Heap(properties)
import Okasaki.Chapter6.Properties as C6(properties)

main :: IO ()
main = defaultMain [
    testGroup "Chapter2" [
        testGroup "List" C2List.properties
      , testGroup "Tree" C2Tree.properties
      ]
  , testGroup "Chapter3" [
        testGroup "Heap" C3Heap.properties
      , testGroup "RBTree" C3RBTree.properties
      ]
  , testGroup "Chapter4" [
        testGroup "Sort" C4Sort.properties
      ]
  , testGroup "Chapter5" [
        testGroup "Queue" C5Queue.properties
      , testGroup "Deque" C5Deque.properties
      , testGroup "Heap" C5Heap.properties
      ]
  , testGroup "Chapter6" C6.properties
  ]
