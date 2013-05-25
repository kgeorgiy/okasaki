{-# LANGUAGE CPP #-}
module Main(main) where

import Test.Framework

import Okasaki.Chapter2.ListProperties as C2List(properties)
import Okasaki.Chapter2.TreeProperties as C2Tree(properties)
import Okasaki.Chapter3.HeapProperties as C3Heap(properties)
import Okasaki.Chapter3.RBTreeProperties as C3RBTree(properties)

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
  ]
