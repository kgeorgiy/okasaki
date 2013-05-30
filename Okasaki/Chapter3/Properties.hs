module Okasaki.Chapter3.Properties(properties) where

import Okasaki.Chapter3.LeftistHeap
import Okasaki.Chapter3.LeftistHeapWeight
import Okasaki.Chapter3.BinomialHeap
import Okasaki.Chapter3.BinomialHeapOpt
import Okasaki.Chapter3.ExplicitMinHeap
import Okasaki.Chapter3.HeapProperties as HP

import Okasaki.Chapter3.RBTree
import Okasaki.Chapter2.TreeProperties as TP

import qualified Data.List as L
import Okasaki.Test

properties :: [Test]
properties = [
    testGroup "Heap" [
        testGroup "LeftistHeap"         $ HP.prop (HP.empty :: LeftistHeap       Int)
      , testGroup "LeftistHeapWeight"   $ HP.prop (HP.empty :: LeftistHeapWeight Int)
      , testGroup "BinomialHeap"        $ HP.prop (HP.empty :: BinomialHeap      Int)
      , testGroup "BinomialHeapOpt"     $ HP.prop (HP.empty :: BinomialHeapOpt   Int)
      , testGroup "ExplicitMinHeap"     $ HP.prop (HP.empty :: ExplicitMinHeap LeftistHeap Int)
      ]
  , testGroup "RBTree" [
        testGroup "RBTree"              $ TP.prop (TP.empty :: RBTree Int)
      , testProperty "fromOrdList"      $ t_fromOrdList
      , testGroup "RBTteeShortInsert"   $ TP.prop (TP.empty :: RBTreeShortInsert Int)
      , testGroup "RBTteeFaster"        $ TP.prop (TP.empty :: RBTreeFaster Int)
      , testGroup "RBTteeFastest"       $ TP.prop (TP.empty :: RBTreeFastest Int)
      ]
  ]

t_fromOrdList :: [Int] -> Bool
t_fromOrdList xs = TP.toList (fromOrdList (L.sort xs)) == (L.sort xs)
