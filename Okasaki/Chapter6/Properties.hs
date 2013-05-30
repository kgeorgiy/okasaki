module Okasaki.Chapter6.Properties(properties) where

import Okasaki.Chapter6.LazyBatchedQueue
import Okasaki.Chapter6.PhysicistsQueue
import Okasaki.Chapter5.QueueProperties as QP

import Okasaki.Chapter6.Sortable
import Okasaki.Chapter6.SortableProperties as SP

import Okasaki.Chapter6.LazyPairingHeap
import Okasaki.Chapter3.HeapProperties as HP

import Okasaki.Test

properties :: [Test]
properties = [
    testGroup "Queue" [
        testGroup "LazyBatchedQueue"    $ QP.prop (QP.empty :: LazyBatchedQueue Int)
      , testGroup "PhysicistsQueue"     $ QP.prop (QP.empty :: PhysicistsQueue Int)
      ]
  , testGroup "Sortable" [
        testGroup "RBTree"              $ SP.prop (SP.empty :: RBTree Int)
      , testGroup "MergeSortable"       $ SP.prop (SP.empty :: MergeSortable Int)
      ]
  , testGroup "Heap" [
        testGroup "LazyPairingHeap"     $ HP.prop (HP.empty :: LazyPairingHeap Int)
      ]
  ]
