module Okasaki.Chapter6.Properties where

import Okasaki.Chapter6.LazyBatchedQueue as Q
import Okasaki.Chapter6.PhysicistsQueue
import Okasaki.Chapter6.LazyPairingHeap as H
import Okasaki.Chapter6.Sortable as S
import Okasaki.Chapter5.QueueProperties as QP
import Okasaki.Chapter6.SortableProperties as SP
import Okasaki.Chapter3.HeapProperties as HP
import Okasaki.Test

properties :: [Test]
properties = [
    testGroup "Queue" [
        testGroup "LazyBatchedQueue"    $ QP.prop (Q.empty :: LazyBatchedQueue Int)
      , testGroup "PhysicistsQueue"     $ QP.prop (Q.empty :: PhysicistsQueue Int)
      ]
  , testGroup "Sortable" [
        testGroup "RBTree"              $ SP.prop (S.empty :: RBTree Int)
      , testGroup "MergeSortable"       $ SP.prop (S.empty :: MergeSortable Int)
      ]
  , testGroup "Heap" [
        testGroup "LazyPairingHeap"     $ HP.prop (H.empty :: LazyPairingHeap Int)
      ]
  ]
