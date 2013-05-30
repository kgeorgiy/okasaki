module Okasaki.Chapter5.Properties(properties) where

import Okasaki.Chapter5.Queue
import Okasaki.Chapter5.QueueProperties as QP

import Okasaki.Chapter5.Deque
import Okasaki.Chapter5.DequeProperties as DP

import Okasaki.Chapter5.SplayHeap
import Okasaki.Chapter5.PairingHeap
import Okasaki.Chapter3.HeapProperties as HP

import Okasaki.Test

properties :: [Test]
properties = [
    testGroup "Queue" [
        testGroup "BatchedQueue"        $ QP.prop (QP.empty :: BatchedQueue Int)
  ]
  , testGroup "Deque" [
        testGroup "BatchedDeque"        $ DP.prop (QP.empty :: BatchedDeque Int)
      ]
  , testGroup "Heap" [
        testGroup "SplayHeapNB"         $ HP.prop (HP.empty :: SplayHeapNB Int)
      , testGroup "SplayHeapSB"         $ HP.prop (HP.empty :: SplayHeapSB Int)
      , testGroup "SplayHeap"           $ HP.prop (HP.empty :: SplayHeap Int)
      , testGroup "PairingHeap"         $ HP.prop (HP.empty :: PairingHeap Int)
      , testGroup "BinaryPairingHeap"   $ HP.prop (HP.empty :: BinaryPairingHeap Int)
      ]
  ]
