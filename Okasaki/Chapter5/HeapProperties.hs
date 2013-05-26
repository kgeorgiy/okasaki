module Okasaki.Chapter5.HeapProperties where

import Okasaki.Chapter5.SplayHeap
import Okasaki.Chapter5.PairingHeap
import Okasaki.Chapter3.HeapProperties
import Okasaki.Test

properties :: [Test]
properties = [
    testGroup "SplayHeapNB"       $ prop (empty :: SplayHeapNB Int)
  , testGroup "SplayHeapSB"       $ prop (empty :: SplayHeapSB Int)
  , testGroup "SplayHeap"         $ prop (empty :: SplayHeap Int)
  , testGroup "PairingHeap"       $ prop (empty :: PairingHeap Int)
  , testGroup "BinaryPairingHeap" $ prop (empty :: BinaryPairingHeap Int)
  ]

