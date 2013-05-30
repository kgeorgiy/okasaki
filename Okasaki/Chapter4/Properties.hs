module Okasaki.Chapter4.Properties(properties) where

import Okasaki.Chapter4.InsertionSort
import Okasaki.Test

import qualified Data.List as L

properties :: [Test]
properties = [
    testProperty "sorted" $ sorted
  ]

sorted :: [Int] -> Bool
sorted xs = insertionSort xs == L.sort xs
