{-# LANGUAGE CPP #-}
module Main(main) where

import Test.Framework

import Okasaki.Chapter2.ListProperties as C2List(properties)
import Okasaki.Chapter2.TreeProperties as C2Tree(properties)

main :: IO ()
main = defaultMain [
    testGroup "Chapter2" [
        testGroup "List" C2List.properties
      , testGroup "Tree" C2Tree.properties
    ]
  ]
