module Okasaki.Chapter2.Properties(properties) where

import Okasaki.Chapter2.List

import Okasaki.Chapter2.Tree
import Okasaki.Chapter2.TreeProperties as TP

import qualified Data.List as L
import Okasaki.Test

properties :: [Test]
properties = [
    testGroup "List" [
        testProperty "t_suffixes"  $ t_suffixes suffixes1
      , testProperty "t_suffixes'" $ t_suffixes suffixes2
      , testProperty "t_suffixes'" $ t_suffixes suffixes3
      ]
  , testGroup "Tree" [
        testGroup "Tree" $ TP.prop (TP.empty :: Tree                 Int)
      , testGroup "TSI"  $ TP.prop (TP.empty :: TreeShortInsert      Int)
      , testGroup "TCI"  $ TP.prop (TP.empty :: TreeCutInsert        Int)
      , testGroup "TSCI" $ TP.prop (TP.empty :: TreeShortCutInsert   Int)
      , testProperty "complete" $ t_complete
      , testProperty "create" $ t_create
      ]
  ]

t_suffixes :: (List Int -> List (List Int)) -> [Int] -> Bool
t_suffixes f xs = (fromL $ mapL fromL $ f $ toL xs) == L.tails xs

t_complete :: Int -> Int -> Bool
t_complete n v = toList (complete (n `mod` 10) v) == take (2 ^ (n `mod` 10) - 1) (cycle [v])

t_create :: Int -> Int -> Bool
t_create n v = toList (create (n `mod` 1000) v) == take (n `mod` 1000) (cycle [v])
