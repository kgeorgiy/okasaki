module Okasaki.Chapter2.ListProperties(properties) where

import Okasaki.Chapter2.List
import Okasaki.Test

import Data.List(tails)

properties = [
    testProperty "t_suffixes"  $ t_suffixes suffixes
  , testProperty "t_suffixes'" $ t_suffixes suffixes'
  ]

t_suffixes :: ([Int] -> [[Int]]) -> [Int] -> Bool
t_suffixes f xs = f xs == tails xs                                                   
