module Main(main) where

import Okasaki.Chapter2.Properties as C2(properties)
import Okasaki.Chapter3.Properties as C3(properties)
import Okasaki.Chapter4.Properties as C4(properties)
import Okasaki.Chapter5.Properties as C5(properties)
import Okasaki.Chapter6.Properties as C6(properties)

import Test.Framework

main :: IO ()
main = defaultMain [
    testGroup "Chapter2" C2.properties
  , testGroup "Chapter3" C3.properties
  , testGroup "Chapter4" C4.properties
  , testGroup "Chapter5" C5.properties
  , testGroup "Chapter6" C6.properties
  ]
