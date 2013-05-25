module Okasaki.Chapter5.QueueProperties where

import Okasaki.Chapter5.Queue
import Okasaki.Test

import qualified Data.List as L

properties :: [Test]
properties = [
    testGroup "BatchedQueue"         $ propertiesFor (empty :: BatchedQueue Int)
  ]

getAll :: (Queue q) => q a -> [a]
getAll q = snd $ head $ dropWhile (not . isEmpty . fst) $
    iterate (\(q, r) -> (qtail q, qhead q : r)) (q, [])

addAll :: (Queue q) => q a -> [a] -> q a
addAll q xs = foldr snoc q xs

propertiesFor :: (Queue q) => (q Int) -> [Test] 
propertiesFor qempty = [
    testProperty "getAdd" $ t_getAdd
  , testProperty "snoc"     $ t_snoc
  ] where
    lempty = []
    cmp qx qy = getAll qx == getAll qy

    t_getAdd xs = cmp q l where
        q = addAll qempty xs
        l = addAll lempty xs

    t_snoc x xs = cmp q l where
        q = snoc x $ addAll qempty xs
        l = snoc x $ addAll lempty xs

instance Queue [] where
    empty = []
    isEmpty = null 
    snoc x = (++ [x])
    qhead = head
    qtail = tail
