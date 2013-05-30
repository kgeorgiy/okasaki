{-# LANGUAGE FlexibleInstances, RankNTypes, UndecidableInstances #-}
module Okasaki.Chapter5.DequeProperties where

import Okasaki.Chapter5.Deque
import Okasaki.Chapter5.Queue
import Okasaki.Test

import qualified Okasaki.Chapter5.QueueProperties as Q
import qualified Data.List as L

properties :: [Test]
properties = [testGroup "BatchedDeque" $ prop (empty :: BatchedDeque Int)]

data Action = Cons | Head | Tail | Snoc | Last | Init deriving (Enum, Show)
instance Arbitrary Action where
    arbitrary = elements [Cons .. Init]

action :: (Deque q) => (Action, Int) -> (q Int, [Int]) -> (q Int, [Int])
action (Cons, x) (d, l ) = (cons x d, cons x l)
action (Head, _) (d, []) = (d, [])
action (Head, _) (d, l ) = assert "Head" (qhead d == qhead l) (d, l)
action (Tail, _) (d, []) = (d, [])
action (Tail, _) (d, l ) = (qtail d, qtail l)
action (Snoc, x) (d, l ) = (snoc x d, snoc x l)
action (Last, _) (d, []) = (d, [])
action (Last, _) (d, l ) = assert "Last" (qlast d == qlast l) (d, l)
action (Init, _) (d, []) = (d, [])
action (Init, _) (d, l ) = (qinit d, qinit l)

instance Eq a => Eq (B Deque a) where
    (B l) == (B r) = toList l == toList r
deque :: Deque d => d a -> B Deque a
deque = B

class (Deque d) => TestDeque d where
    pair :: d Int -> [Int] -> (d Int, [Int])
    pair _ xs = ((fromList xs), (fromList xs)) 

    v :: (Eq a) => d Int -> (forall d. (Deque d) => d Int -> a) -> [Int] -> Bool
    v w f xs = f h1 == f h2 where 
        (h1, h2) = pair w xs 

    prop :: d Int -> [Test] 
    prop w = Q.prop w ++ [
        testProperty "cons"   $ \x -> v w (deque . cons x)
      , testProperty "qlast"  $ nonEmpty $ v w qlast
      , testProperty "qinit"  $ nonEmpty $ v w (deque . qtail)
      , testProperty "random" $ t_random
      ] where
        t_random = t_rand action (\(l, r) -> deque l == deque r) . pair w

instance Deque d => TestDeque d where

instance Deque [] where
    cons = (:)
    qlast = last
    qinit = init

