module Okasaki.Chapter5.DequeProperties where

import Okasaki.Chapter5.Deque
import Okasaki.Chapter5.Queue
import Okasaki.Test

import qualified Okasaki.Chapter5.QueueProperties as Q
import qualified Data.List as L

properties :: [Test]
properties = [
    testGroup "BatchedDeque"         $ propertiesFor (empty :: BatchedDeque Int)
  ]

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

propertiesFor :: (Deque d) => (d Int) -> [Test] 
propertiesFor dempty = Q.propertiesFor dempty ++ [
    testProperty "cons"     $ t_cons
  , testProperty "dlast"    $ t_qlast
  , testProperty "dinit"    $ t_qinit
  , testProperty "random"   $ t_random
  ] where
    lempty = []
    cmp qx qy = Q.getAll qx == Q.getAll qy

    t_cons x xs = cmp d l where
        d = cons x $ Q.addAll dempty xs
        l = cons x $ Q.addAll lempty xs

    t_qlast (NonEmpty xs) = d == l where
        d = qlast $ Q.addAll dempty xs
        l = qlast $ Q.addAll lempty xs

    t_qinit (NonEmpty xs) = cmp d l where
        d = qinit $ Q.addAll dempty xs
        l = qinit $ Q.addAll lempty xs

    t_random xs = t_rand action (uncurry cmp) (d, l) where
        d = Q.addAll dempty xs
        l = Q.addAll lempty xs


instance Deque [] where
    cons = (:)
    qlast = last
    qinit = init

