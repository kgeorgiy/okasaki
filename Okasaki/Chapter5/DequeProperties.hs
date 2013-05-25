module Okasaki.Chapter5.DequeProperties where

import Okasaki.Chapter5.Deque
import Okasaki.Chapter5.Queue
import qualified Okasaki.Chapter5.QueueProperties as Q

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import qualified Data.List as L

properties :: [Test]
properties = [
    testGroup "BatchedDeque"         $ propertiesFor (empty :: BatchedDeque Int)
  ]

assert :: Bool -> a -> a
assert c v = if c then v else error("Assertion failed")

data Action = Cons | Head | Tail | Snoc | Last | Init deriving (Enum, Show)
instance Arbitrary Action where
    arbitrary = elements [Cons .. Init]

action :: (Deque q) => (q Int, [Int]) -> (Action, Int) -> (q Int, [Int])
action (d, l ) (Cons, x) = (cons x d, cons x l)
action (d, []) (Head, _) = (d, [])
action (d, l ) (Head, _) = assert (qhead d == qhead l) (d, l)
action (d, []) (Tail, _) = (d, [])
action (d, l ) (Tail, _) = (qtail d, qtail l)
action (d, l ) (Snoc, x) = (snoc x d, snoc x l)
action (d, []) (Last, _) = (d, [])
action (d, l ) (Last, _) = assert (qlast d == qlast l) (d, l)
action (d, []) (Init, _) = (d, [])
action (d, l ) (Init, _) = (qinit d, qinit l)

check :: (Deque q) => (q Int, [Int]) -> (q Int, [Int])
check (d, l) = assert (Q.getAll d == Q.getAll l) (d, l)


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

    t_random :: [(Action, Int)] -> [Int] -> Bool
    t_random as xs = uncurry cmp $ foldl (\p a -> check $ action p a) (d, l) as where
        d = Q.addAll dempty xs
        l = Q.addAll lempty xs


instance Deque [] where
    cons = (:)
    qlast = last
    qinit = init

