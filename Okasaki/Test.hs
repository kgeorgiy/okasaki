{-# LANGUAGE ExistentialQuantification, ConstraintKinds #-}

module Okasaki.Test(
    assert
  , Arbitrary(..)
  , elements
  , choose
  , NonEmptyList(..)
  , Test
  , testGroup
  , testProperty
  , t_rand
  , B(..)
  , nonEmpty
  , distinct
) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import qualified Data.List as L

assert :: String -> Bool -> a -> a
assert message statement value = 
    if statement 
    then value
    else error("Assertion failed: " ++ message)

t_rand :: (action -> state -> state) -> (state -> Bool) -> state -> [action] -> Bool
t_rand action check start = check . foldl go start where
        go p a = action a $ assert "check" (check p) p

data B cls a = forall t. cls t => B (t a)

nonEmpty f (NonEmpty xs) = f xs
distinct f = f . L.nub
