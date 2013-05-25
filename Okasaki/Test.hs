module Okasaki.Test(
    assert, 
    Arbitrary(..),
    elements,
    choose,
    NonEmptyList(..), 
    Test, 
    testGroup, 
    testProperty,
    t_rand
) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

assert :: String -> Bool -> a -> a
assert message statement value = 
    if statement 
    then value
    else error("Assertion failed: " ++ message)

t_rand :: (action -> state -> state) -> (state -> Bool) -> state -> [action] -> Bool
t_rand action check start = check . foldl go start where
        go p a = action a $ assert "check" (check p) p
