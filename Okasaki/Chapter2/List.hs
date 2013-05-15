{- Excercise 2.1 -}
suffixes = foldr (\x ss -> (x : head ss) : ss) [[]]

suffixes' [] = [[]]
suffixes' (x:xs) = (x : head ss) : ss where ss = suffixes xs
