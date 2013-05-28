module Main(main) where

import Okasaki.Chapter4.InsertionSort

import Okasaki.Bench

insertionSortExperiment = experiment prepare run factors inputs where
    prepare (n, _) = randomList n
    run (_, k) = sum . take k . insertionSort
    factors (n, k) = map fromIntegral [1, n, k, n * n, n * k, k * k]
    inputs = [(n, k) | n <- powers, k <- powers, k <= n]
    powers = map (1 *) $ takeWhile (10000 >=) $ map floor $ iterate (1.05 *) 5000.0

main = do 
    insertionSortExperiment
