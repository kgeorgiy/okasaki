module Okasaki.Bench where

import Okasaki.LinAlg as LA

import System.Random
import System.CPUTime
import Control.Exception(evaluate)
import Text.Printf
import qualified Data.List as L

randomList :: Int -> IO [Int]
randomList n = do
    xs <- newStdGen >>= evaluate . take n . L.unfoldr (Just . random)
    evaluate $ sum xs
    return xs
    
time f args = do
    evaluate f
    evaluate args
    t <- getCPUTime
    evaluate $ f args
    t' <- getCPUTime
    return (t' - t)

probe :: (Show a) => (a -> IO t) -> (a -> t -> b) -> a -> IO Double
probe prepare run input = do
    arg <- prepare input 
    t <- time (run input) arg
    let tm = fromIntegral t / 1e12
    putStrLn $ printf "%s, %0.3f" (show input) tm
    return tm

probes :: (Show a) => (a -> IO t) -> (a -> t -> b) -> [a] -> IO [Double]
probes prepare run inputs = mapM (probe prepare run) inputs

experiment :: (Show a) => (a -> IO t) -> (a -> t -> b) -> (a -> [Double]) -> [a] -> IO ()
experiment prepare run factors inputs = do
    print $ length inputs
    vY <- probes prepare run inputs
    let (vB, vR) = LA.leastSquaresV (map factors inputs) vY
    printV "vB" $ vB
    print $ LA.moduleV vR

logAbs = map (log . abs)
showV :: [Double] -> String 
showV = L.intercalate ", " . map (printf "%0.3f")

printV :: String -> [Double] -> IO ()
printV name xs = putStrLn $ name ++ " " ++ showV xs

