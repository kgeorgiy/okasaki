module Okasaki.Bench where

import Okasaki.LinAlg as LA

import System.Random
import System.CPUTime
import Control.Exception(evaluate)
import Text.Printf
import qualified Data.List as L
import System.IO

randomList :: Int -> IO [Int]
randomList n = newStdGen >>= forceList . take n . L.unfoldr (Just . random)

forceList :: [Int] -> IO [Int]
forceList xs = evaluate (sum xs) >> return xs
    
time f args = do
    f' <- evaluate f
    args' <- evaluate args
    t <- getCPUTime
    evaluate $ f' args'
    t' <- getCPUTime
    return (t' - t)

wipe = take 100 $ repeat '\b'

probe :: (Show a) => (a -> IO t) -> (a -> t -> b) -> a -> IO Double
probe prepare run input = do
    arg <- prepare input 
    t <- time (run input) arg
    let tm = fromIntegral t / 1e12
    putStr wipe
    putStr $ printf "    %s -> %0.3f" (show input) tm
    hFlush stdout
    return tm

probes :: (Show a) => (a -> IO t) -> (a -> t -> b) -> [a] -> IO [Double]
probes prepare run inputs = mapM (probe prepare run) inputs

experiment :: (Show a) => String -> (a -> IO t) -> (a -> t -> b) -> (a -> ([String], [Double])) -> [a] -> IO ()
experiment name prepare run factors inputs = do
    putStrLn name
    putStrLn $ "    inputs = " ++ show (length inputs)
    vY <- probes prepare run inputs
    putStr wipe
    putStrLn $ printf "    minTime = %0.3f, maxTime = %0.3f" (minimum vY) (maximum vY)
    let (vB, vR) = LA.leastSquaresV (map (snd . factors) inputs) vY
    printV "    time = " (fst $ factors $ head inputs) vB
--    print $ LA.moduleV vR

logAbs = map (log . abs)
showV :: [Double] -> String 
showV = L.intercalate ", " . map (printf "%0.3f")

printV :: String -> [String] -> [Double] -> IO ()
printV name ns vs = putStrLn $ name ++ " " ++ L.intercalate " + " (map (\(v, n) -> printf {-%0.3f-}"%s" n) $ filter ((limit <) . fst) $ zip vs ns) where
    limit = maximum vs / 3

