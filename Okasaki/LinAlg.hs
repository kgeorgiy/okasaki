module Okasaki.LinAlg where

import qualified Data.List as L

{- Start of code taken from http://hpaste.org/75404 -}
type Matrix = [[Double]]

msize :: Matrix -> Int
msize = length

mapMatrix :: (Double -> Double) -> Matrix -> Matrix
mapMatrix f = map (map f)

coords :: Matrix -> [[(Int, Int)]]
coords = zipWith (map . (,)) [0..] . map (zipWith const [0..])

delmatrix :: Int -> Int -> Matrix -> Matrix
delmatrix i j = dellist i . map (dellist j)
  where
    dellist i xs = take i xs ++ drop (i + 1) xs

determinant :: Matrix -> Double
determinant m
    | msize m == 1 = head (head m)
    | otherwise    = sum $ zipWith addition [0..] m
  where
    addition i (x:_) =  x * cofactor i 0 m

cofactor :: Int -> Int -> Matrix -> Double
cofactor i j m = ((-1.0) ** fromIntegral (i + j)) * determinant (delmatrix i j m)

cofactorM :: Matrix -> Matrix
cofactorM m = map (map (\(i,j) -> cofactor j i m)) $ coords m

inverse :: Matrix -> Matrix
inverse m = mapMatrix (* recip det) $ cofactorM m
  where
    det = determinant m
{- End of code taken from http://hpaste.org/75404 -}

multiplyM :: Matrix -> Matrix -> Matrix
multiplyM a b = map (\xs -> map (sum . zipWith (*) xs) (L.transpose b)) a 

type Vector = [Double]

multiplyV :: Matrix -> Vector -> Vector
multiplyV a v = map (sum . zipWith (*) v) a 

average :: Vector -> Double
average xs = sum xs / fromIntegral (length xs)

normV :: Vector -> Vector
normV xs = map (/ average xs) xs

normM :: Matrix -> Matrix
normM = L.transpose . map normV . L.transpose

moduleV :: Vector -> Double
moduleV = sum . map (^2)

leastSquares :: [a] -> (a -> Vector) -> (a -> Double) -> (Vector, Vector)
leastSquares inputs factors value = leastSquaresV (map factors inputs) (map value inputs)

leastSquaresV :: Matrix -> Vector -> (Vector, Vector)
leastSquaresV mX vY = (vB, vR) where
    mX' = normM mX
    vY' = normV vY
    vB = multiplyV (multiplyM (inverse (multiplyM (L.transpose mX') mX')) 
                              (L.transpose mX')) vY'
    vR = zipWith (-) (multiplyV mX' vB) vY'
