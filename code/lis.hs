import Data.Array

import qualified Data.Vector.Unboxed.Mutable as MUV
import Control.Monad
import Control.Monad.ST

import Debug.Trace

longestIncreasingSubsequence :: (Ord a) => [a] -> (Int,[a]) -- (長さ,lis)
longestIncreasingSubsequence as = (len, arrN ! len)
  where
    n = length as
    arr0 = listArray (0, n) $ [] : replicate n []
    (len, arrN) = foldr step (0, arr0) as
    step ai larr@(l,arr)
      | gt ai (arr ! k1) = (max l k1, arr // [(k1, ai : (arr ! k))])
      | otherwise = larr
      where
        (_,k) = binarySearch ((ai <) . head . (arr !)) (succ l) 0
        k1 = succ k
    gt _ [] = True
    gt x (y:_) = x > y

-- @gotoki_no_joe
binarySearch :: (Int -> Bool) -> Int -> Int -> (Int, Int)
binarySearch prop unsat sat = loop unsat sat
  where
    loop a b
      | ende   = (a, b)
      | prop m = loop a m
      | True   = loop m b
      where
        ende = a == m || b == m
        m = div (a + b) 2

-- @gotoki_no_joe
binarySearchM :: Monad m => (Int -> m Bool) -> Int -> Int -> m (Int, Int)
binarySearchM prop unsat sat = loop unsat sat
  where
    loop a b = do
      let m = div (a + b) 2
      if a == m || b == m then return (a,b) else do
        f <- prop m
        if f then loop a m else loop m b

lisLen :: (Ord a, MUV.Unbox a, Bounded a) => [a] -> Int
--lisLen :: String -> Int
lisLen xs = runST $
  do
    vec <- MUV.replicate (succ n) maxBound
    MUV.write vec 0 minBound
    foldM (step vec) 0 xs
  where
    n = length xs
    step vec len x = do
      (j,k) <- binarySearchM (prop vec x) (-1) (succ len)
      y <- MUV.read vec j
      if x <= y then return len else do
        MUV.write vec k x
        return $ max len k
    prop vec x i = do
      v <- MUV.read vec i
      return $ x < v
-- STモナドの中にあるmutable vectorへのアクセスを含む計算を、
-- binarySearchのpropに渡すことができない？
-- もう一度runSTで包むことで可能？無理でした。

--lisLenA :: (Ord a, Bounded a, Show a) => [a] -> Int
--lisLen :: String -> Int
lisLenA xs = lenN
  where
    n = length xs
    arr0 = listArray (0,n) $ minBound : replicate n maxBound
    (arrN,lenN) = foldl step (arr0, 0) xs
    step (arr, len) x
--      | trace (show (arr, len, x, k)) False = undefined
      | arr ! j < x = (arr // [(k, x)], max len k)
      | otherwise   = (arr, len)
      where
        (j,k) = binarySearch ((x <) . (arr !)) (-1) (succ len)

-- longuest increasing or decreasing sequence
lids :: (a->a->Bool) -> [a] -> (Int, [a]) -- length, sequence
lids lt as = (len, arrN ! len)
  where
    n = length as
    arr0 = listArray (0,n) $ replicate (succ n) []
    (len, arrN) = foldr step (0, arr0) as
    step ai larr@(l,arr)
      | gt ai (arr ! j) = (max l j, arr // [(j, ai : (arr ! k))])
      | otherwise = larr
      where
        (j,k) = binarySearch (lt ai . head . (arr !)) (succ l) 0
    gt _ [] = True
    gt x (y:_) = lt y x
