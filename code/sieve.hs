import System.CPUTime

-- エラトステネスの篩
import Data.Numbers.Primes
import Data.List
import qualified Data.Heap as H
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad

-- ナイーブ版

primes0 :: [Int]
primes0 = 2 : sieve [3,5..]
  where
    sieve (x:xs) = x : sieve (filter (nd x) xs)
-- not divisible
    nd x y = mod y x /= 0

-- リストでキュー版

primes1 :: [Int]
primes1 = 2 : sieve [] [3,5..]
  where
    sieve [] (x:xs) = x : sieve [(3 * x, x)] xs
    sieve npnps@((np,p):nps) xxs@(x:xs) =
      case compare np x of
        LT ->     sieve (insert (np + 2 * p, p) nps) xxs
        EQ ->     sieve (insert (np + 2 * p, p) nps)  xs
        GT -> x : sieve (insert (3 * x, x) npnps) xs

-- ヒープ版

-- @gotoki_no_joe
primes2 :: [Int]
primes2 = 2 : 3 : sieve q0 [5,7..]
  where
    q0 = H.insert (H.Entry 9 6) H.empty
    sieve queue xxs@(x:xs) =
      case compare np x of
        LT ->     sieve queue1 xxs
        EQ ->     sieve queue1  xs
        GT -> x : sieve queue2  xs
      where
        H.Entry np p = H.minimum queue
        queue1 = H.insert (H.Entry (np+p) p) $ H.deleteMin queue
        queue2 = H.insert (H.Entry (x * 3) (x * 2)) queue

-- ベクタ版

-- @gotoki_no_joe
ub = 104729 :: Int -- 上限
primev = V.create (do
  v <- MV.replicate (ub+1) True
  forM_ (takeWhile ((ub >=).(^ 2)) [2..]) (\i -> do -- 上限が定数なら手計算で [2..√ub]
    f <- MV.read v i
    when f (forM_ [i*2,i*3..ub] (\j -> MV.write v j False)))
  return v
  )

primes3 :: [Int]
primes3 = [k | k <- [2..ub], primev V.! k]

main = do
    print (primes !! 9999)
    t0 <- getCPUTime
    print $ take 2000 primes == take 2000 primes0
    t1 <- getCPUTime
    print ("take 2000 primes0", t1 - t0, "filter mod")
    print $ take 10000 primes == take 10000 primes1
    t2 <- getCPUTime
    print ("take 10000 primes1", t2 - t1, "List.insert")
    print $ take 10000 primes == take 10000 primes2
    t3 <- getCPUTime
    print ("take 10000 primes2", t3 - t2, "Heap")
    print $ take 10000 primes == take 10000 primes3
    t4 <- getCPUTime
    print ("take 10000 primes3", t4 - t3, "Vector")

{-
*Main> main
104729
True ("take  2000 primes0",2312500000000,"filter mod")
True ("take 10000 primes1",2671875000000,"List.insert")
True ("take 10000 primes2", 484375000000,"Heap")
True ("take 10000 primes3", 109375000000,"Vector")
-}
