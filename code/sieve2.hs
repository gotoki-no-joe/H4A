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
    nd x y = mod y x /= 0

-- modしない版

primes1 :: [Int]
primes1 = 2 : 3 : sieve [6,9..] [5,7..]
  where
    sieve (x:xs) (p:ps) =
      case compare x p of
        LT -> sieve xs (p:ps)
        EQ -> sieve xs ps
        GT -> p : sieve (x : merge xs [p*2,p*3..]) ps
    merge (x:xs) (y:ys) =
      case compare x y of
        LT -> x : merge xs (y:ys)
        EQ -> x : merge xs ys
        GT -> y : merge (x:xs) ys

-- リストでキュー版

primes2 :: [Int]
primes2 = 2 : sieve [] [3,5..]
  where
    sieve ((np,p2):nps) (x:xs) | np <  x =     sieve (insert (np+p2,p2) nps) (x:xs)
                               | np == x =     sieve (insert (np+p2,p2) nps)    xs
    sieve          nps  (x:xs)           = x : sieve (insert (x*x, x+x) nps)    xs

-- ヒープ版

-- @gotoki_no_joe
primes3 :: [Int]
primes3 = 2 : 3 : sieve q0 [5,7..]
  where
    q0 = H.insert (H.Entry 9 6) H.empty
    sieve queue xxs@(x:xs) =
      case compare np x of
        LT ->     sieve queue1 xxs
        EQ ->     sieve queue1  xs
        GT -> x : sieve queue2  xs
      where
        H.Entry np p2 = H.minimum queue
        queue1 = H.insert (H.Entry (np+p2) p2) $ H.deleteMin queue
--        queue1 = H.adjustMin (const (H.Entry (np+p2) p2)) queue
        queue2 = H.insert (H.Entry (x * x) (x * 2)) queue

-- ベクタ版

-- @gotoki_no_joe
ub = 104729 :: Int -- 上限
primev = V.create (do
  v <- MV.replicate (ub+1) True
  forM_ (takeWhile ((ub >=).(^ 2)) [2..]) (\i -> do -- 上限が定数なら手計算で [2..√ub]
    f <- MV.read v i
    when f (forM_ [i*i,i*succ i..ub] (\j -> MV.write v j False)))
  return v
  )

primes4 :: [Int]
primes4 = [k | k <- [2..ub], primev V.! k]

main = do
    print (primes !! 9999)
    run (take  2000 primes == take  2000 primes0) "take  2000 primes0 (filter mod)"
    run (take  2000 primes == take  2000 primes1) "take  2000 primes1 (mergeing sieve)"
    run (take 10000 primes == take 10000 primes2) "take 10000 primes2 (list queue)"
    run (take 10000 primes == take 10000 primes3) "take 10000 primes3 (Heap)"
    run (take 10000 primes == take 10000 primes4) "take 10000 primes4 (Vector)"

run v s = do
  t0 <- getCPUTime
  print v
  t1 <- getCPUTime
  print (t1 - t0, s)

{-
*Main> main
104729
True ("take  2000 primes0",2312500000000,"filter mod")
True ("take 10000 primes1",2671875000000,"List.insert")
True ("take 10000 primes2", 484375000000,"Heap")
True ("take 10000 primes3", 109375000000,"Vector")

*3を*xに修正した後
104729
True ("take 2000 primes0",2421875000000,"filter mod")
True ("take 10000 primes1",187500000000,"List.insert")
True ("take 10000 primes2",328125000000,"Heap")
True ("take 10000 primes3",125000000000,"Vector")

primes1を「modしない版」にして、続きを後ろに下げた
*Main> main
104729
True (2343750000000,"take  2000 primes0 (filter mod)")
True (5750000000000,"take  2000 primes1 (mergeing sieve)")
True ( 171875000000,"take 10000 primes2 (list queue)")
True ( 312500000000,"take 10000 primes3 (Heap)")
True ( 156250000000,"take 10000 primes4 (Vector)")
-}
