import System.CPUTime

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV
import Control.Monad
import Control.Monad.ST

import Data.List

main = do
    run test1 "test1"
    run test2 "test2"
    run test3 "test3"

run v s = do
  t0 <- getCPUTime
  print v
  t1 <- getCPUTime
  print (t1 - t0, s)

accsum1 :: [[Int]] -> V.Vector (V.Vector Int)
accsum1 dss =
    V.fromList $ map V.fromList $
    scanl' (zipWith (+)) (replicate n 0) $
    map (scanl' (+) 0) dss
  where
    n = succ $ length $ head dss

spansum1 :: V.Vector (V.Vector Int) -> (Int,Int) -> (Int,Int) -> Int
spansum1 avv (x1,y1) (x2,y2) =
    av2 V.! y2 - av2 V.! y1 - av1 V.! y2 + av1 V.! y1
  where
    av1 = avv V.! x1
    av2 = avv V.! x2

size = 3000

test1 = spansum1 vv (0,0) (size,size)
  where
    vv = accsum1 $ replicate size (replicate size 1)

accsum2 :: [[Int]] -> V.Vector (V.Vector Int)
accsum2 dss =
    V.scanl' (V.zipWith (+)) (V.replicate n 0) $
    V.fromList $
    map (V.scanl' (+) 0 . V.fromList) dss
  where
    n = succ $ length $ head dss

test2 = spansum1 vv (0,0) (size,size)
  where
    vv = accsum2 $ replicate size (replicate size 1)

accsum3 :: [[Int]] -> V.Vector (UV.Vector Int)
accsum3 dss =
    V.scanl' (UV.zipWith (+)) (UV.replicate n 0) $
    V.fromList $
    map (UV.scanl' (+) 0 . UV.fromList) dss
  where
    n = succ $ length $ head dss

spansum3 :: V.Vector (UV.Vector Int) -> (Int,Int) -> (Int,Int) -> Int
spansum3 avv (x1,y1) (x2,y2) =
    av2 UV.! y2 - av2 UV.! y1 - av1 UV.! y2 + av1 UV.! y1
  where
    av1 = avv V.! x1
    av2 = avv V.! x2

test3 = spansum3 vv (0,0) (size,size)
  where
    vv = accsum3 $ replicate size (replicate size 1)

{-
type Mat a = (Int, Int, UV.Vector a)

accsum4 :: [[Int]] -> Mat Int
accsum4 dss = (h, w, v)
  where
    h = succ $ length dss
    w = succ $ length $ head dss
    p x y = y * succ w + x
    v = UV.create action
    action :: ST s (MUV.MVector s Int)
    action = do
      v <- MUV.new (succ h * succ w)
      forM_ [0..w] (\i -> do
        MUV.write v (p i 0) 0
        MUV.write v (p i h) 0)
      forM_ [0..h] (\i -> do
        MUV.write v (p 0 i) 0
        MUV.write v (p w i) 0)
      sequence_ [ MUV.write v (p i j) d | (i,ds) <- zip [1..] dss, (j,d) <- zip [1..] ds]
      ...
      return v

-- Haskellで書くというのはこういうことではない、と思い直して中断。
-}

{-
size = 10000

>.\cumul2d.exe
100000000
(83187500000000,"test1")
100000000
(23546875000000,"test2")
先にベクタにする方がずっと速い。
-}

{-
size = 3000

>.\cumul2d.exe
9000000
(2546875000000,"test1")
9000000
(1281250000000,"test2")
9000000
(  46875000000,"test3")
可能ならUnboxedにした方がずっとずっと速い。
-}
