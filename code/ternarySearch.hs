import Control.Monad
import Debug.Trace

-- ABC151F

main = do
  n <- readLn
  xys <- replicateM n getLnInts
  let ans = compute n xys
  print ans

getLnInts :: IO [Int]
getLnInts = getLine >>= return . map read . words

compute :: Int -> [[Int]] -> (Double,Double,Double) -- Double
compute n xys = (ax, ay, sqrt $ fr ax ay)
  where
    xs = map (fromIntegral . head) xys
    ys = map (fromIntegral . head . tail) xys
    minX = minimum xs
    minY = minimum ys
    maxX = maximum xs
    maxY = maximum ys

    ax = fst $ ternarySearch f1 epsilon minX maxX
    f1 cx = snd $ ternarySearch (f2 cx) epsilon minY maxY
    f2 cx = \cy -> maximum [cxx + (cy - y)^2 | (cxx,y) <- zip cxxs ys] -- f2 == fr
      where
        cxxs = [(cx - x)^2 | x <- xs]
    ay = fst $ ternarySearch (fr ax) epsilon minY maxY -- 重複
    fr cx cy = maximum [(cx - x)^2 + (cy - y)^2 |(x,y) <- zip xs ys]

epsilon = 1E-6

{-
適当な点 cx, cy に中心をおいたとき、全ての点を含む円の半径(^2)は、点までの距離の最大値
この関数fr(cx,cy)の最小値をとる(cx,cy)を見つけたい。
凸関数なので、X軸に関してスキャンしつつ、その中でY軸に関してスキャンする

-}
-- @gotoki_no_joe
ternarySearch :: Ord a
              => (Double -> a)      -- 目的関数
              -> Double             -- 許容誤差ε
              -> Double -> Double   -- 左と右の範囲
              -> (Double, a)        -- 最小値をとる引数とその最小値
ternarySearch f epsilon l0 r0 = end $ until stop step (l0, r0, f l0)
  where
    end (l, r, fl) = (l, fl)
    step (l,r, fl)
      | fc1 < f c2 = (l, c2, fl)
      | otherwise  = (c1, r, fc1)
      where
        c1 = (l * 2 + r) / 3
        c2 = (l + r * 2) / 3
        fc1 = f c1
    stop (l,r,_) = r - l < epsilon

test1 = compute 2 [[0, 0],[1, 0]] -- (0.5,0,0.5)
test2 = compute 3 [[0, 0],[0, 1],[1, 0]] -- (?,?,0.707106781186497524)
