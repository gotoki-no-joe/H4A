import qualified Data.Vector.Unboxed as UV

-- ABC102D

test1 = compute 5 [3, 2, 4, 1, 2]
-- 2
test2 = compute 10 [10, 71, 84, 33, 6, 47, 23, 25, 52, 64]
-- 36

test3 = compute 7 [1, 2, 3, 1000000000, 4, 5, 6]
-- 999999994

-- 左右をバランスさせる、それぞれで左右をバランスさせる、では不正解だった。
{-
compute :: Int -> [Int] -> (Int, Int, Int, [Int], Int) -- Int
compute n as = (k1,k2,k3,ss,maximum ss - minimum ss)
  where
    v = UV.fromList $ scanl1 (+) as
    n1 = pred n
    total = v UV.! n1
    k2 = fst $ ternarySearch (\i -> abs (total - 2 * v UV.! i)) 0 n1
    s12 = v UV.! k2
    k1 = fst $ ternarySearch (\i -> abs (s12 - 2 * v UV.! i)) 0 k2
    k3 = fst $ ternarySearch (\i -> abs (total + s12 - 2 * v UV.! i)) k2 n1
    s1 = v UV.! k1
    s2 = s12 - s1
    s3 = v UV.! k3 - s12
    s4 = total - v UV.! k3
    ss = [s1,s2,s3,s4]
-}

-- 分割の左右で、左右をバランスさせた結果のmax-minを最小化する、という使い方をしてみよう。
compute :: Int -> [Int] -> Int
compute n as = ans
  where
    v = UV.fromList $ scanl (+) 0 as
    spansum l r = v UV.! r - v UV.! l -- lは含まない
    spandiff l c r = abs (spansum l c - spansum c r) -- 上に同じ

    ans = snd $ ternarySearch fun 2 (n-2)
    fun j = maximum ss - minimum ss
      where
        i = fst $ ternarySearch (\i -> spandiff 0 i j) 1 (pred j)
        k = fst $ ternarySearch (\k -> spandiff j k n) (succ j) (pred n)
        ss = [spansum 0 i, spansum i j, spansum j k, spansum k n]

-- @gotoki_no_joe
ternarySearch :: Ord a
              => (Int -> a)   -- 目的関数
              -> Int -> Int   -- 左と右の範囲
              -> (Int, a)     -- 最小値をとる引数とその最小値
ternarySearch f l0 r0 = end $ until stop step (l0, r0)
  where
    swap (a,b) = (b,a)
    end (l, r) = swap $ minimum [(f x, x) | x <- [l,r,div (l+r) 2]]
    step (l, r)
      | f c1 < f c2 = (l, c2)
      | otherwise   = (c1, r)
      where
        d = div (r - l + 2) 3
        c1 = l + d
        c2 = r - d
    stop (l,r) = abs (r - l) <= 2
