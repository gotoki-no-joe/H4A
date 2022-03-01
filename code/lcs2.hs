-- import Data.List

lcsLen as bs = head line0
  where
    lineN = replicate (succ $ length bs) 0
    line0 = foldr step lineN as
    step ai line1 = foldr (sstep ai) [0] (zip3 bs line1 (tail line1))
    sstep ai (bi, c01, c11) cs@(c10:_)
      | ai == bi = succ c11 : cs
      | otherwise = max c01 c10 : cs
    sstep _ _ _ = error ""

lcsStr as bs = (x, ys)
  where
    lineN = replicate (succ $ length bs) 0
    lines = scanr step lineN as
    step ai line1 = foldr (sstep ai) [0] (zip3 bs line1 (tail line1))
    sstep ai (bi, c01, c11) cs@(c10:_)
      | ai == bi = succ c11 : cs
      | otherwise = max c01 c10 : cs
    sstep _ _ _ = error ""
    x = head (head lines)
    ys = recover 0 bs (zip as lines)
    recover _ [] _ = []
    recover _ _ [] = []
    recover i bbs@(bi:bs) alalines@((aj, line):alines)
      | bi == aj = aj : recover (succ i) bs alines
      | left = recover (succ i) bs alalines
      | otherwise = recover i bbs alines
      where
        left = case drop i line of
                 c:d:_ -> c == d
                 _ -> False
