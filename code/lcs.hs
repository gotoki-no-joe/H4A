import Test.QuickCheck
import Data.List
import Data.Array

import qualified Data.Vector.Unboxed.Mutable as MUV
import Control.Monad
import Control.Monad.ST

import System.CPUTime

{-
-- @gotoki_no_joe
lcslen :: Eq a => [a] -> [a] -> Int
lcslen as bs
  | null as || null bs = 0
  | otherwise = last (last lines)
  where
    line0 = replicate (length as) 0
    lines = line0 : zipWith linef bs lines
    linef b line = last line11 `seq` tail line11
      where
        line11 = 0 : zipWith4 f as (0:line) line line11
        f a c00 c01 c10
          | a /= b = c01 `max` c10
          | otherwise = c01 `max` c10 `max` succ c00

-- @gotoki_no_joe
lcslenX :: Eq a => [a] -> [a] -> Int
lcslenX as bs
  | null as || null bs = 0
  | otherwise = last (last lines)
  where
    line0 = replicate (length as) 0
    lines = line0 : zipWith linef bs lines
    linef b line = last line11 `seq` tail line11
      where
        line11 = 0 : zipWith4 f as (0:line) line line11
        f a c00 c01 c10
          | a /= b = c01 `max` c10
          | otherwise = succ c00 -- here

prop :: [Bool] -> [Bool] -> Property
prop as bs = label (show v1) (v1 == v2)
  where
    v1 = lcslen as bs
    v2 = lcslenX as bs
-}

lcs0 as bs = recur (length as, length bs)
  where
    recur (0, _) = 0
    recur (_, 0) = 0
    recur (i, j)
      | as !! i1 /= bs !! j1 = recur (i1, j) `max` recur (i, j1)
      | otherwise            = succ $ recur (i1, j1)
      where
        i1 = pred i
        j1 = pred j

lcs1 as bs = recur (length as, length bs)
  where
    recur (0, _) = 0
    recur (_, 0) = 0
    recur (i, j) = maximum $
                   [succ $ recur (i1, j1) | as !! i1 == bs !! j1] ++
                   [recur (i1,j), recur (i,j1)]
      where
        i1 = pred i
        j1 = pred j

-- both deadly slow
prop :: [Bool] -> [Bool] -> Property
prop as bs = label (show v1) (v1 == v2)
  where
    v1 = lcs0 as bs
    v2 = lcs1 as bs

lcsArr as bs = arr ! (al, bl)
  where
    al = length as
    bl = length bs
    arr = array ((0,0), (al, bl))
          [((i,j), f i j) | i <- [0..al], j <- [0..bl]]
    f 0 _ = 0
    f _ 0 = 0
    f i j
      | a /= b = (arr ! (pred i, j)) `max` (arr ! (i, pred j))
      | otherwise = succ $ arr ! (pred i, pred j)
      where
        a = as !! pred i
        b = bs !! pred j

prop2 :: String -> String -> Property
prop2 as bs = label (show v1) (v1 == v2)
  where
    v1 = lcs0 as bs
    v2 = lcsArr as bs

lcsList as bs = last linen
  where
    line0 = 0 : replicate (length as) 0
    linen = foldl' {-'-} step line0 bs
    step linei bi = sum linei1 `seq` linei1
      where
        linei1 = 0 : zipWith4 f as linei (tail linei) linei1
        f ai arr00 arr10 arr01
          | ai == bi = succ arr00
          | otherwise = max arr10 arr01

prop3 :: String -> String -> Property
prop3 as bs = label (show v1) (v1 == v2)
  where
    v1 = lcsList as bs
    v2 = lcsArr as bs

{-
lcsListRe as bs = last linen
  where
    line0 = 0 : replicate (length as) 0
    linen = foldl' {-'-} step line0 bs
    step linei bi = linei1
      where
        linei1 = 0 : zipWith4 f as linei (tail linei) linei1
        f ai arr00 arr10 arr01
          | ai == bi = maximum [succ arr00, arr10, arr01] -- redundant version
          | otherwise = max arr10 arr01

prop4 :: String -> String -> Property
prop4 as bs = label (show v1) (v1 == v2)
  where
    v1 = lcsList as bs
    v2 = lcsListRe as bs
-}

lcs :: Eq a => [a] -> [a] -> (Int,[[a]])
lcs as bs = fmap (map reverse) $ last linen
  where
    zero = (0,[[]])
    line0 = zero : replicate (length as) zero
    linen = foldl' {-'-} step line0 bs
    step line bi = sum (map fst line1) `seq` line1
      where
        line1 = zero : zipWith4 f as line (tail line) line1
        f ai (c00, s00) a10@(c10, s10) a01@(c01, s01)
          | ai == bi = (succ c00, map (ai :) s00)
          | otherwise =
              case compare c10 c01 of
                GT -> a10
                LT -> a01
                EQ -> (c10, nub (s10 ++ s01))

lcsA :: Eq a => [a] -> [a] -> (Int,[a])
lcsA as bs = fmap reverse $ last linen
  where
    zero = (0,[])
    line0 = zero : replicate (length as) zero
    linen = foldl' {-'-} step line0 bs
    step line bi = line1
      where
        line1 = zero : zipWith4 f as line (tail line) line1
        f ai (c00, s00) a10@(c10, s10) a01@(c01, _)
          | ai == bi = (succ c00, ai : s00)
          | otherwise =
              case compare c10 c01 of
                GT -> a10
                LT -> a01
                EQ -> (c10, s10)

lcsListV as bs = runST action
  where
    n = length as
    n1 = succ n
    action :: ST s Int
    action = do
      line0 <- MUV.replicate n1 0
      linen <- foldM (step as) line0 bs
      MUV.read linen n
--    step :: Eq t => [t] -> MUV.MVector s Int -> t -> ST s (MUV.MVector s Int)
    step as line bi = do
      line1 <- MUV.new n1
      MUV.write line1 0 0
      forM_ (zip [1..n] as) (\(i,ai) ->
        if ai == bi
        then do
          a00 <- MUV.read line (pred i)
          MUV.write line1 i (succ a00)
        else do
          a01 <- MUV.read line i
          a10 <- MUV.read line1 (pred i)
          MUV.write line1 i (max a10 a01)
        )
      return line1

main = do
  let as = replicate 10000 'a'
  putStrLn "lcsList"
  time $ lcsList as as
--  putStrLn "lcsArr"
--  time $ lcsArr as as
  putStrLn "lcsVec"
  time $ lcsListV as as
  putStrLn "lcsVec2"
  time $ lcsListV2 as as

time e = do
  t0 <- getCPUTime
  print e
  t1 <- getCPUTime
  print $ t1 - t0

{-
リスト版で、 foldl' して、さらに step の中で maximum line1 `seq` して強制すると、
STモナドの中のMutable Vector版と遜色ない、むしろ速いという結果を得た。

>lcs
lcsList
10000
6281250000000
lcsArr
10000
10828125000000
lcsVec
10000
750000000000

-}

lcsListV2 :: (Foldable t1, Eq t2) => [t2] -> t1 t2 -> Int
lcsListV2 as bs = runST action
  where
    n = length as
    n1 = succ n
    action :: ST s Int
    action = do
      line0 <- MUV.replicate n1 0
      line1 <- MUV.replicate n1 0
      (linen,_) <- foldM (step as) (line0,line1) bs
      MUV.read linen n
--    step :: Eq t => [t] -> (MUV.MVector s Int,MUV.MVector s Int) -> t -> ST s (MUV.MVector s Int,MUV.MVector s Int)
    step as (line,line1) bi = do
      forM_ (zip [1..n] as) (\(i,ai) ->
        if ai == bi
        then do
          a00 <- MUV.read line (pred i)
          MUV.write line1 i (succ a00)
        else do
          a01 <- MUV.read line i
          a10 <- MUV.read line1 (pred i)
          MUV.write line1 i (max a10 a01)
        )
      return (line1, line)

{-
C:\Users\ohkubo\Documents\repo\H4A\code>lcs
lcsList
10000
6375000000000
lcsVec
10000
421875000000
lcsVec2
10000
406250000000

遜色ないとか嘘で、Arr版の後始末でGCしていただけっぽい。

-}

lcsMV :: Eq t => [t] -> [t] -> Int
lcsMV as bs = runST action
  where
    n = length as
    n1 = succ n
    action :: ST s Int
    action = do
      line0 <- MUV.replicate n1 0
      line1 <- MUV.replicate n1 0
      let step = \(line, line1) bi -> do {
        forM_ (zip [1..n] as) (\(i,ai) ->
          if ai == bi
          then do
            a00 <- MUV.read line (pred i)
            MUV.write line1 i (succ a00)
          else do
            a01 <- MUV.read line i
            a10 <- MUV.read line1 (pred i)
            MUV.write line1 i (max a10 a01)
          );
        return (line1, line)}
      (linen,_) <- foldM step (line0,line1) bs
      MUV.read linen n

lcsVV :: Eq t => [t] -> [t] -> (Int,[t])
lcsVV as bs = runST action
  where
    n = length as
    n1 = succ n
    action = do
      line0 <- MUV.replicate n1 0
      lines <- foldM (step as) [line0] bs
      x <- MUV.read (head lines) n
      ys <- recover n [] (reverse as) (reverse bs) lines
      return (x, ys)
--    step :: Eq t => [t] -> [MUV.MVector s Int] -> t -> ST s [MUV.MVector s Int]
    step as lines bi = do
        let line = head lines
        line1 <- MUV.new n1 ; MUV.write line1 0 0
        forM_ (zip [1..n] as) (\(i,ai) ->
          if ai == bi
          then do
            a00 <- MUV.read line (pred i)
            MUV.write line1 i (succ a00)
          else do
            a01 <- MUV.read line i
            a10 <- MUV.read line1 (pred i)
            MUV.write line1 i (max a10 a01)
          )
        return (line1 : lines)
    recover :: Eq t => Int -> [t] -> [t] -> [t] -> [MUV.MVector s Int] -> ST s [t]
    recover 0 ys _ _ _ = return ys
--    recover _ ys [] _ _ = return ys -- redundant?
    recover _ ys _ [] _ = return ys
    recover i ys aas@(a:as) bbs@(b:bs) lls@(line:lines)
      | a == b = recover (pred i) (a:ys) as bs lines
      | otherwise = do
          a11 <- MUV.read line i
          a01 <- MUV.read line (pred i)
          if a01 == a11
            then recover (pred i) ys as bbs lls
            else recover i ys aas bs lines
    recover _ _ _ _ _ = error "" -- diable warning

prop5 :: String -> String -> Property
prop5 as bs = label (show $ fst v1) (v1 == v2)
  where
    v1 = lcsA as bs
    v2 = lcsVV as bs

