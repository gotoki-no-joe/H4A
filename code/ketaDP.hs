import Data.Array
import Debug.Trace

step1 :: [Int] -> Integer
step1 as = trace (show final) $ ans
  where
    bnds = (False,True)
    initial :: Array Bool Integer
    initial = listArray bnds [1,0]
    final = foldl step initial as
    step arr ai = trace (show (arr,ai)) $ accumArray (+) 0 bnds
      [ (cond1 || d < ai, val)
      | (cond1, val) <- assocs arr
      , val > 0
      , let ub = if cond1 then 9 else ai
      , d <- [0..ub]
      ]
    ans = sum [val | (_,val) <- assocs final]

test1 :: Integer
test1 = step1 [4,2,7,5,6,3,1]

step2 :: [Int] -> Integer
step2 as = ans
  where
    bnds = ((False,False),(True,True))     -- 小さい数字を使った/3を使った
    initial :: Array (Bool,Bool) Integer
    initial = listArray bnds $ 1 : replicate 3 0
    final = foldl step initial as
    step arr ai = accumArray (+) 0 bnds
      [ (( cond1 || d < ai
         , cond2 || d == 3)                -- 3を使ったことがあるか、今回使ったならTrue
        , val)
      | ((cond1,cond2),val) <- assocs arr
      , val > 0
      , let ub = if cond1 then 9 else ai
      , d <- [0..ub]
      ]
    ans = sum [val | ((_,True),val) <- assocs final] -- 3を使った場合の総和

test2 :: Integer
test2 = step2 [4,2,7,5,6,3,1]

test20 :: Int
test20 = length [() | i <- [0..4275631], elem '3' $ show i]

step3 :: [Int] -> Integer
step3 as = ans
  where
    bnds = ((False,False,0),(True,True,2))     -- 小さい数字を使った/3を使った/桁ごとの和 mod 3
    initial :: Array (Bool,Bool,Int) Integer
    initial = accumArray (+) 0 bnds [((False,False,0),1)]
    final = foldl step initial as
    step arr ai = accumArray (+) 0 bnds
      [ (( cond1 || d < ai
         , cond2 || d == 3
         , mod (x + d) 3)
        , val)
      | ((cond1,cond2,x),val) <- assocs arr
      , val > 0
      , let ub = if cond1 then 9 else ai
      , d <- [0..ub]
      ]
    ans = sum [val | ((_, b, x),val) <- assocs final, b || x == 0] -- 3がある or mod 3 == 0

test3 :: Integer
test3 = step3 [4,2,7,5,6,3,1]

test30 :: Int
test30 = length [() | i <- [0..4275631], elem '3' (show i) || mod i 3 == 0]

step4 :: [Int] -> Integer
step4 as = ans
  where
    bnds = ((False,False,0,0),(True,True,2,7))     -- 小さい数字を使った/3を使った/桁ごとの和 mod 3/mod 8
    initial :: Array (Bool,Bool,Int,Int) Integer
    initial = accumArray (+) 0 bnds [((False,False,0,0),1)]
    final = foldl step initial as
    step arr ai = accumArray (+) 0 bnds
      [ (( cond1 || d < ai
         , cond2 || d == 3
         , mod (x + d) 3
         , mod (2 * y + d) 8) -- 8で割った余り
        , val)
      | ((cond1,cond2,x,y),val) <- assocs arr
      , val > 0
      , let ub = if cond1 then 9 else ai
      , d <- [0..ub]
      ]
    ans = sum [val | ((_, b, x, y),val) <- assocs final, b || x == 0, y /= 0] -- 3がある or mod 3 == 0 かつ mod 8 ≠ 0

test4 :: Integer
test4 = step4 [4,2,7,5,6,3,1]

test40 :: Int
test40 = length [() | i <- [0..4275631], elem '3' (show i) || mod i 3 == 0, mod i 8 /= 0]
