折り畳みの実現 divの後１行空けるのが必須

<details><summary>すごく長い文章とかプログラムとか</summary><div>

```python
print('Hello world!')
```　
</div></details>


# 切り上げ除算の話

(この辺の解説は多分リンク先にある。どこかで、除算が4種類あるという話を読んだし。)

Haskellには、整数除算は `divMod` と `quotRem` の2つがある。

```
> map (flip divMod (3)) [5,6,7,-5,-6,-7]
[(1,2),(2,0),(2,1),(-2,1),(-2,0),(-3,2)]
> map (flip divMod (-3)) [5,6,7,-5,-6,-7]
[(-2,-1),(-2,0),(-3,-2),(1,-2),(2,0),(2,-1)]
> map (flip quotRem (3)) [5,6,7,-5,-6,-7]
[(1,2),(2,0),(2,1),(-1,-2),(-2,0),(-2,-1)]
> map (flip quotRem (-3)) [5,6,7,-5,-6,-7]
[(-1,2),(-2,0),(-2,1),(1,-2),(2,0),(2,-1)]
```
表に整理すると

```
divMod
    5   6   7   -5   -6   -7 : 被除数
+3  1   2   2   -2   -2   -3 : 商
    2   0   1    1    0    2 : 余り
-3 -2  -2  -3    1    2    2
   -1   0  -2   -2    0   -1

quotRem
    5   6   7   -5   -6   -7 : 被除数
+3  1   2   2   -1   -2   -2 : 商
    2   0   1   -2    0   -1 : 余り
-3 -1  -2  -2    1    2    2
    2   0   1   -2    0   -1
```

除数が正のとき、divModは商を「超えない」値に、すなわち負の無限大方向に寄せている。
一方quotRemは0に寄せている。
被除数も正の範囲ではどちらも変わらず、切り捨てを行っている。


# べき乗mod

```haskell
powerish f a b c = foldl' f c [p | (b,p) <- zip bs ps, odd b]
  where
    bs = takeWhile (0 /=) $ iterate (flip div 2) b
    ps = iterate (\x -> f x x) a
```

```haskell
-- @gotoki_no_joe
powerish mul init a b =
    foldl' mul init [p | (b,p) <- zip bs ps, odd b]
  where
    bs = takeWhile (0 /=) $ iterate (flip div 2) b
    ps = iterate (\x -> mul x x) a
```

# ベクタに読み込む

まず、ByteString 1行をリストに読み込む断片を名前つける

```haskell
readIntsBSLine = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
```

そして、これなどをn回使った結果をn要素のmutable vectorに読み込むアクション

```haskell
-- アクション使ってn行読んでn要素のVectorに入れて返す
readIntsMV :: Int -> IO a -> IO (MV.IOVector a)
readIntsMV n action = do
  v <- MV.new n
  forM_ [0..pred n] (\i -> do
    x <- action
    MV.write v i x
    )
  return v
-- readIntsMV action n = MV.generateM n (\_ -> action) AtCoder Sucks!
```

AtCoderのVectorが古くてgenerateMが使えないとか。

# アルゴ式のスニペット

module Main where

import Control.Applicative

main = do
  s <- getLine
  [a,b] <- map read . words <$> getLine
  print $ compute a b
  putStrLn $ if compute a b then "Yes" else "No"

compute :: Int -> Int -> Int
compute a b =

---

module Main where

import Control.Applicative
import Control.Monad
import Data.Array
import Data.List

main = do
  [n,m] <- getIntsLine
  abL <- replicateM m getIntsLine
  let ans = compute n m abL
  forM ans (putStrLn . unwords . map show)

getIntsLine = map read . words <$> getLine

compute :: Int -> Int -> [[Int]] -> [[Int]]
compute n m abL =
  map sort $ elems $
  accumArray (flip (:)) [] (0,pred n) [(a,b) | (a:b:_) <- abL]



module Main where

import Control.Applicative
import Control.Monad
import Data.Array
import Data.List
import qualified Data.IntSet as IS

main = do
  [n,m,x] <- map read . words <$> getLine
  abL <- replicateM m (map read . words <$> getLine)
  print $ compute n m x abL

compute :: Int -> Int -> Int -> [[Int]] -> Int
compute n m x abL = length foff
  where
    fa = accumArray (flip (:)) [] (0,pred n) $
         [p | (a:b:_) <- abL, p <- [(a,b),(b,a)]]
    friends = IS.fromList (x : fa ! x)
    foff = [ i
           | i <- [0..pred n]
           , IS.notMember i friends
           , any (flip IS.member friends) (fa ! i)]
