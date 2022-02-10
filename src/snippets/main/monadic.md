---
order: -03000
---
# 一つずつ処理する

同じ問題を $N$ 個のデータについて解け、のように
入力データを部分ごとに分割して処理できる場合、
全体を読み込むのではなく必要な都度読み込む方がメモリが軽くなる。

```haskell
import Control.Monad

main = do
-- データ個数読み込み
  n <- readLn
-- n回繰り返し
  forM_ [1..n] $ const $ do
-- 個別データ読み込み
    [a,b,c] <- getLnInts
    xys <- replicateM c getLnInts
-- 本体
    let ans = compute a b c xys
-- 出力
    print ans
    putStrLn $ if ans then "Yes" else "No"
    putStrLn ans
    putStrLn $ unwords $ map show ans
    putStrLn $ foldr ($) "" $ intersperse (' ' :) $ map shows ans
    mapM_ print ans

getLnInts :: IO [Int]
getLnInts = getLine >>= return . map read . words

compute :: Int -> [Int] -> [[Int]] -> [Int Bool]
compute n a b c xys =
```

## ByteString版

あるいは、mutable vector を軸に進むために
コード全体をモナド計算にすることが必要な場合、
`main` の IO モナドを続けてしまえばよい。

mutable vector版のUnion-Findを使う場合などがこれに該当するだろう。

```haskell
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.List

main = do
-- 読み込み
  [n] <- bsGetLnInts
-- mutable vector を作成
  vec <- MV.new (succ n)
-- 繰り返し読み込み、vectorに書き込み
  forM_ [1..n] $ (\i -> do
    [a,b,c] <- bsGetLnInts
    MV.write vec i (f a b c)
-- 本体
  ans <- compute n a b c xys
-- 出力
  print ans
  putStrLn ans
  putStrLn $ if ans then "Yes" else "No"
  putStrLn $ foldr ($) "" $ intersperse (' ' :) $ map shows ans
  mapM_ print ans

bsGetLnInts :: IO [Int]
bsGetLnInts = BS.getLine >>= return . unfoldr (BS.readInt . BS.dropWhile isSpace)

compute :: Int -> [Int] -> [[Int]] -> [Int Bool]
compute n a b c xys = ...
```
