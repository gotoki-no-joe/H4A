---
order: -02000
---
# String版

```haskell
import Control.Monad

main = do
-- 読み込み
  n <- readLn
  [a,b,c] <- getLnInts
  xys <- replicateM n getLnInts
-- 本体
  let ans = compute n a b c xys
  ans <- compute n a b c xys
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
