---
order: 99000
---
# Stringテンプレート

```haskell
import Control.Applicative
import Control.Monad

main = do
-- 読み込み
  n <- read <$> getLine
  [a,b,c] <- map read . words <$> getLine
  xys <- replicateM n (map read . words <$> getLine)
-- 本体
  let ans = compute n a b c xys
-- 出力
  print ans
  putStrLn $ if ans then "Yes" else "No"
  putStrLn ans
  putStrLn $ unwords $ map show ans
  putStrLn $ foldr ($) "" $ intersperse (' ' :) $ map shows ans
  mapM_ print ans

compute :: Int -> [Int] -> [[Int]] -> [Int Bool]
compute n a b c xys =
```
