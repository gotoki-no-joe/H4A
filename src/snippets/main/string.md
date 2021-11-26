---
order: 99000
---
# Stringテンプレート

```haskell
import Control.Applicative

main = do
-- 読み込み
  x <- read <$> getLine
  [a,b,c] <- map read . words <$> getLine
  xys <- map (map read . words) . lines <$> getContents
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
