---
order: 97000
---
# ByteStringテンプレート

```haskell
import Control.Applicative
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.List

main = do
-- 読み込み
  Just (n,_) <- BS.readInt li <$> BS.getLine
  [a,b,c] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  xys <- map (unfoldr (BS.readInt . BS.dropWhile isSpace)) . BS.lines <$> BS.getContents
-- 本体
  let ans = compute n a b c xys
-- 出力
  print ans
  putStrLn ans
  putStrLn $ if ans then "Yes" else "No"
  putStrLn $ foldr ($) "" $ intersperse (' ' :) $ map shows ans
  mapM_ print ans

compute :: Int -> [Int] -> [[Int]] -> [Int Bool]
compute n a b c xys = ...
```
