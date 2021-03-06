---
order: -04000
---

# はじめからVectorに読み込む

たいていの場合は入力データをリストに読み込んで問題ないが、後でvectorに入れるなら初めからそうしてしまえばよい。

```haskell
import qualified Data.ByteString.Char8 as BS
import Data.Char

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV

-- ByteStringを読んで1からNに収めた長さN+1のVectorを作る
readIntV1N :: Int -> IO (UV.Vector Int)
readIntV1N n = do
    bs <- BS.getLine
    v <- MUV.new (succ n)
    loop v 1 bs
    UV.freeze v
  where
    loop v i bs =
      case BS.readInt (BS.dropWhile isSpace bs) of
        Just (x, bs1) -> do { MUV.write v i x; loop v (succ i) bs1 }
        Nothing -> return ()
```

MV.createを使う版

```haskell
import qualified Data.ByteString.Char8 as BS
import Data.Char

import Control.Monad.ST (ST)
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Vector.Unboxed as V

main = do
  ...
  let as = readIntsV1N  n bytestr
  bs    <- readIntsMV1N n bytestr
  ...

-- ByteStringを読んで1からNに収めた長さN+1のVectorを作る
readIntsV1N :: Int -> BS.ByteString -> V.Vector Int
readIntsV1N n bs = V.create action
  where
    action :: ST s (MV.MVector s Int)
    action = do
      v <- MV.new (succ n)
      loop v 1 bs
    loop v i bs =
      case BS.readInt (BS.dropWhile isSpace bs) of
        Just (x,bs1) -> do { MV.write v i x; loop v (succ i) bs1 }
        Nothing -> return v

-- ByteStringを読んで1からNに収めた長さN+1のMutable Vectorを作る
readIntsMV1N :: Int -> BS.ByteString -> IO (MV.IOVector Int)
readIntsMV1N n bs =
  do
    v <- MV.new (succ n)
    loop v 1 bs
  where
    loop v i bs =
      case BS.readInt (BS.dropWhile isSpace bs) of
        Just (x,bs1) -> do { MV.write v i x; loop v (succ i) bs1 }
        Nothing -> return v
```
