---
order: -02000
---
# 多重集合

Mapで値域を無視すると、その定義域により集合を表現できる。

```haskell
type Set a = Map a ()
```

これを拡張し、重複する要素数を表す自然数を値域にすれば、
多重集合が表現できる。

## 任意の型版

```haskell
import qualified Data.Map as M

-- @gotoki_no_joe
type MultiSet a = M.Map a Int

emptyMS = M.empty

singletonMS x = M.singleton x 1

fromListMS xs = M.fromListWith (+) [(x, 1) | x <- xs]

insertMS x ms = M.insertWith (+) x 1 ms

deleteMS x ms = M.update dec x ms
  where
    dec 1 = Nothing
    dec n = Just $ pred n

memberMS x ms = M.member x ms

sizeMS ms = sum $ M.elems ms

lookup**MS x ms = fmap fst $ M.lookup** x ms -- LT,LE,GE,GT
```

## 整数版

```haskell
import qualified Data.IntMap as IM

-- @gotoki_no_joe
type MultiIntSet = IM.IntMap Int

emptyMIS = IM.empty

singletonMIS x = IM.singleton x 1

fromListMIS xs = IM.fromListWith (+) [(x, 1) | x <- xs]

insertMIS x ms = IM.insertWith (+) x 1 ms

deleteMIS x ms = IM.update dec x ms
  where
    dec 1 = Nothing
    dec n = Just $ pred n

memberMIS x ms = IM.member x ms

sizeMIS ms = sum $ IM.elems ms

lookup**MS x ms = fmap fst $ IM.lookup** x ms -- LT,LE,GE,GT
```
