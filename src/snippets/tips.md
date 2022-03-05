---
order: -90000
---
# 小技

## 二つのベクタ間の値スワップ

v1のaとv2のbを交換  

```haskell
swapswap v1 a v2 b = MUV.write v1 a =<< exchange v2 b =<< MUV.read v1 a
```

しかしAtCoderの Mutable Vector には `MUV.exchange` がない。

```haskell
-- glue
exchange v i x = do y <- MUV.read v i; MUV.write v i x; return y
```

## 座標変換

疎な集合の要素に番号を振るには、`Data.Set` だけあればできる。

```haskell
import qualified Data.Set as S

-- xs の要素を持つ集合
s = S.fromList xs

-- xsの要素xからその番号を得る O(log n)
i = S.findIndex x s

-- 番号iからその要素xを得る O(log n)
x = S.elemAt i x
```

この便利な機能は残念ながら `Data.IntSet` にはない。  
整数が相手の場合、自分で番号を振った `Data.IntMap` を作る方がよいか。

```haskell
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Array

-- xs の要素に番号を付ける
(x2i, i2x) =
  let
    uxs = IS.elems $ IS.fromList xs
    x2i = IM.fromList $ zip uxs [0..]
    i2x = listArray (0, pred $ length uxs) uxs
  in
    (x2i, i2x)

-- xs の要素xからその番号を得る
i = x2i IM.! x

-- 番号iからその要素xをえる
x = i2x ! i
```
