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
