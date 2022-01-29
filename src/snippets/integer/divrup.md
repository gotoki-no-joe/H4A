---
order: -01000
---
# 切り上げ除算

```haskell
-- @gotoki_no_joe
divrup x y = negate $ div (negate x) y
```

[あのアルゴリズムはどこ？の2](/H4A/readings/whereis/02.divrup/)より。
