---
order: -01000
---
# 切り上げ除算

```haskell
-- @gotoki_no_joe
divrup x y = negate $ div (negate x) y
```

[あのアルゴリズムはどこ？の2](/readings/whereis/02.divrup/)より。
