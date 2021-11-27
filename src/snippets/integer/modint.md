---
order: -05000
---
# 合同算術

```haskell
modBase = 1000000007 -- 10^9+7

r x = mod x modBase
add x y = r (x + y)
mul x y = r (x * y)
```

(追記予定...)
