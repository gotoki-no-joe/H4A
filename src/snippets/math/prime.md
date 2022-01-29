# 素数

```haskell
modBase = 1000000007 -- 10^9 + 7
modBase =  998244353

r x = mod x modBase
add x y = r (x + y)
mul x y = r (x * y)
```
