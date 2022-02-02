---
order: -03000
---
# 素因数分解（試し割り法）

```haskell #
-- @gotoki_no_joe
primeFactors :: Int -> [Int]
primeFactors n = loop n primes
  where
    primes = 2 : 3 : [y | x <- [5,11..], y <- [x, x + 2]]
    loop n pps@(p:ps)
      | n == 1    = []
      | n < p * p = [n]
      | r == 0    = p : loop q pps
      | otherwise = loop n ps
      where
        (q,r) = divMod n p
```

行5の疑似素数リストは、「3より大きい素数は6の倍数±1」に基づいているが、
「2より大きい素数は奇数」に基づく`2 : [3,5..]` でも大差ないかもしれない。

本物の素数リスト `primes` が[手元にある場合](primes/)はこの定義をなくせばよい。

[あのアルゴリズムはどこ？の4](/readings/whereis/04.primefactors/)より。
