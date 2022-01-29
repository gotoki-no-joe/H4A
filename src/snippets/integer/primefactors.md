---
order: -03000
---
# 素因数分解（試し割り法）

```haskell #
-- @gotoki_no_joe
primeFactors :: Int -> [Int]
primeFactors n = loop n primes
  where
    primes = 2 : 3 : [y | x <- [6,12..], y <- [pred x, succ x]]
    loop n pps@(p:ps)
      | n == 1    = []
      | n < p * p = [n]
      | r == 0    = p : loop q pps
      | otherwise = loop n ps
      where
        (q,r) = divMod n p
```

素数リスト `primes` が[手元にある場合](../primes/)は、
行5の疑似素数リストの定義をなくせばよい。

[あのアルゴリズムはどこ？の4](/H4A/readings/whereis/04.primefactors/)より。
