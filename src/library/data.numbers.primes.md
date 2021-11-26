# Data.Numbers.Primes

素数に関する機能

Project Eulerをするには欠かせない。
Haskell Platformには含まれない。

[hackage](http://hackage.haskell.org/package/primes-0.2.1.0/docs/Data-Numbers-Primes.html)

```haskell
import Data.Numbers.Primes
```

```haskell
-- 素数列
primes :: Integral int => [int]
-- 素数判定
isPrime :: Integral int => int -> Bool
-- 素因数分解
primeFactors :: Integral int => int -> [int]
```
