---
order: -03000
---
# 素数（エラトステネスの篩）

Haskellらしい、無限リストとして昇順に素数が取り出せる版（リストで優先度付きキューを模倣）

```haskell
-- @gotoki_no_joe
primes :: [Int]
primes = 2 : sieve [] [3,5..]
  where
    sieve ((np,p2):nps) (x:xs) | np <  x =     sieve (insert (np+p2,p2) nps) (x:xs)
                               | np == x =     sieve (insert (np+p2,p2) nps)    xs
    sieve          nps  (x:xs)           = x : sieve (insert (x*x, x+x) nps)    xs
```

優先度付きキューの Data.Heap をちゃんと使う版（ただし、上の実装の方がなぜか速い）

```haskell
import qualified Data.Heap as H

-- @gotoki_no_joe
primes :: [Int]
primes = 2 : 3 : sieve q0 [5,7..]
  where
    q0 = H.insert (H.Entry 9 6) H.empty
    sieve queue xxs@(x:xs) =
      case compare np x of
        LT ->     sieve queue1 xxs
        EQ ->     sieve queue1  xs
        GT -> x : sieve queue2  xs
      where
        H.Entry np p2 = H.minimum queue
        queue1 = H.insert (H.Entry (np+p2) p2) $ H.deleteMin queue
        queue2 = H.insert (H.Entry (x * x) (x * 2)) queue
```

上限が設定できるとき、フラグの配列を舐めることで素数を見つける、
命令型言語でおなじみのやり方をする版。
残念ながらこれが最も速い。

```haskell
import qualified Data.Vector(.Unboxed) as V
import qualified Data.Vector(.Unboxed).Mutable as MV

-- @gotoki_no_joe
ub = 1024 :: Int -- 上限
primev = V.create (do
  v <- MV.replicate (ub+1) True
  forM_ (takeWhile ((ub >=).(^ 2)) [2..]) (\i -> do -- 上限が定数なら手計算で [2..√ub]
    f <- MV.read v i
    when f (forM_ [i*i,i*succ i..ub] (\j -> MV.write v j False)))
  return v
  )

primes :: [Int]
primes = [k | k <- [2..ub], primev V.! k]
```

[あのアルゴリズムはどこ？の5](/readings/whereis/05.primes/) より。
必要なものはData.Numbers.Primesに全て揃っているので、
Project Eulerをするなら[そちら](../../library/data.numbers.primes/)を使ったほうがよい。
AtCoderにはこのライブラリがないので自作する必要がある。
