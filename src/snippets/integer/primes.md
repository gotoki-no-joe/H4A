---
order: -03000
---
# 素数（エラトステネスの篩）

Haskellらしい、無限リストとして昇順に素数が取り出せる版

```haskell
import qualified Data.Heap as H

-- @gotoki_no_joe
primes :: [Int]
primes = 2 : 3 : sieve q0 [5,7..]
  where
    q0 = H.insert (H.Entry 6 3) H.empty
    sieve queue xxs@(x:xs) =
      case compare np x of
        LT ->     sieve queue1 xxs
        EQ ->     sieve queue1  xs
        GT -> x : sieve queue2  xs
      where
        H.Entry np p = H.minimum queue
        queue1 = H.insert (H.Entry (np+p) p) $ H.deleteMin queue
        queue2 = H.insert (H.Entry (x+x)  x) queue
```

上限が設定できるとき、フラグの配列を舐めることで素数を見つける、
命令型言語でおなじみのやり方をする版

しかしvの型が、IOVectorでないときに文句を言われがちなので調整が必要と思われる。

```haskell
import qualified Data.Vector(.Unboxed) as V
import qualified Data.Vector(.Unboxed).Mutable as MV

--@gotoki_no_joe
ub = 1024 :: Int -- 上限
primev = V.create (do
  v <- MV.replicate (n+1) True
  forM_ [2..floor $ sqrt $ fromIntegral n]) (\i -> do -- 定数なら手計算で
    f <- MV.read v i
    when f (forM_ [i*2,i*3..n] (\j -> MV.write v j False)))
  return v
  )
```

## お話

あのアルゴリズムはどこ？の5 より。
必要なものはData.Numbers.Primesに全て揃っているので、
Project Eulerをするなら[そちら](../../library/data.numbers.primes/)を使ったほうがよい。
AtCoderにはこのライブラリがないので自作する必要がある。

### 関連問題

- [天下一プログラマーコンテスト2012 予選C A](https://atcoder.jp/contests/tenka1-2012-qualC/tasks/tenka1_2012_9) - [ACコード](https://atcoder.jp/contests/tenka1-2012-qualC/submissions/27486067)
- [ABC149 C Next Prime](https://atcoder.jp/contests/abc149/tasks/abc149_c) - [ACコード](https://atcoder.jp/contests/abc149/submissions/27486099)
- [ABC170 D Not Divisible](https://atcoder.jp/contests/abc170/tasks/abc170_d) - このコードを使う形では解けていない。別のページで解いているようだ。
