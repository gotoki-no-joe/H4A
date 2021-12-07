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
    q0 = H.insert (H.Entry 9 6) H.empty
    sieve queue xxs@(x:xs) =
      case compare np x of
        LT ->     sieve queue1 xxs
        EQ ->     sieve queue1  xs
        GT -> x : sieve queue2  xs
      where
        H.Entry np p = H.minimum queue
        queue1 = H.insert (H.Entry (np+p) p) $ H.deleteMin queue
        queue2 = H.insert (H.Entry (x * 3) (x * 2)) queue
```

上限が設定できるとき、フラグの配列を舐めることで素数を見つける、
命令型言語でおなじみのやり方をする版

```haskell
import qualified Data.Vector(.Unboxed) as V
import qualified Data.Vector(.Unboxed).Mutable as MV

-- @gotoki_no_joe
ub = 1024 :: Int -- 上限
primev = V.create (do
  v <- MV.replicate (ub+1) True
  forM_ (takeWhile ((ub >=).(^ 2)) [2..]) (\i -> do -- 上限が定数なら手計算で [2..√ub]
    f <- MV.read v i
    when f (forM_ [i*2,i*3..ub] (\j -> MV.write v j False)))
  return v
  )

primes :: [Int]
primes = [k | k <- [2..ub], primev V.! k]
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

## 蛇足

配列を用いた命令型の篩は、素数を見つけるたびにその整数倍を全て除去する。
Haskellの教科書の前の方に書いてある典型的な篩は、素数を見つけるたびに、その素数で割り切れる値を除去するフィルタを追加する。

```Haskell
primes0 :: [Int]
primes0 = 2 : sieve [3,5..]
  where
    sieve (x:xs) = x : sieve (filter (nd x) xs)
    nd x y = mod y x /= 0 -- "Not Divisible"
```

これは、見つけた素数の数だけ `filter` が入れ子になり、また `mod` を使うため効率が悪い。

```haskell
primes0 = 2 : sieve [3,5..] =
2 : 3 : sieve (filter (nd 3) [5,7..]) =
2 : 3 : 5 : sieve (filter (nd 5) (filter (nd 3) [7,9..])) =
2 : 3 : 5 : 7 : sieve (filter (nd 7) (filter (nd 5) (filter (nd 3) [9,11..]))) =
2 : 3 : 5 : 7 : 11 : sieve (filter (nd 11) sieve (filter (nd 7) (filter (nd 5) (filter (nd 3) [13,15..])))) =
...
```

素数3が見つかった後、その3倍の9、その5倍の15、…は順に除去の対象となる。
そこで、「素数pの奇数倍npが次に除去できる候補」という情報を、npの小さい順に優先度付きキューに保存する。
無限に供給される奇数 `[3,5..]` 値 x に対して、このキューの先頭の np の値と比較し、

- np < x の場合、np は既に他の素数の倍数として除去されたので、次は (n+2)p を候補に加える。xはまだ素数かどうか確定していないので保留する。
- np == x の場合、x はまさに p の倍数なので除去する。次は (n+2)p を候補に加える。
- np > x の場合、x は除去できない。これは新たな素数である。npは以降の数と比べるためにそのまま残し、3x をキューにさらに追加する。

とすることで、サンクの肥大を避けることができる。
このアルゴリズムを `Data.List.insert` で手抜き実装すると次のようになる。
見つけた素数pに対して、タプルの左側は奇数倍np、右側は2pを格納している。

```haskell
primes1 :: [Int]
primes1 = 2 : sieve [] [3,5..]
  where
    sieve [] (x:xs) = x : sieve [(3 * x, x)] xs
    sieve npnps@((np,p):nps) xxs@(x:xs) =
      case compare np x of
        LT ->     sieve (insert (np + 2 * p, p) nps) xxs
        EQ ->     sieve (insert (np + 2 * p, p) nps)  xs
        GT -> x : sieve (insert (3 * x, x) npnps) xs
```

リストへの `insert` は重いので、より効率的な Heap を用いて、
また Heap が空であるのは 3 を処理する前だけの特別な場合なので、そこを少しほどくと冒頭のコードになる。

これらの性能を計測すると次のような結果が得られた。(GHCiで実行)
filter modだけ2,000個、他は10,000個の素数を求めている。

|方式|時間 (μ秒)|
|----|----:|
|filter mod (2000個)|2,312,500|
|List.insert|2,671,875|
|Heap|484,375|
|Vector|109,375|
