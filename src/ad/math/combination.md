# 二項係数

あのアルゴリズムはどこ？の16 および
アルゴリズムロジック
より。

高校数学では $_nC_k$ と、世界的には
$\left ( \begin{array}{c} n \\ k \end{array} \right )$
と表記する、 $n$ 個の中から $k$ 個取り出す組み合わせの場合の数。

- [アルゴリズムロジック](https://algo-logic.info/combination/)
- https://blog.satoooh.com/entry/5195/

$$\displaystyle {}_nC_r = \frac{n!}{r ! \times (n-r) !} = {}_nC_{n-r}$$

素直に書くと、$n!$ の前半 $1 \times 2 \times \dots \times r$ は分母の $r!$ と約分されて、

```haskell
comb n r = product [r+1..n] `div` product [1..n-r]
```

この `product` はすぐ巨大な数になり `Int` ではオーバーフローするし、
結果は整数になるので分子は分母で必ず割り切れ、結果は `Int` に収まるときに `Integer` を使うのは癪。
可能な項で約分して、よしなに計算するようにしてみる。

```haskell
comb n r = product $ foldl step [r+1..n] [2..n-r]
  where
    step (x:xs) d
      | cd == 1 = step (x:ys) xs d
      | True = (if x == cd then id else (div x cd :)) $
               (if d == cd then xs else (step xs (div d cd)))
      where
        cd = gcd x d
```

## (ここにアルゴリズムロジックをもとに書いた内容を転記)


### 関連問題

- [ABC034 C 経路](https://atcoder.jp/contests/abc034/tasks/abc034_c) - [ACコード](https://atcoder.jp/contests/abc034/submissions/22940225)
- [ABC145 D Knight](https://atcoder.jp/contests/abc145/tasks/abc145_d) - [ACコード](https://atcoder.jp/contests/abc145/submissions/22940393)  コーナーケースに気が付かなかった…
- [ABC132 D Blue and Red Balls](https://atcoder.jp/contests/abc132/tasks/abc132_d) - 【ACコード】
- [ABC167 E Colorful Blocks](https://atcoder.jp/contests/abc167/tasks/abc167_e) - 【ACコード】
- [ABC202 D aab aba baa](https://atcoder.jp/contests/abc202/tasks/abc202_d) - [ACコード](https://atcoder.jp/contests/abc202/submissions/23698227)
- [ABC021 D 多重ループ](https://atcoder.jp/contests/abc021/tasks/abc021_d) - [ACコード](https://atcoder.jp/contests/abc021/submissions/26238289)
- [ABC156 D Bouquet](https://atcoder.jp/contests/abc156/tasks/abc156_d) - [ACコード](https://atcoder.jp/contests/abc156/submissions/26238931) 「繰り返し2乗法と n が大きい場合の二項係数です。」

ABC021Dについて  
1からnの中から、重複を許してk個選択して、それを小さい順にa1からakに割り当てると全ての場合を数え上げることになる。
このような重複組み合わせの場合の数は高校数学の公式で ${}_{n+k-1}C_k$ と教わっている。

