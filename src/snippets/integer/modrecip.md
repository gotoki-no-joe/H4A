---
order: -06000
---
# モジュラ逆数

フェルマーの小定理による方法。
[べき乗](powerish/)を用いている。
また、おそらく他のモジュラ演算も同時に使うため、`mul`の定義はグローバルに置けるだろう。

```haskell
-- @gotoki_no_joe
inv p a = powerish mul 1 a (p-2)
  where
    mul x y = mod (x*y) p
```

拡張ユークリッドの互除法を使う方法もある。

(追記予定...)

## 説明

あのアルゴリズムはどこ？の14、およびアルゴリズムロジックより。

[Wikipedia](https://ja.wikipedia.org/wiki/%E3%83%A2%E3%82%B8%E3%83%A5%E3%83%A9%E9%80%86%E6%95%B0)

難しい話は置いておいて、互いに素な $a$ と $m$ に対して、  
$$ax \equiv 1 \mod m$$  
を満たすような  
$$x \equiv a^{-1} \mod m$$  
を求める方法。

(追記予定...)

## 関連問題

- [ABC177 C Sum of product of pairs](https://atcoder.jp/contests/abc177/tasks/abc177_c)

「紹介した問題は、別に逆元とる必要なかった問題でしたが一応記載します。」とは？

ということで以下ネタバレ

求める値を $X$ とおく。
$S = A_1 + A_2 + \dots + A_N$ とおいて、  
$S^2 = (A_1 + A_2 + \dots + A_N) \times S = A_1 \times S + A_2 \times S + \dots + A_N \times S$ を展開すると
$$
\begin{array}{cccccccc}
A_1 \times A_1 & + & A_1 \times A_2 & + & \dots & + & A_1 \times A_N & + \\
A_2 \times A_1 & + & A_2 \times A_2 & + & \dots & + & A_2 \times A_N & + \\
\vdots \\
A_N \times A_1 & + & A_N \times A_2 & + & \dots & + & A_N \times A_N \\
\end{array}
$$
これは、対角線に $A_i \times A_i$ が並び、その上と下に $X$ が2度現れている。
つまり $S^2 = \sum_{i=1}^N A_i^2 + 2X$
変形すると $X = (S^2 + \sum_{i=1}^N A_i^2) / 2$ で、
$A_i$の総和の2乗から$A_i$の2乗の総和を引き、さらに2で割れば答えが得られる。

しかしそれをまともに計算すると桁あふれするので、合同算術で計算すると、普通に2で割ることができない。
そこで2のモジュラ逆数を求めて乗じることで解決する、ということ。

逆元を使わない答え [ACコード](https://atcoder.jp/contests/abc177/submissions/16440784)

$X = A_1 \times (A_2 + A_3 + \dots + A_N) + A_2 \times (A_3 + \dots + A_N) + \dots + A_{N-1} \times A_N$  
とまとめると、乗算の左辺は $A_i$ が順に現れ、右辺は $A_i$ を後ろから足しこんでいった値になっている。
つまり
```haskell
compute as = sum $ zipWith (*) as $ tail $ scanr1 (+) as
```
で求められる。除算は現れない。

あるいは、前から  
$X = A_1 \times A_2 + (A_1 + A_2) \times A_3 + \dots + (A_1 + \dots + A_{N-1}) \times A_N$  
の向きにまとめて、
```haskell
compute as = snd $ foldl step (0, 0) as
  where
    step (sum, acc) ai = (sum + ai, acc + sum * ai)
```
としてもよい。こちらの方が命令型のループに直しやすそう。
