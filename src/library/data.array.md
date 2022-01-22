# Data.Array

Immutable, Non-Strictな配列っぽい何か。

```haskell
import Data.Array
```

```haskell
data Array i e  -- i 添字型, e 要素型 → 配列の型
```

## 配列を作る

```haskell
-- 添字と要素の対のリストから作る
array      :: Ix i => (i,i) -> [(i,e)] -> Array i e
-- 添え字の順の要素のリストから作る
listArray  :: Ix i => (i,i) ->    [e]  -> Array i e
-- 初期値と継ぎ足し関数で作る
accumArray :: Ix i => (e -> a -> e) -> e -> (i,i) -> [(i,a)] -> Array i e
```

例えば ```accumArray (flip (:)) [] (0, 2) [(i `mod` 3, i) | i <- [1..9]]``` などとできる。

## アクセス

```haskell
-- 添字で1要素を取り出す
(!) :: Ix i => Array i e -> i -> e
-- 配列の範囲
bounds :: Array i e -> (i, i)
-- 添字のリスト
indices :: Ix i => Array i e -> [i]
-- 要素のリスト
elems :: Array i e -> [e]
-- 添字と要素の対応リスト
assocs :: Ix i => Array i e -> [(i, e)]
```

## 更新

```haskell
-- まとめて上書き
(//) :: Ix i => Array i e -> [(i,e)] -> Array i e
-- まとめて継ぎ足し
accum :: Ix i => (e -> a -> e) -> Array i e -> [(i,a)] -> Array i e
```

## 説明

要素へのアクセスは $O(\log n)$ 、要素の更新はかなりかかるが、インデックスの自由度が高いのは利点。
古くから標準で入っているので安心して使える、Immutable, Non-Strictな配列っぽい何か。

ある種の動的プログラミングを実現するのにも使える。
いわゆる「集めるDP」と呼ばれるもので、
再帰において引数が必ず「小さく」なるような再帰関数により定義される関数の計算を高速化する。
必要な関数の値を全て配列に格納するが、関数定義では再帰呼び出しを行わず、
該当する値の格納された配列要素を参照する形に書き換える。

例えば、フィボナッチ数列
$$
\textit{fib}(n) = \left \{
\begin{array}{ll}
1 & \textrm{if} \; 0 \leq n \leq 1\\
\textit{fib}(n-2) + \textit{fib}(n-1) & \textrm{otherwise}
\end{array}
\right .
$$
に対する素朴な定義

```haskell
fib n
  | n <= 1 = 1
  | otherwise = fib (n-2) + fib (n-1)
```

を書き換えると、

```haskell
fibArr = listArray (0,ub) $ map fibFunc [1..ub] -- 配列fibArrの要素は全てfibFuncの値
  where
    ub = 10000
    fibFunc n
      | n <= 1 = 1
      | otherwise = fibArr ! (n-2) + fibArr ! (n-1) -- 関数fibFuncの計算はfibArrを参照する
```

となる。この書き換えは明示的なメモ化とも解釈できる。

もう一方の「配るDP」と呼ばれるスタイルの動的プログラミングの手続きをHaskellで実現するのはもう少し工夫を要する。
