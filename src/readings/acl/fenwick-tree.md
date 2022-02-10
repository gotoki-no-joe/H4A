---
order: -01000
---

# フェニック木

- [Wikipedia フェニック木](https://ja.wikipedia.org/wiki/%E3%83%95%E3%82%A7%E3%83%8B%E3%83%83%E3%82%AF%E6%9C%A8)

固定長の数列があり、以下の操作を入り混ぜて何度も効率的に行いたい。

- 先頭から $k$ 要素の和を求める
- 第 $k$ 要素の値を更新する

この両方を $O(\log n)$ で行えるデータ構造とアルゴリズムの一つがフェニック木である。

## 解説

この説明をどこで読んだのか、これで理解しても配列実装に全然結びつかない。
オクライリ。

木は $2^n-1$ 個の要素を管理する。

$2^{m+1} - 1$ 個の要素を管理する木の根ノードには、
管理する範囲の先頭から $2^m$ 個の要素の総和がラベル付けされ、
その続き $2^m - 1$ 個の要素を管理する部分木を右の子として持つ。
また、自身が総和だけ持っている $2^m$ 個の要素の詳細情報は、
左の子として持つ、先頭から $2^m-1$ 個の要素を管理する部分木にある。

$m = 0$ のとき、自身の管理する要素数1、子が管理する要素数0となるので、葉ノードとなる。

以上の情報を表現できる代数的データ型は次のように書ける。  
（mを全てのノードに持たせる必要はないが、話を簡単にするためにつける。）

```haskell
data FWT a = FWTNull
           | FWT Int     -- 管理する要素数 2^m
                 a       -- その要素の総和
                 (FWT a) -- 左の子
                 (FWT a) -- 右の子
```

### 問い合わせ

この木に対して、
先頭の第0要素から $k \; (0 \leq k < 2^{m+1})$ 個の要素の和を
求める手順を考える。

```haskell
query :: Num a => Int -> (FWT a) -> a
query k (FWT size val lt rt) = ...
```

`k` と `size` の大小関係により、次のように場合分けできる。

- $k < 2^m$ ならば、左の子に問い合わせを丸投げする。
- $k = 2^m$ ならば、答えは自分が持っている。
- $k > 2^m$ ならば、前 $2^m$ 個については持っており、残りを右の子に問い合わせ、結果を足し合わせる。

```haskell
query k (FWT size val lt rt) =
  case compare k size of
    LT -> query k lt
    EQ -> val
    GT -> val + query (k - size) rt
```

$k = 0$ で呼び出されたとき即座に 0 を返すことにすれば、`EQ` と `GT` を一本化できる。

```haskell
query 0 _                                = 0
query k (FWT size val lt rt) | k < size  = query k lt
                             | otherwise = val + query (k - size) rt
```

要素数 $n = 2^m$ の木は高さは $m$ の完全二分木で、
`query` の再帰呼び出しは左か右か、どちらか一方の子にのみ進むので、
計算量は $O(\log n)$ である。

### 構築

このような木を初期値のリストから構築する方法を考える。
簡単のため、0をパディングして要素数は $2^{m+1} - 1$ とする。
木の構築は、構造の定義を忠実になぞることで行える。

$m = 0$ のとき、つまり要素数1のとき、それを `x` とすると `FWT 1 x FWTNull FWTNull` が求める木である。
それより長い場合は、

- 幅 `len` を $2^{m+1} - 1$, `len2` を $2^m - 1$ とする。
- 前から $2^{m} - 1$ 要素の木 `lt = FWT len2 valL ...` を作る。
- 前 $2^m$ 要素より後ろの木 `rt` を作る。
- 前 $2^m$ 要素の末尾、第 $2^m-1$ 要素を `x` とすると、
`FWT len (valL + x) lt rt` が求める木である。

と再帰的に構築できる。

```haskell
makeFWT :: Num a => [a] -> FWT a
makeFWT xs = recur n (xs ++ repeat 0)
  where
    len = length xs
    n = pred $ head $ dropWhile (succ len >=) $ iterate (2 *) 1
    recur 1 (x:_) = FWT 1 x FWTNull FWTNull
    recur n xs = FWT n (s1 + x) lt rt
      where
        n2 = div n 2
        x = xs !! n2
        lt@(FWT _ s1 _ _) = recur n2 xs -- take n2 xs は不要
        rt = recur n2 (drop n2 xs)
```

### 更新

第 $k$ 要素の値に $d$ を加えたい（負の値も含めて）とする。
そのためには、この要素の値だけを持つ葉ノードだけでなく、
この要素を範囲に含める全ての中間ノードの値も同じだけ変化させる。

```haskell
modifyFWT :: Num a => Int -> a -> FWT a -> FWT a
modifyFWT _ _ FWTNull = FWTNull
modifyFWT k d (FWT size val lt rt)
  | k < size  = FWT size (val+d) (modifyFWT k d lt)    rt
  | otherwise = FWT size  val lt (modifyFWT (k-size) d rt)
```

第 $k$ 要素の値を $v$ に変更するには、
第 $k$ 要素までの和から第 $k - 1$ 要素までの和を引くことで
現在の値 $x$ を得て、これとの差を $v - x$ を足しこめばよい。

```haskell
writeFWT :: Num a => Int -> a -> FWT a -> FWT a
writeFWT k v fwt = modifyFWT k (v-x) fwt
  where
    x = query (succ k) fwt - query k fwt
```

計算量はやはり $O(\log n)$ である。

### 木による実装まとめ

根の幅がわかれば、子に降りるたびに2で割った値がその幅なので、
全てのノードにsize情報を持たせる必要はない。
また、木が保持する情報は総和に限定されず、
結合律が成り立ちパディングのための単位元が成り立つ演算であればよい。
これらの点を改良した版を示す。

```haskell
data FenwickTree a =
 FenwickTree Int (a->a->a) a (FWT a) -- 幅, 演算, 単位元, 木

data FWT a = FWT a (FWT a) (FWT a) -- 総和, 左右の子

-- 先頭からk要素の総和を返す
queryFWT :: Int -> (FenwickTree a) -> a
queryFWT k (FenwickTree w op u t) = recur k w t
  where
    recur 0 _ = u
    recur k w (FWT val lt rt)
      | k < w2 = recur k w2 lt
      | True   = op val $ recur (k - w2) rt
      where
        w2 = div w 2

-- リストに対するフェニック木を作る
makeFWT :: (a->a->a) -> a -> [a] -> FenwickTree a
makeFWT op u xs = FenwickTree w op u $ recur w (xs ++ repeat u)
  where
    len = length xs
    n = pred $ head $ dropWhile (succ len >=) $ iterate (2 *) 1
    recur 1 (x:_) = FWT 1 x undefined undefined
    recur n xs = FWT n (s1 + x) lt rt
      where
        n2 = div n 2
        x = xs !! n2
        lt@(FWT _ s1 _ _) = recur n2 xs
        rt = recur n2 (drop n2 xs)

-- 木の第k要素にdを足しこむ
modifyFWT :: Int -> a -> FenwickTree a -> FenwickTree a
modifyFWT k d (FenwickTree w op u t) = FenwickTree w op u (recur w k d t)
  where
    recur 0 _ _ t = t -- undefined
    recur w k d (FWT val lt rt)
      | k < w2 = FWT (op val d) (recur w2 k d lt) rt
      | True   = FWT val lt (recur w2 (k - w2) d rt)
      where
        w2 = div w 2
```

`writeFWT` を一般化するには、`(+)`に対する`(-)`のような逆演算も必要となる。

```haskell
-- 第k要素をvに変更する
writeFWT :: (a->a->a) -> Int -> a -> FenwickTree a -> FenwickTree a
writeFWT po k v fwt = modifyFWT k (v-x) fwt
  where
    x = queryFWT (succ k) fwt `po` queryFWT k fwt
```

子がないことを`FWTNull`構成子で表していたところを、`undefined`で火遊びしている。

書き換え可能な配列が使えるとき、これらのノードの位置を配列の添え字に落とし込むことができる。
一般的にはフェニック木とはそちらのことをいう。
ここで示した実装は、代数的データ型の操作を伴うので、計算量は同じでも係数が大きい。
Haskellでも可能ならmutable vectorを用いてそのような実装をした方がよいだろうが、
添え字の割付がややこしいので保留。