---
order: -04000
---

# 厳選！C++ アルゴリズム実装に使える 25 の STL 機能

- [前編](https://qiita.com/e869120/items/518297c6816adb67f9a5)
- [後編](https://qiita.com/e869120/items/702ca1c1ed6ff6770257)

競技プログラミングに使う機能A = STL機能の一部B + それ以外で提供されている機能C

Cがあとどれだけあるかわからないが、BはAの一部をなしていることは確かなので、
それがHaskellでどれにあたるかを確認することは、
知っているべき機能の洗い出しに有効だろう。

## 導入編

> 理解するのが簡単なものが中心です。
> 多くの人は知っていると思います。

### 絶対値 abs

`Prelude` にあるから普通に使える。

### 三角関数 sin/cos/tan

`Prelude` にあるから普通に使える。

### 文字列 string

Haskellの高速化で痛いところではあるが、大量のデータを読み込む場面でない、
アルゴリズム本体に限っていえば、`[Char]` であってもそれほど困ることはないだろうか。
長い文字列に `(!!)` を使うべきではないが、それは文字列に限らず全てのリストについて注意することがら。

## 初級編

> 基本的なアルゴリズムの実装に必要なライブラリです。（AtCoder の 100-300 点問題レベルでもよく使われます）
> アルゴリズム以外の場面でも使われることが多いかもしれません。

### 最小値・最大値 min/max

`Prelude` にあるから普通に使える。
対象が2つより多い場合、リストで与える `minimum`, `maximum` がある。これも `Prelude` にある。

### 値の交換 swap

変数の再代入がないのでそんな機能は必要ない。
Vectorの2要素を交換する `exchange` という関数があったりもする。

### 最大公約数 __gcd

`Prelude` に `gcd` も `lcm` もある。

### 乱数 rand

参照透明なので乱数はない。
かなり上級の問題では、確率的アルゴリズムを揺らすために乱数を使うようだが…
古いHaskellでは Random ライブラリがバンドルされていたが、
今は System.Random は自分で入れる必要がある。

### 時間計測 clock

小さなライブラリを読み込む。

```haskell
import System.CPUTime

-- プログラム起動時からの経過時間（ピコ秒）10^-12
getCPUTime :: IO Integer

-- 最小精度
cpuTimePrecision :: Integer
```

### 配列を逆順に並び替え reverse

リストなら `reverse`, ArrayはないがVectorにはある。
区間だけどうこうという便利機能はないが、それいる？

### ソート sort

リストなら `Data.List.sort` その他がある。
区間だけどうこうという便利機能はないが、それいる？

## 中級編第一部

> 基本的なアルゴリズムの実装に必要なライブラリです。（AtCoder の 200-400 点問題レベルでもよく使われます）

### vector

HaskellのVectorとはかなり意味合いが違う。
この辺りまでくると、計算をどう組み立てるかのパラダイムの差が大きくて、
一言で対比することは難しい。

### stack

pushとpopでデータを出し入れするスタックは、
リストを使って自然に実現できる。

### queue

FIFOキューは、
リストの末尾に追加する `list ++ [item]` とか末尾を取りはずす `last`, `init` は重いので、
そういうときは
[Data.Sequence](/library/data.sequence)
を用いる。

### priority_queue

優先度付きキューは、
[Data.Heap](/library/data.heap)がAtCoderでは使える。

### map

ふつう「連想配列」と呼ぶもの。[Data.Map](/library/data.map)が対応する。

### lower_bound

二分探索は、[用意した](/snippets/search/binary-search)。

### set

集合は [Data.Set](/library/data.set) で提供されている。
多重集合はないので、[Data.Map](/library/data.map) で要素数を管理するなどで対応する。

### pair

2タプルですね。

### tuple

タプルですね。

## 中級編第二部

> 第一部ほどではないですが使われるので、知っていて損はないでしょう。
> AtCoder 水色～黄色の上級者でも知らないものがあるかもしれません。

### assert

[Control.Exception.assert](https://qiita.com/mod_poppo/items/b3b415ea72eee210d222)
があるようだが、使う？

### count

あのどこ、にカウンタの話題があった。

### find

- リストにあるかないかは `elem`
- リストにある個数を数えるのはイディオム `length . filter (x ==)` `length . elemIndices` の方がよいのかも。
- どこにあるかは `elemIndex`, `elemIndices`, 対象を述語で指定する `findIndex`, `findIndices`

### next_permutation

順列を与えると、辞書順で次の順列を返すもの。

Vector.Mutable には `nextPermutation` があるが、
これを使うより、数え上げをする生成器を独立させる構造にするべきかと思う。
ただし、`Data.List.permutations` は辞書順を考慮しないので、
辞書順に得たい場合は次のようにする。

```haskell
perm [] = [ [] ]
perm xs = [x:ps | x <- xs, ps <- perm (delete x xs)]

main = mapM_ print (perm [1,2,3])
```

`next_permutation` の実現は、
「なるべく下位桁で、自分より大きい要素がより右にある場所を見つけ、その中の最小の値と差し替え、
残った桁と自分を整列する」とすればよいだろうか。
これ以上大きなものがない状態のとき失敗する必要がある。

```haskell
nextPermutation :: Ord a => [a] -> Maybe [a]
nextPermutation [] = Nothing
nextPermutation (x:xs) = maybe thispos (Just . (x :)) (nextPermutation xs)
  where
    thispos =
      case filter (x <) xs of
        [] -> Nothing
        bs -> let b = minimum bs in Just (b : sort (x : delete b xs))
```

### __builtin_popcount

2進数で表したときの1の個数。[Data.Bits.popCount](/library/data.bits)にある。

### bitset

巨大なビットベクタが使えるらしい。ずるいや。
`Data.IntSet` の中身はビットベクタの木らしいので、代用できないか。

後編の使用例で、ブールのベクタ dp に対して、添え字を Ai だけずらしたものを論理和する

```
foreach j in dp
  if dp[j] then dp[j+Ai] = True
```

を、ビット位置を Ai だけずらした bitset の論理和で表せる

```
dp |= (dp << Ai)
```

としていて、確かに簡潔になってはいる。`Data.IntSet で同じような計算を表現するなら

```haskell
IS.union dp (IS.map (ai +) dp)
```

となるか。
