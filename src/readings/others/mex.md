---
order: -01000
---

# MEX

[Mex - Wikipedia.en](https://en.wikipedia.org/wiki/Mex_(mathematics))

> In mathematics, the mex of a subset of a well-ordered set is the smallest value from the whole set that does not belong to the subset. That is, it is the minimum value of the complement set. The name "mex" is shorthand for "minimum excluded" value.

数学において、順序集合の部分集合に対するmexとは、
全体集合に含まれてその部分集合に含まれない最小の要素である。
つまり補集合の最小値である。
mexとは minimum exluded の略である。

[要素の追加・削除と mex を対数時間で処理するよ えびちゃんの日記](https://rsk0315.hatenablog.com/entry/2020/10/11/125049) をなぞる。

## 表現

対象を整数に限定する。

部分集合を表現する方法を考える。

1. `IntSet`はまさに整数の集合を表すためのものである。
2. ネタ元では、区間の下限と上限 $[a,b)$ の対の集合を用いている。
3. 下限をキー、値を上限とするマップで同様のことが実現できる。
4. 下限と上限の両方をキーとし、そのどちらであるかのタグを値とするマップでも実現できる。

それぞれで $\{-3,-2,0,1,2,3,7\}$ を表すと次のようになる。

```haskell
type MexSet1 = IS.IntSet
s1 = IS.fromList [-3,-2,0,1,2,3,7] :: MexSet1

type MexSet2 = S.Set (Int, Int)
s2 = S.fromList [(-3,-1), (0,4), (7,8)] :: MexSet2

type MexSet3 = IM.IntMap Int
s3 = IM.fromList [(-3,-1), (0,4), (7,8)] :: MexSet3

type MexSet4 = IM.IntMap Bool
s4 = IM.fromList [(-3,True),(-1,False),(0,True),(4,False),(7,True),(8,False)] :: MexSet4
```

## member

手始めに、ある要素が含まれるかどうかを判定する。

```haskell
memberMEX1 x ms = IS.member x ms

memberMEX2 x ms =
  case S.lookupLE (x, maxBound) ms of -- または S.lookupLT (succ x, minBound)
    Nothing -> False
    Just (y,z) -> x < z

memberMEX2 x ms = maybe False ((x <) . snd) (S.lookupLE (x, maxBound) ms)

memberMEX3 x ms = maybe False ((x <) . snd) (IM.lookupLE x ms)

memberMEX4 x ms = maybe False snd (IM.lookupLE x ms)
```

方式3では、関心の値 x 以下のキーを探す `lookupLE` で、xを含む可能性のある区間のキーと値、
すなわち下限と上限が取り出せる。  
方式2は方式3とよく似ているが、`lookupLE` などを使うための値を対にする必要があるため、
右側の値に少し配慮が必要になる。

<!--
方式2の古い説明

範囲 $[y,z)$ が $x$ を含むとは、 $y \le x \land x < z$ である。  
つまり、`x` を含む範囲らしいものを見つけ出すには、
左が`x`以下のものを探し出す必要がある。

例えば今 x=3 として、`lookupLE (3,3)` としたとする。
集合にある3を覆う範囲が `(2,7)` ならば `(2,7) < (3,3)` で無事に見つかるが、
集合に入っている対が `(3,7)` だったときは `(3,7) > (3,3)` で `lookupLE` では見つけられない。
つまり、`lookupLE (3,maxBound)` のようにする必要がある。

あるいは、`lookupLT (4,3)` としてみる。
集合にある、左が4であるような要素は右は必ずそれより大きいので、誤検知することはなく、
左が3以下である要素をこれで確実に見つけ出せる。
-->

## insert

値 x を追加することを考える。  
区間で表す方式2,3,4で、単純に $[x,x+1)$ を追加するのでは、
全ての区間がぶつ切りになって意味がないので、
前後の区間の延長、あるいは両者の連結ができる場合はそうする必要がある。  
なお、元々xが含まれている場合には何もしない。

```haskell
insertMEX1 x ms = IS.insert x ms

insertMEX2 x ms
  | member x = ms
  | expandLow && expandHigh = S.insert (fst ab, snd cd) $ S.delete ab $ S.delete cd ms
  | expandLow               = S.insert (fst ab, x1)     $ S.delete ab               ms
  |              expandHigh = S.insert (x, snd cd)                    $ S.delete cd ms
  | otherwise               = S.insert (x, x1) ms
  where
    x1 = succ x
    lowPair    = S.lookupLT (succ x, minBound) ms
    highPair   = S.lookupGT (x, maxBound) ms
    expandLow  = maybe False ((x  ==) . snd) lowPair
    expandHigh = maybe False ((x1 ==) . fst) highPair
    Just ab = lowPair
    Just cd = highPair
    memberx = maybe False ((x <) . snd) lowPair

insertMEX3 x ms
  | memberx = ms
  | expandLow && expandHigh = IM.insert a d  $ IM.delete a $ IM.delete c ms
  | expandLow               = IM.insert a x1 $ IM.delete a               ms
  |              expandHigh = IM.insert x d                $ IM.delete c ms
  | otherwise               = IM.insert x x1 ms
  where
    x1 = succ x
    lowPair    = IM.lookupLE x ms
    highPair   = IM.lookupGT x ms
    expandLow  = maybe False ((x  ==) . snd) lowPair
    expandHigh = maybe False ((x1 ==) . fst) highPair
    Just (a,_) = lowPair
    Just (c,d) = highPair
    memberx = maybe False ((x <) . snd) lowPair

insertMEX4 x ms
  | memberx = ms
  | expandLow && expandHigh = IM.delete x        $ IM.delete x1 ms
  | expandLow               = IM.insert x1 False $ IM.delete x  ms
  |              expandHigh = IM.insert x  True  $ IM.delete x1 ms
  | otherwise               = IM.insert x1 False $ IM.insert x  True ms
  where
    x1 = succ x
    low  = IM.lookupLE x ms
    high = IM.lookupGT x ms
    expandLow  = maybe False ((x  ==) . fst) low
    expandHigh = maybe False ((x1 ==) . fst) high
    memberx = maybe False snd low
```

## mex

いよいよ核心の、下限x以上で、含まれていない最小の値を探すことを考える。

集合そのものを用いる方法では、xから順に、それが含まれていないかどうかを調べていくしかないので $O(n)$ かかる。

区間を用いる方法では、
xが含まれていないときはそれが答えである。
xが含まれているときは、xを含む区間の上限が答えである。

```haskell
mexMEX1 x ms = until (flip IS.notMember ms) succ x

mexMEX2 x ms =
  case S.lookupLE (x, maxBound) ms of
    Nothing -> x
    Just (_,b) -> if x < b then b else x

memMEX3 x ms =
  case S.lookupLE (x, maxBound) ms of
    Nothing -> x
    Just (a,b) -> if x < b then b else x

mexMEX4 x ms =
  case IM.lookupGT x ms of
    Just (b,False) -> b
    _ -> x
```

## 関連問題

[HHKB プログラミングコンテスト 2020 C Neq Min](https://atcoder.jp/contests/hhkb2020/tasks/hhkb2020_c)

要素を追加しながらその都度MEXを求めよという要求。

あろうことか、（全体集合が固定されていることを利用して）
関心の集合の補集合そのものを `IntSet` で管理したやり方で
[普通にAC](https://atcoder.jp/contests/hhkb2020/submissions/19442398)してしまっていた。

[方式4でAC](https://atcoder.jp/contests/hhkb2020/submissions/29931978)
上の方がメモリも軽いし速いってどういうことなの…

[ABC194 E Mex Min](https://atcoder.jp/contests/abc194/tasks/abc194_e)

しゃくとり法で、mexからの要素の除去が必要になる。  
挿入とは逆に、区間を二分するのか、端を切り詰めるのか、長さ1の区間を取り除くのか、判別が必要。

```haskell
deleteMEX4 x ms
  | notmemberx = ms
  | shrinkLow && shrinkHigh = IM.delete x       $ IM.delete x1 ms
  | shrinkLow               = IM.insert x False $ IM.delete x1 ms
  |              shrinkHigh = IM.insert x1 True $ IM.delete x  ms
  | otherwise               = IM.insert x1 True $ IM.insert x False ms
  where
    x1 = succ x
    low  = IM.lookupLE x ms
    high = IM.lookupGT x ms
    shrinkLow  = maybe False ((x1 ==) . fst) high
    shrinkHigh = maybe False ((x  ==) . fst) low
    notmemberx = not $ maybe False snd low
```

ぎりぎり3990msで[ACした](https://atcoder.jp/contests/abc194/submissions/29933199)。
が、このアプローチは間違いで、これは関連問題ではないらしい。
タイトルでひっかかりました。

[想定解ACコード](https://atcoder.jp/contests/abc194/submissions/29934180) ただの命令型プログラムだこれ。

## Range Update Query

配列に対して、区間を指定しての一括更新と要素の問い合わせを繰り返す問題を、
この区間配列の応用で解ける、と書いてある。

[Range Update Query](https://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=DSL_2_D&lang=ja)

要素が保持する情報のビット数だけmex集合を用意しそれぞれのビットに割り当て、
更新を、その範囲のビットの0/1上書きにするために、区間追加と区間削除を用いる、と言っているようだ。

しかしそれなら、方式4のBool値の代わりに、個々以上の値は（次に言い換えられるまで）この値である、
という形で、区間で値を記憶し、区間を更新する仕組みを作る方が素直ではないか。

## その他関連問題

どこにどういう風にmexが絡むのかピンとこない。区間による集合の話か？うーむ…

- [ARC039 C 幼稚園児高橋君](https://atcoder.jp/contests/arc039/tasks/arc039_c)
- [AOJ 2880 Elevator](http://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=2880)
