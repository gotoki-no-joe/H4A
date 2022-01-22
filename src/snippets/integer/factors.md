---
order: -02000
---
# 約数列挙

```haskell
-- @gotoki_no_joe
factors :: Int -> [Int]
factors 1 = [1]
factors n = 1 : loop 2 [n]
  where
    loop k us
      | k2 >  n =     us
      | k2 == n = k : us
      | r  == 0 = k : next (q:us)
      | True    =     next    us
      where
        (q,r) = divMod n k
        next = loop (succ k)
        k2 = k * k
```

#### 逆順版

```haskell
-- @gotoki_no_joe
factorsR :: Int -> [Int]
factorsR 1 = [1]
factorsR n = n : loop 2 [1]
  where
    loop k us
      | k2 >  n =     us
      | k2 == n = k : us
      | r  == 0 = q : next (k:us)
      | True    =     next    us
      where
        (q,r) = divMod n k
        next = loop (succ k)
        k2 = k * k
```

## 説明

正整数に対して、割り切ることのできる数を昇順のリストにする。
与えられた数が平方数 $n^2$ のときに $n$ はひとつだけ出力する。

## お話

あのアルゴリズムはどこ？の3より。

1から順に対象の数を割ってみて、割り切れれば約数である。

```haskell
factors n = [k | k <- [1..n], mod n k == 0]
```

ここで、

- あるkでnが割り切れたとき、kだけでなくn/kも（大きい方の）約数である。
- $\sqrt n < k < n$ の範囲には、小さい方の約数は存在しない。

を利用すると、上のように、$\sqrt n$ より大きい後半の約数を蓄積しながら、
$\sqrt n$ 以下の約数を順に見つける、 $O(\sqrt n)$ のコードができる。
これ以上の性能を求めるなら、フェルマーの小定理を使う方法というものがあるらしい。

### 関連問題

- [ABC180 C Cream puff](https://atcoder.jp/contests/abc180/tasks/abc180_c) [ACコード](https://atcoder.jp/contests/abc180/submissions/22727220)
- [ABC112 D Partition](https://atcoder.jp/contests/abc112/tasks/abc112_d) - [ACコード](https://atcoder.jp/contests/abc112/submissions/23709880)
- [ABC190 D Staircase Sequences](https://atcoder.jp/contests/abc190/tasks/abc190_d) - [ACコード](https://atcoder.jp/contests/abc190/submissions/23710955)
- [diverta 2019 D - DivRem Number](https://atcoder.jp/contests/diverta2019/tasks/diverta2019_d) - [ACコード](https://atcoder.jp/contests/diverta2019/submissions/27485495)
- [ARC108 A Sum and Product](https://atcoder.jp/contests/arc108/tasks/arc108_a) - [ACコード](https://atcoder.jp/contests/arc108/submissions/23711077)

他サイトなので未解答
- [MojaCoder Polygon of Polygons](https://mojacoder.app/users/magurofly/problems/polygon-of-polygons)
