---
order: -05000
---
# 三分探索

難しい言い方をすると

https://qiita.com/DaikiSuyama/items/5ab08c1581af238de909

> 全順序集合上に定義されている狭義凸関数 $f$ に対して
> $f(x)$ が最小値をとる $x$ を求めるアルゴリズム

https://qiita.com/ganyariya/items/1553ff2bf8d6d7789127

原理の図解

最小値を挟む2点から始め、3等分した2点の $f$ の値で大きいほうを縮めることを繰り返すと、
範囲が狭まり、追い詰めることができる。

```haskell
-- @gotoki_no_joe
ternarySearch :: Ord a
              => (Double -> a)      -- 目的関数
              -> Double             -- 許容誤差ε
              -> Double -> Double   -- 左と右の範囲
              -> (Double, a)        -- 最小値をとる引数とその最小値
ternarySearch f epsilon l0 r0 = end $ until stop step (l0, r0, f l0)
  where
    end (l, r, fl) = (l, fl)
    step (l,r, fl)
      | fc1 < f c2 = (l, c2, fl)
      | otherwise  = (c1, r, fc1)
      where
        c1 = (l * 2 + r) / 3
        c2 = (l + r * 2) / 3
        fc1 = f c1
    stop (l,r,_) = r - l < epsilon
```

追い詰めきれない場合に無限ループにならないように、反復の上限を設定した版。

```haskell
ternarySearch :: Ord a
              => (Double -> a)      -- 目的関数
              -> Double             -- 許容誤差ε
              -> Int                -- 反復打ち切り回数
              -> Double -> Double   -- 左と右の範囲
              -> Maybe (Double, a)  -- 最小値をとる引数とその最小値
ternarySearch f lim epsilon l r
  | b = Just (x, fx)
  | otherwise = Nothing
  where
    (_,x,_,fx,b) = until stop step (0,l,r,f l,False)
    step (n,l,r,fl,_)
      | fc1 < f c2 = (succ n, l, c2, fc1, ok l c2)
      | otherwise  = (succ n, c1, r, fl , ok c1 r)
      where
        c1 = (l * 2 + r) / 3
        c2 = (l + r * 2) / 3
        fc1 = f c1
    ok l r = r - l >= epsilon
    stop (n,_,_,_,b) = b || n >= lim
```

### 関連問題

- [ARC122 B Insurance](https://atcoder.jp/contests/arc122/tasks/arc122_b) - 【ACコード】
- [ARC049 B 高橋ノルム君](https://atcoder.jp/contests/arc049/tasks/arc049_b) - 【ACコード】
- [ARC045 B ムーアの法則](https://atcoder.jp/contests/arc054/tasks/arc054_b)
- [ABC151 F Enclose All](https://atcoder.jp/contests/abc151/tasks/abc151_f) - [ACコード](https://atcoder.jp/contests/abc151/submissions/28803687)
- [ABC102 D - Equal Cut](https://atcoder.jp/contests/abc102/tasks/arc100_b)

<!--
ABC151Fについて、X軸を動かして探す最小点は、そこでスライスした最小値なので、
Xをどこに取っていても、底をなぞってY方向の最小値に向かう
-->

他サイト

- [yukicoder No.198 キャンディー・ボックス２](https://yukicoder.me/problems/no/198)
- [yukicoder No.306 さいたま2008](https://yukicoder.me/problems/no/306)
