---
order: -02000
---
# 約数列挙

正整数に対して、割り切ることのできる数を昇順のリストにする。  
与えられた数が平方数 $n^2$ のときに $n$ はひとつだけ出力する。

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

降順に欲しい場合はこちら。

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

[あのアルゴリズムはどこ？の3](/H4A/readings/whereis/03.factors/)より。
