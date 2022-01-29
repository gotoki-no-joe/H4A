---
order: -06000
---
# モジュラ逆数

フェルマーの小定理による方法。
[べき乗](powerish/)を用いている。
また、おそらく他のモジュラ演算も同時に使うため、`mul`の定義はグローバルに置けるだろう。

```haskell
-- @gotoki_no_joe
inv modBase a = powerish mul 1 a (modBase-2)
  where
    mul x y = mod (x*y) modBase
```

ようは $a^{p-2} = 1/a \mod p$ をしている。
この逆数が何かを一度割る (b/a) ことにしか使われない場合、初期値の1をbで置き換えることで少し手間が減る。
$b/a = b * a^{p-2} \mod p$

```haskell
div modBase b a = powerish mul b a (modBase-2)
  where
    mul x y = mod (x*y) modBase
```

拡張ユークリッドの互除法を使う方法もある。

```haskell
modRecip (ModInt a) =
   toEnum $ you $ head $ dropWhile cond $ iterate step (a, modBase, 1, 0)
  where
    step (a,b,u,v) =
      let t = a `div` b
      in  (b, a - t * b, v, u - t * v)
    cond (_,b,_,_) = b /= 0
    you (_,_,u,_) = u
```

[あのアルゴリズムはどこ？の14](/H4A/readings/whereis/14.modrecip/)、およびアルゴリズムロジックより。
