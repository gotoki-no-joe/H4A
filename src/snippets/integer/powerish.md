---
order: -95000
---
# べき乗

自然数の $b$ と結合律を満たす演算 $\times$ に関して、べき乗を一般化した

$$i \times a^b = i \, \overbrace{\times a \times a \times \dots \times a}^{b回}$$

を高速に求める関数。
行列積やモジュロな積で使える。

```haskell
-- @gotoki_no_joe
powerish mul i a b =
    foldl' {-'-} mul i [p | (b,p) <- zip bs ps, odd b]
  where
    bs = takeWhile (0 /=) $ iterate (flip div 2) b
    ps = iterate (\x -> mul x x) a
```
