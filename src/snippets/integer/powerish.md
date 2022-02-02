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

少し改善。

```haskell
import Data.Bits

bitSeq n = map (flip testBit 0) $ takeWhile (0 /=) $ iterate (flip shiftR 1) n

-- @gotoki_no_joe
powerish mul a = \i b -> foldl' {-'-} mul i [p | (True, p) <- zip (bitSeq b) ps]
  where
    ps = iterate (\x -> mul x x) a
```

bitSeqは Data.Bits を使わない下の書き方でも、最適化でやることは同じになる気もする。

```haskell
bitSeq :: Int -> [Bool]
bitSeq = map odd . takeWhile (0 /=) . iterate (flip div 2)
```
