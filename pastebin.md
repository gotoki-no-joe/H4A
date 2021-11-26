# 切り上げ除算の話

(この辺の解説は多分リンク先にある。どこかで、除算が4種類あるという話を読んだし。)

Haskellには、整数除算は `divMod` と `quotRem` の2つがある。

```
> map (flip divMod (3)) [5,6,7,-5,-6,-7]
[(1,2),(2,0),(2,1),(-2,1),(-2,0),(-3,2)]
> map (flip divMod (-3)) [5,6,7,-5,-6,-7]
[(-2,-1),(-2,0),(-3,-2),(1,-2),(2,0),(2,-1)]
> map (flip quotRem (3)) [5,6,7,-5,-6,-7]
[(1,2),(2,0),(2,1),(-1,-2),(-2,0),(-2,-1)]
> map (flip quotRem (-3)) [5,6,7,-5,-6,-7]
[(-1,2),(-2,0),(-2,1),(1,-2),(2,0),(2,-1)]
```
表に整理すると

```
divMod
    5   6   7   -5   -6   -7 : 被除数
+3  1   2   2   -2   -2   -3 : 商
    2   0   1    1    0    2 : 余り
-3 -2  -2  -3    1    2    2
   -1   0  -2   -2    0   -1

quotRem
    5   6   7   -5   -6   -7 : 被除数
+3  1   2   2   -1   -2   -2 : 商
    2   0   1   -2    0   -1 : 余り
-3 -1  -2  -2    1    2    2
    2   0   1   -2    0   -1
```

除数が正のとき、divModは商を「超えない」値に、すなわち負の無限大方向に寄せている。
一方quotRemは0に寄せている。
被除数も正の範囲ではどちらも変わらず、切り捨てを行っている。


# べき乗mod

```haskell
powerish f a b c = foldl' f c [p | (b,p) <- zip bs ps, odd b]
  where
    bs = takeWhile (0 /=) $ iterate (flip div 2) b
    ps = iterate (\x -> f x x) a
```

```haskell
-- @gotoki_no_joe
powerish mul init a b =
    foldl' mul init [p | (b,p) <- zip bs ps, odd b]
  where
    bs = takeWhile (0 /=) $ iterate (flip div 2) b
    ps = iterate (\x -> mul x x) a
```
