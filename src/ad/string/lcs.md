# 最長共有部分列

二つの文字列が与えられたとき、一部の文字を取り除いて両者を等しくしたときの最長の文字列、またはその長さを見つける。
つまり、連続した要素でなくて構わないが、順序は変更できない。

## 長さだけ得る版

```haskell
import qualified Data.Vector.Unboxed.Mutable as MUV
import Control.Monad
import Control.Monad.ST

-- @gotoki_no_joe
lcsLen :: Eq t => [t] -> [t] -> Int
lcsLen as bs = runST action
  where
    n = length as
    n1 = succ n
    action :: ST s Int
    action = do
      line0 <- MUV.replicate n1 0
      line1 <- MUV.replicate n1 0
      let step = \(line, line1) bi -> do {
        forM_ (zip [1..n] as) (\(i,ai) ->
          if ai == bi
          then do
            a00 <- MUV.read line (pred i)
            MUV.write line1 i (succ a00)
          else do
            a01 <- MUV.read line i
            a10 <- MUV.read line1 (pred i)
            MUV.write line1 i (max a10 a01)
          );
        return (line1, line)}
      (linen,_) <- foldM step (line0,line1) bs
      MUV.read linen n
```

計算の進行に必要な2行ぶんのメモリだけを Mutable Vector で確保して交互に使いまわしている。
STモナドで計算しているのでIO汚染されていない。

## 長さだけ得るList版

```haskell
import Data.List

-- @gotoki_no_joe
lcsLen as bs = last linen
  where
    line0 = 0 : replicate (length as) 0
    linen = foldl' {-'-} step line0 bs
    step line bi = last line1 `seq` line1
      where
        line1 = 0 : zipWith4 f as line (tail line) line1
        f ai arr00 arr10 arr01
          | ai == bi  = arr01 `seq` succ arr00
          | otherwise = max arr10 arr01
```

畳み込みを `foldl'` で正格評価し、さらに個々の要素をそうするために `seq` を仕込んでいる。

## 列をひとつだけ取り出すList版

```haskell
import Data.List

-- @gotoki_no_joe
lcs :: Eq a => [a] -> [a] -> (Int,[a])
lcs as bs = fmap reverse $ last linen
  where
    zero = (0,[])
    maxx abs cds = if fst abs >= fst cds then abs else cds
    line0 = zero : replicate (length as) zero
    linen = foldl' {-'-} step line0 bs
    step line bi = last line1 `seq` line1
      where
        line1 = zero : zipWith4 f as line (tail line) line1
        f ai (x,ys) c01 c10
          | ai /= bi = c10 `maxx` c01
          | ai == bi = c10 `maxx` c01 `maxx` (succ x, a : ys)
```

aiとbiが等しいとき、確実に最後の項が選ばれ、上や左との比較は無駄なのだが、
これを省略するとスペースリークしてフットプリントが大きくなりすぎる。
（なぜそうなるのかまだよくわからない。）
この式を
```fst c10 `seq` fst c01 `seq` (succ x, a : ys)```
としても挙動が変わる。

`maxx` を単純に `max` に置き換えると `a` に `Ord` が要求される。

## 列をひとつだけ取り出すMutable Vector版

```haskell #
lcs :: Eq t => [t] -> [t] -> (Int,[t])
lcs as bs = runST action
  where
    n = length as
    n1 = succ n
    action = do
      line0 <- MUV.replicate n1 0
      lines <- foldM (step as) [line0] bs
      x <- MUV.read (head lines) n
      ys <- recover n [] (reverse as) (reverse bs) lines
      return (x, ys)
    step as lines bi = do
        let line = head lines
        line1 <- MUV.new n1 ; MUV.write line1 0 0
        forM_ (zip [1..n] as) (\(i,ai) ->
          if ai == bi
          then do
            a00 <- MUV.read line (pred i)
            MUV.write line1 i (succ a00)
          else do
            a01 <- MUV.read line i
            a10 <- MUV.read line1 (pred i)
            MUV.write line1 i (max a10 a01)
          )
        return (line1 : lines)
    recover 0 ys _ _ _ = return ys
    recover _ ys [] _ _ = return ys -- redundant?
    recover _ ys _ [] _ = return ys
    recover i ys aas@(a:as) bbs@(b:bs) lls@(line:lines)
      | a == b = recover (pred i) (a:ys) as bs lines
      | otherwise = do
          a11 <- MUV.read line i
          a01 <- MUV.read line (pred i)
          if a01 == a11
            then recover (pred i) ys as bbs lls
            else recover i ys aas bs lines
```

行8のループで長さを求める過程の表をmutable vectorのリストとして持っておき、
行10で末尾から復元を行っている。

## お話

[あのアルゴリズムはどこ？の33](/readings/whereis/33.lcs)より。
