# セグメント木

一次元配列があり、その様々な区間に対して特定の演算を行った結果を何度も求めたり、
配列の要素を更新したりする操作が $O(\log n)$ で行えるデータ構造。  

## Immutable Tree版

```haskell
-- @gotoki_no_joe
data SegmentTree a = SegmentTree Int (a->a->a) a (STree a)

data STree a = Leaf a | Node a (STree a) (STree a)

-- セグメント木を作る
-- 引数：データ列, モノイド演算, 単位元
makeSegTree :: [a] -> (a->a->a) -> a -> SegmentTree a
makeSegTree xs op u = SegmentTree w op u t
  where
    len = length xs
    w = head $ dropWhile (len >) $ iterate (2 *) 1
    t = loop w (xs ++ repeat u)
    loop 1 (x:_) = Leaf x
    loop w xs = mkNode op lt rt
      where
        w2 = div w 2
        lt = loop w2 xs
        rt = loop w2 $ drop w2 xs

-- utilities
getVal (Leaf x) = x
getVal (Node x _ _) = x

mkNode op lt rt = Node (getVal lt `op` getVal rt) lt rt

-- 第j要素(0始まり)をxに変更する
updateSegTree :: SegmentTree a -> Int -> a -> SegmentTree a
updateSegTree (SegmentTree w op u t) j x = SegmentTree w op u t1
  where
    t1 = loop w j x t
    loop 1 _ x _ = Leaf xoldval
    loop w j x (Node _ lt rt)
      | j < w2 = mkNode op lt1 rt
      | True   = mkNode op lt rt1
      where
        w2 = div w 2
        lt1 = loop w2 j x lt
        rt1 = loop w2 (j-w2) x rt

-- 木の第a要素から第b要素の手前までを演算した結果を求める
querySegTree :: SegmentTree a -> Int -> Int -> a
querySegTree (SegmentTree w op u t) a b = loop 0 w w t
  where
    loop p q _ t
      | b <= p || q <= a = u
      | a <= p && q <= b = getVal t
    loop l r w (Node _ lt rt)
      | otherwise = loop p m w2 lt `op` loop m q w2 rt
      where
        w2 = div w 2
        m = p + w2
```

## Mutable Vector版

```haskell
import Control.Monad
import qualified Data.Vector.Unboxed.Mutable as MUV

data SegmentTree a = SegmentTree Int (a->a->a) a (STree a)

type STree a = MUV.IOVector a

makeSegTree :: MUV.Unbox a => [a] -> (a->a->a) -> a -> IO (SegmentTree a)
makeSegTree xs op u = do
  let len = length xs
  let w = head $ dropWhile (len >) $ iterate (2 *) 1
  vec <- MUV.new (w * 2 - 1)
  forM_ (zip [0..pred w] (xs ++ repeat u))
        (\(i,x) -> MUV.write vec (i + w - 1) x)
  forM_ [w-2,w-1..0] (\k -> do
    l <- MUV.read vec (k*2+1)
    r <- MUV.read vec (k*2+2)
    MUV.write vec k (op l r)
    )
  return $ SegmentTree w op e vec

updateSegTree :: MUV.Unbox a => SegmentTree a -> Int -> a -> IO ()
updateSegTree (SegmentTree w op _ vec) j x =
  do
    MUV.write vec i0 x
    forM_ upwards (\i -> do
      l <- MUV.read vec (i*2+1)
      r <- MUV.read vec (i*2+2)
      MUV.write vec i (op l r)
      )
  where
    i0 = j + w - 1
    upwards = tail $ takeWhile (0 <=) $ iterate step i0
    step i = div (pred i) 2

querySegTree :: MUV.Unbox a => SegmentTree a -> Int -> Int -> IO a -- [左,右)で範囲を指定
querySegTree (SegmentTree w op u vec) a b = loop vec 0 w w 0
  where
    loop vec p q w i
      | q <= a || b <= p = return u
      | a <= p && q <= b = MUV.read vec i
      | otherwise = do
        let w2 = div w 2
        let m = p + w2
        l <- loop vec p m w2 (i*2+1)
        r <- loop vec m q w2 (i*2+2)
        return (op l r)
```

- [あのアルゴリズムはどこ？の29](/readings/whereis/29.segment-tree)
- アルゴリズムロジック
- ACL

より。
