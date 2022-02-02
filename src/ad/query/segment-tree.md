# セグメント木

一次元配列があり、その様々な区間に対して特定の演算を行った結果を何度も求めたり、
配列の要素を更新したりする操作が $O(\log n)$ で行えるデータ構造。  

```haskell
-- @gotoki_no_joe
data SegmentTree a = SegmentTree Int (a->a->a) a (STree a)

data STree a = Leaf a | Node a (STree a) (STree a)

-- セグメント木を作る
makeSegTree :: [a]            -- データ列
            -> (a->a->a)      -- モノイド演算
            -> a              -- 単位元
            -> SegmentTree a
makeSegTree xs op i = SegmentTree w op i t
  where
    len = length xs
    w = head $ dropWhile (len >) $ iterate (2 *) 1
    t = loop w (xs ++ repeat i)
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

-- 木の第j要素(0始まり)をxに変更する
updateSegTree :: SegmentTree a -> Int -> a -> SegmentTree a
updateSegTree (SegmentTree w op i t) j x = SegmentTree w op i t1
  where
    t1 = loop w j x t
    loop 1 _ x _ = Leaf x -- arg2 = 0, arg4 = Leaf oldval
    loop w j x (Node _ lt rt)
      | j < w2 = mkNode op lt1 rt
      | True   = mkNode op lt rt1
      where
        w2 = div w 2
        lt1 = loop w2 j x lt
        rt1 = loop w2 (j-w2) x rt

-- 木の第i要素から連続するj要素に対する演算結果を求める
querySegTree :: SegmentTree a -> Int -> Int -> a
querySegTree (SegmentTree w op i t) p q
  | q == 0 = i
  | True   = loop w t p q
  where
    loop w t 0 q | w == q = getVal t
    loop w (Node _ lt rt) p q
      | p+q <= w2 = loop w2 lt p q
      | w2  <= p  = loop w2 rt (p-w2) q
      | True = loop w2 lt p q1 `op` loop w2 rt 0 (q-q1)
      where
        w2 = div w 2
        q1 = w2-p
```

- [あのアルゴリズムはどこ？の29](/readings/whereis/29.segment-tree)
- アルゴリズムロジック
- ACL

より。
