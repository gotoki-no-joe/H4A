---
order: -01010
---
# Union-Find in "The Algorithms"

[The Algorithms/Haskell/MergeFindSet](https://github.com/TheAlgorithms/Haskell/blob/master/src/SpecializedStructure/MergeFindSet.hs)にとても素直な実装があった。

本書の言葉遣いに直した版を示す。

```haskell
import qualified Data.Map as M
import qualified Data.Set as S

type UnionFind a = (M.Map a a, M.Map a Int)

newUF :: Ord a => [a] -> UnionFind a
newUF xs = (fathers, ranks)
  where
    fathers = M.fromList [(x, x) | x <- xs]
    ranks   = M.fromList [(x, 0) | x <- xs]

getRoot :: Ord a => a -> UnionFind a -> a
getRoot x uf = let father = (fst uf) M.! x
               in  if father == x then x else getRoot father uf

findUF x y uf :: Ord a => a -> a -> UnionFind a -> Bool
findUF x y uf = getRoot x uf == getRoot y uf

uniteUF :: Ord a => a -> a -> UnionFind a -> UnionFind a
uniteUF x y uf@(fathers, ranks)
  | p == q = uf
  | True = case comapre a b of
      LT -> (p `connectTo` q, ranks)
      GT -> (q `connectTo` p, ranks)
      EQ -> (p `connectTo` q, M.adjust succ q ranks) -- M.insert q (succ b) ranks
  where
    (p, a) = (getRoot x uf, ranks M.! p)
    (q, b) = (getRoot y uf, ranks M.! q)
    connectTo a b = M.adjust (const b) a fathers -- M.insert a b fathers
```
`adjust` を意図的に使っているけれど、`insert`を使うのと何が違うのかしら...

経路圧縮もしていないし、性能は期待しない方がよさそう。
`connectTo` を関数に抽出しているところは美しいので見習いたい。
