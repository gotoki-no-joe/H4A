# Data.List

覚えていて当たり前な関数は省略し、時々必要になる便利な関数の覚え書き。

```haskell
-- 間に挟み込む ex) unwords = intercalate " "
intersperse ::  a  ->  [a]  -> [a]
intercalate :: [a] -> [[a]] -> [a]

-- 部分列
-- subsequences "abc" = ["","a","b","ab","c","ac","bc","abc"]
subsequences :: [a] -> [[a]]

-- 順列
-- permutations "abc" = ["abc","bac","cba","bca","cab","acb"]
permutations :: [a] -> [[a]]

-- 分割
-- span p xs = (takeWhile p xs, dropWhile p xs)
span :: (a -> Bool) -> [a] -> ([a], [a])

-- 選り分け
-- partition p xs == (filter p xs, filter (not . p) xs)
partition :: (a -> Bool) -> [a] -> ([a], [a])

-- 特別な整列
-- sortOn f = sortBy (comparing f)
sortOn :: Ord b => (a -> b) -> [a] -> [a]
```

本当はTraversableだけどリストに特化して解釈

```haskell
mapAccumL :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
mapAccumL f s []     = (s, [])
mapAccumL f s (x:xs) = (u, y:ys)
  where (t, y ) = f s x
        (u, ys) = mapAccumL f t xs
```

状態sと入力列[x]から状態遷移を繰り返し、同時に出力列[y]も生成する、状態遷移機械の一般化。

LがあるならRもある。

```haskell
mapAccumR :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
mapAccumR f s []     = (s, [])
mapAccumR f s (x:xs) = (u, y:ys)
  where (t, ys) = mapAccumR f s xs
        (u, y ) = f t x
```

fold, scan, accumの関係を図にしてみた。

![](/static/foldl.svg)

![](/static/foldr.svg)
