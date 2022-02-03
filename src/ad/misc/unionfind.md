---
order: -01000
---
# Union-Find

集合の要素を、交わりのない部分に分割する。
全ての要素が、自分だけが属する分割にいる状態から始める。
「以後、要素aとbは同じ分割に属する」という分割の統合(union)と、
「現在、要素aとbは同じ分割に属しているか？」という問い合わせ(find)を
効率的に行えるデータ構造およびアルゴリズム。

## コード

0からn-1までのn個の整数を対象としたUnion-Findの、mutable vectorによる実装。

```haskell API
-- Union-Find構造体を作る
newUF :: Int -> IO UnionFind
-- 同じ分割に属するときTrue
findUF :: UnionFind -> Int -> Int -> IO Bool
-- 統合する。元々同じ分割に属していたらFalseを返す
uniteUF :: UnionFind -> Int -> Int -> IO Bool
-- 代表元を得る補助関数
getRoot :: UnionFind -> Int -> IO Int
```

```haskell
import qualified Data.Vector.Unboxed.Mutable as MUV
import Control.Monad

type UnionFind = MUV.IOVector Int

newUF :: Int -> IO UnionFind
newUF n = MUV.replicate n (-1)

getRoot :: UnionFind -> Int -> IO Int
getRoot vec i = loop i []
  where
    loop :: Int -> [Int] -> IO Int
    loop i ks =
      do
        k <- MUV.read vec i
        if k < 0 then
          do
            forM_ ks (\k -> do MUV.write vec k i)   -- 経路圧縮
            return i
          else
            loop k (i:ks)

findUF :: UnionFind -> Int -> Int -> IO Bool
findUF vec i j =
 do
    a <- getRoot vec i
    b <- getRoot vec j
    return (a == b)

uniteUF :: UnionFind -> Int -> Int -> IO Bool
uniteUF vec i j =
  do
    a <- getRoot vec i
    b <- getRoot vec j
    if a == b then return False else
      do
        r <- MUV.read vec a
        s <- MUV.read vec b
        if r < s then
          do
            MUV.write vec a b
            MUV.write vec b (r+s)
          else do
            MUV.write vec b a
            MUV.write vec a (r+s)
        return True
```

## 任意のペイロード版

経路圧縮を行う場合、木の高さや大きさはほとんど意味がないと判断し、
それぞれの分割に対して任意の情報を添付できるようにした版。  
ABC183Fのために開発。

```haskell API
-- 初期の孤独な分割に対する情報のリストを与える
newUF :: [a] -> IO (UnionFind a)
-- 代表元の番号と、分割に付けられた情報の両方を返す
getRoot :: UnionFind a -> Int -> IO (Int, a)
-- 同じ分割か判定する
findUF :: UnionFind a -> Int -> Int -> IO Bool
-- 分割を統合する。初めから同じ分割であったらFalseを返す
-- 統合した場合に、両者の情報を統合する関数を第1引数に与える
uniteUF :: (a -> a -> a) -> UnionFind a -> Int -> Int -> IO Bool
```

```haskell
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as V

-- @gotoki_no_joe
type UnionFind a = MV.IOVector (Either Int a)

newUF :: [a] -> IO (UnionFind a)
newUF = V.thaw . V.fromList . map Right

getRoot :: UnionFind a -> Int -> IO (Int, a)
getRoot vec i = loop vec i []
  where
    loop vec i ks =
      do
        k <- MV.read vec i
        case k of
          Right x -> do
            forM_ ks (\k -> MV.write vec k (Left i))
            return (i, x)
          Left j -> loop vec j (i:ks)

findUF :: UnionFind a -> Int -> Int -> IO Bool
findUF vec i j =
 do
    (a, _) <- getRoot vec i
    (b, _) <- getRoot vec j
    return (a == b)

uniteUF :: (a -> a -> a) -> UnionFind a -> Int -> Int -> IO Bool
uniteUF f vec i j =
  do
    (a, r) <- getRoot vec i
    (b, s) <- getRoot vec j
    if a == b then return False else
      do
        MV.write vec a (Left b)
        MV.write vec b (Right $ f r s)
        return True
```

[あのアルゴリズムはどこ？の7](/readings/whereis/07.unionfind/)より。

AtCoder Class Library の
[DSU](https://atcoder.github.io/ac-library/production/document_ja/dsu.html)
とは、Disjoint Set Union, 素集合データ構造のことで、つまり同じもの。
