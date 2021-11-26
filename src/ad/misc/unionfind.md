---
---
# Union-Find

集合の要素を、交わりのない部分に分割する。
全ての要素が、自分だけが属する分割にいる状態から始める。
「以後、要素aとbは同じ分割に属する」という分割の統合(union)と、
「現在、要素aとbは同じ分割に属しているか？」という問い合わせ(find)を
効率的に行えるデータ構造およびアルゴリズム。

## コード

```haskell
import qualified Data.Vector.Unboxed.Mutable as MUV
import Control.Monad

-- @gotoki_no_joe
type UnionFind = MUV.IOVector Int

newUF :: Int -> IO UnionFind
newUF ub = MUV.replicate (succ ub) (-1)

getRoot :: UnionFind -> Int -> IO Int
getRoot vec i = loop i []
  where
    loop :: Int -> [Int] -> IO Int
    loop i ks =
      do
        k <- MUV.read vec i
        if k < 0 then
          do
            forM_ ks (\k -> do MUV.write vec k i)
            return i
          else
            loop k (i:ks)

findUF :: UnionFind -> Int -> Int -> IO Bool
findUF vec i j =
 do
    a <- getRoot vec i
    b <- getRoot vec j
    return (a == b)

uniteUF :: UnionFind -> Int -> Int -> IO ()
uniteUF vec i j =
  do
    a <- getRoot vec i
    b <- getRoot vec j
    if a == b then return () else
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
```

## 説明

参考
- [Wikipedia 素集合データ構造](https://ja.wikipedia.org/wiki/%E7%B4%A0%E9%9B%86%E5%90%88%E3%83%87%E3%83%BC%E3%82%BF%E6%A7%8B%E9%80%A0)
- [PythonでのUnion-Find（素集合データ構造）の実装と使い方](https://note.nkmk.me/python-union-find/)

集合の要素を添え字とする配列を用意し、
内容には、自分と同じ分割に属する（成り行き上の）代表元に至る木をなすような、親へのリンクを保持する。

命令型配列の、 $O(1)$ の高速なランダムアクセス、特にリンクの書き換えがアルゴリズムの要であるため、
`Data.Array.(//)` や `Data.Vector.(//)` での模倣は性能が得られないが、
はじめは説明のために Array で作ってみよう。

### 素朴な実装

集合の要素を上限ubまでの0以上の整数と仮定する。
配列の添え字は0からubまで、内容は同じ分割に属する他の要素の番号、
ただし自分が代表元（木の根）であるとき自分自身とする。
（これを代数的データ型のタグで見分けてもよいが、そこまで行儀よく作ることもない。）

```haskell
import Data.Array

type UnionFind = Array Int Int
```

配列の初期値は、全て自分自身の値を持つことになる。

```haskell
newUF :: Int -> UnionFind
newUF ub = listArray (0,ub) [0..ub]
```

指定の番号の要素から、その要素が属する分割の代表元の番号を得る操作が普通に欲しいだろう。

```haskell
getRoot :: UnionFind -> Int -> Int
getRoot uf i = let k = uf ! i in if i == k then i else getRoot uf k
```

ふたつの要素を指定してのfindは、リンクを辿ることでそれぞれの代表元を求め、等しければ同じ分割にある。

```haskell
findUF :: UnionFind -> Int -> Int -> Bool
findUF uf a b = getRoot uf a == getRoot uf b
```

ふたつの要素を指定してのunionは、やはりまずそれぞれの代表元を求め、
（それらが異なるとき）いずれかの親をもう一方に更新して、根に接ぎ木する。

```haskell
uniteUF :: UnionFind -> Int -> Int -> UnionFind
uniteUF uf i j = let a = getRoot uf i
                     b = getRoot uf j
                 in if a == b then uf else uf // [(a,b)]
```

### 木の高さを考慮した効率化

上の素朴な実装では、union操作の順により、木が一本道になりうる。
このときgetRootに $O(n)$ かかり、効率が稼げない。
木を極力低く保つ必要がある。
その対処は二通り知られており、両方を組み合わせることもできるが、片方だけでもそれなりに動作するらしい。

ひとつは経路圧縮という。
getRootの際に通過した要素全てについて、それらを代表元の直接の子になるようにリンクをつなぎ直す。
これはimmutableなgetRootに配列の（意味を変更しないが）mutableな変更を副作用として持ち込むもので、
Haskellとは相性が非常によくない。

もうひとつは、union操作における接ぎ木をする際に木の高さを意識して、
高い方を根にし、低い方の木をその子にすることである。
両方の木の高さを $h_1 > h_2$ とすると、結果の木の高さは $\max(h_1, h_2+1) = h_1$ となる。
$h_1 = h_2$ のとき、（どちらを根にしても）結果の木の高さは $h_1+1$ となる。
unionは元々UnionFind配列に対する更新操作なので、これは導入するべきである。
この場合木の高さが $O(\log n)$ で抑えられるらしい。

木の高さを参照する必要があるのは、根（代表元）に関してのみである。
分割の全ての要素が自分の木の高さを知っている必要はない。
ここで、配列の要素を `Either (木の高さ) (親の番号)` とする代わりに、
符号をタグ代わりに「0以上の数ならば親の番号、負の数ならば木の高さにマイナスをつけたもの」と
横着をすることができる。

```haskell
newUF :: Int -> UnionFind
newUF ub = listArray (0,ub) $ replicate (succ ub) (-1)

getRoot :: UnionFind -> Int -> Int
getRoot uf i = let k = uf ! i in if 0 > k then i else getRoot uf k

uniteUF :: UnionFind -> Int -> Int -> UnionFind
uniteUF uf i j
  | a == b = uf
  | True   = case compare r s of
               GT -> uf // [(a,b)]
               LT -> uf // [(b,a)]
               EQ -> uf // [(b,a), (a, pred r))]
  where
    a = getRoot uf i
    r = uf ! a
    b = getRoot uf k
    s = uf ! b
```

### mutable vector化と経路圧縮

上の `(//)` を使うコードは、説明としては見通しがよいが、効率の点で劣る。
このため、 `Data.Vector.Mutable` を用いる形に直す。
mutable vectorは読みだすだけでアクションになるため、
同時に `getRoot` で経路圧縮も行う。
そうするとき、均すために木の高さを追跡しても、経路圧縮により実際にはそれより木が低くなっていることも多い。
そこで、木の高さと相関のありそうな値として大きさ、要素数を追跡し、unionで接ぎ木をする際には、大きい方を根にする。

結果がページトップに示したコードである。

### 全ての分割

全ての要素について、同じ分割に属するものを集めたリストを得たい場合は、
getRootをキーとするMapに集めるとよい。

```haskell
allDivisions :: UnionFind -> IO [[Int]]
allDivisions uf =
  do
    ri <- forM [0..ub] (\i -> do
      r <- getRoot uf i
      return (r, i)
      )
    return $ IM.elems $ IM.fromListWith (++) [(r,[i]) | (r,i) <- ris]
```

### DSU

AtCoder Class Library の [DSU](https://atcoder.github.io/ac-library/production/document_ja/dsu.html) は同一の機能を提供する。裏にあるデータ構造が何かは不明。

## 関連問題

- [ABC177 D Friends](https://atcoder.jp/contests/abc177/tasks/abc177_d) - [ACコード](https://atcoder.jp/contests/abc177/submissions/22742331) 旧タイプ
- [ACL Beginner Contest C](https://atcoder.jp/contests/abl/tasks/abl_c) - [ACコード](https://atcoder.jp/contests/abl/submissions/27497827)
- [ARC106 B Values](https://atcoder.jp/contests/arc106/tasks/arc106_b) - 【ACコード】
- [ARC114 B Special Subsets](https://atcoder.jp/contests/arc114/tasks/arc114_b) - 【ACコード】
- [ABC157 D Friend Suggestions](https://atcoder.jp/contests/abc157/tasks/abc157_d) - 【ACコード】
- [ABC120 D Decayed Bridges](https://atcoder.jp/contests/abc120/tasks/abc120_d) - 【ACコード】
- [ARC111 B Reversible Cards](https://atcoder.jp/contests/arc111/tasks/arc111_b) - 【ACコード】
- [ABC183 F Confluence](https://atcoder.jp/contests/abc183/tasks/abc183_f) - 【ACコード】

その他
- [yukicoder No.1390 Get together](https://yukicoder.me/problems/no/1390) - 【ACコード】
- [MojaCoder Bonsai](https://mojacoder.app/users/magurofly/problems/bonsai) - 【ACコード】

nkmk.meより
- [AtCoder Typical Contest 001 B - Union Find](https://atcoder.jp/contests/atc001/tasks/unionfind_a)
- [互いに素な集合 Union Find| データ構造ライブラリ | Aizu Online Judge](http://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=DSL_1_A&lang=jp)
