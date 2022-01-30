---
order: -03000
---

# クラスカル法

辺に重みがある**連結な無向グラフ**が与えられ、

- 部分グラフで、木
- グラフの全ての頂点を含む（これを全域木という）
- 辺の重みの総和が最小

という条件を満たすグラフ、すなわち**最小全域木**を見つける問題を解くアルゴリズム

ノードが0からn-1、i=0～について辺iがノードaiとbiを結び重みciという情報が与えられるとする。
合計コストと、最小全域木に使った辺の番号のリスト（重い順）を返却する。
内部的に[Union-Find](/ad/misc/unionfind/)の通常版を使用する。

```haskell
import Data.List

-- @gotoki_no_joe
kruskal :: Int -> [(Int,Int,Int)] -> IO (Int,[Int])
kruskal n abcs =
  do
    uf <- newUF n
    loop uf 0 [] (pred n) cabis
  where
    cabis = sort [(c,a,b,i) | (i,(a,b,c)) <- zip [0..] abcs]
    loop _  cost res 0 _ = return (cost, res)
    loop uf cost res k ((c,a,b,i):abis) =
      do
        b <- uniteUF uf a b
        if b then loop uf (cost+c) (i:res) (pred k) abis
             else loop uf  cost       res        k  abis
```

n頂点を持つ木は辺をn-1本だけ持つので、それで終了にしている。

[あのアルゴリズムはどこ？の8](/readings/whereis/08.kruskal/)より。
