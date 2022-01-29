# クラスカル法

ノードが0からn-1、i=0～について辺iがノードaiとbiを結び重みciという情報が与えられるとする。
合計コストと、最小全域木に使った辺の番号のリスト（重い順）を返却する。
内部的に[Union-Find](/H4A/ad/misc/unionfind/)の通常版を使用する。

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

[あのアルゴリズムはどこ？の8](/H4A/readings/whereis/08.kruskal/)より。
