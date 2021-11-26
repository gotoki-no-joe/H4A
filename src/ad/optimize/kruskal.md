# クラスカル法

- [Wikipedia](https://ja.wikipedia.org/wiki/%E3%82%AF%E3%83%A9%E3%82%B9%E3%82%AB%E3%83%AB%E6%B3%95)

連結な無向グラフの全域木で、辺の重み合計が最小の最小全域木を得るアルゴリズムのひとつ。もうひとつはPrim法。

「無向グラフ、連結、辺に重みがある」という条件の元で、「グラフの全ての頂点を含む部分グラフで、木で、そのようなものの中で重みの合計が最小」である最小全域木を得る。

Prim法は、任意の1頂点だけからなる木を初期値とし、まだ木に含まれない頂点と木を結ぶ辺で重みが最小のものを貪欲に追加する。

Kruskal法は、重みの小さい順に辺を調べ、閉路や多重辺がなければ木の辺として採用することを繰り返す。
つまり、Union-Findで、無駄足にならない辺だけを追加して全て連結になるまで繰り返す。
計算量は $O(|E|\log|V|)$ となるらしい。

ノードが0からn-1、i=0～について辺iがノードaiとbiを結び重みciという情報が与えられるとする。
合計コストと、最小全域木に使った辺の番号のリスト（重い順）を返却する。
内部的に[Union-Find](../misc/unionfind/)を使用する。

```haskell
-- @gotoki_no_joe
kruskal :: Int -> [(Int,Int,Int)] -> IO (Int,[Int])
kruskal n abcs =
  do
    v <- newUF n
    loop 0 [] v (pred n) cabis
  where
    cabis = sort [(c,a,b,i) | (i,(a,b,c)) <- zip [0..] abcs]
    loop cost res _ 0 _ = return (cost, res)
    loop cost res v k ((c,a,b,i):abis) =
      do
        e <- findUF v a b
        if e then loop cost res v k abis
             else do
               uniteUF v a b
               loop (cost+c) (i:res) v (pred k) abis
```

n頂点を持つ木は辺をn-1本だけ持つので、それで終了にしている。

## 関連問題

- [いろはちゃんコンテスト Day2 D](https://atcoder.jp/contests/iroha2019-day2/tasks/iroha2019_day2_d) - [ACコード](https://atcoder.jp/contests/iroha2019-day2/submissions/22745993)
- [ABC218 E Destruction](https://atcoder.jp/contests/abc218/tasks/abc218_e) - [ACコード](https://atcoder.jp/contests/abc218/submissions/27503010)
- [ABC065 D Built?](https://atcoder.jp/contests/abc065/tasks/arc076_b) - 【ACコード】

## 関連

多次元配列のソート、も参照せよとのこと。
