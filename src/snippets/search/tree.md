---
order: -03000
---
# 木を扱う

AtCoderでよくある木、グラフの表現は、
頂点が1からNで、それらの間の無向辺の情報リストを与えられるものである。

まずその情報を、頂点に対する辺の接続先のリストの配列にすることで扱いやすくする。
（辺の情報を `[(Int,Int)]` でなく `[[Int]]` で扱うのは、
入力データの読み込みの都合。）

```haskell
import Data.Array

-- @gotoki_no_joe
type TreeEdges = Array Int [Int]

mkEdges :: Int -> [[Int]] -> TreeEdges
mkEdges n abs = accumArray (flip (:)) [] (1,n) [p | (a:b:_) <- abs, p <- [(a,b),(b,a)]]
```

根から始めて、状態変化を持ちながら葉に探索を進める探索を、
一般的な探索の `dfs`, `bfs` を応用して実現したい。
これらが受け取るステップ関数の型は `s -> (Maybe x, [s])` としていた。

木を辿るステップ関数は、子を知るために `TreeEdges` も持っている必要がある。
この配列から頂点に対して接続先を取り出すと、親もその中に混じっているので、
探索の際には、現在の注目頂点だけでなく親も渡す必要がある。
（一般化して、根からの経路（の逆順）を与えることにする。）

親から受け取る状態sと、現在の頂点番号iから、
結果を返すなら返しMaybe x、次に伝える状態を作るステップ関数
`Int -> s -> (Maybe x, s)`
と、木の形 `TreeEdges` をパラメータにして、
`dfs`, `bfs` で使えるステップ関数を作るアダプタは次のようにできる。

```haskell
traverser :: TreeEdges -> (Int -> s -> (Maybe x, s))
          -> (([Int], s) -> (Maybe x, [([Int], s)]))
traverser edges f = step
  where
    step (path@(i:p:_), s) = (mx, [(c:path, t) | c <- delete p $ edges ! i])
      where
        (mx, t) = f i s
```

初期状態としては、本来の初期状態sに加えて、根への経路も付ける。
存在しないノード0を、根の親として使っている。

```haskell
initialState s = ([1,0], s)
```

頂点で得られた結果xは、探索順のリストとして取り出される。
畳み込みをして、子の結果を集約して根まで戻す形の計算も必要になるかもしれない。

### 関連問題

- [ABC198 E Unique Color](https://atcoder.jp/contests/abc198/tasks/abc198_e) - [ACコード](https://atcoder.jp/contests/abc198/submissions/28774494)
