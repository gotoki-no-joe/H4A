---
order: -02000
---
# 探索

初期状態iから、状態遷移を繰り返して目的状態に到達できるかを探す。  
状態sが完了状態であるなら、そこから返すべき結果xが求められる。  
状態sから、さらに探索するべき子の状態リストtsが求められる。  
状態sからこの2つの情報を返す関数をパラメータとして、探索を実行する関数を作る。

## 深さ優先探索

再帰呼び出しにより深さ優先探索を行うスタイルのドライバは以下のようにできる。

```haskell
-- @gotoki_no_joe
dfs :: (s -> (Maybe x, [s])) -> s -> [x]
dfs f i = recur i []
  where
    recur s rest =
      case f s of
        (Nothing, ts) ->     foldr recur rest ts
        (Just x , ts) -> x : foldr recur rest ts
```

あるいは、処理するべき状態のスタックをリストで表現することもできる。

```haskell
dfs_in_loop :: (s -> (Maybe x, [s])) -> s -> [x]
dfs_in_loop f i = loop [i]
  where
    loop [] = []
    loop (s:ss) =
      case f s of
        (Nothing, ts) ->     loop (ts ++ ss)
        (Just x , ts) -> x : loop (ts ++ ss)
```

## 幅優先探索

現在の状態から派生した次の状態をスタックトップに積む代わりに、
キューの末尾に追加することで幅優先探索になる。
上の `dfs_in_loop` の `ts ++ ss` を `ss ++ ts` にするだけでそうなるが、
正しいデータ構造を使ってみよう。

```haskell
import qualified Data.Sequence as Q

-- @gotoki_no_joe
bfs :: (s -> (Maybe x, [s])) -> s -> [x]
bfs f i = loop (Q.singleton i)
  where
    loop Q.Empty     = []
    loop (s Q.:<| q) =
      case f s of
        (Nothing, ts) ->     loop (foldl (Q.|>) q ts)
        (Just x , ts) -> x : loop (foldl (Q.|>) q ts)
```

## 関連問題

- 深さ優先探索
  - [ABC114 C 755](https://atcoder.jp/contests/abc114/tasks/abc114_c) - [ACコード](https://atcoder.jp/contests/abc114/submissions/28769755)
  - [パナソニックプログラミングコンテスト D String Equivalence](https://atcoder.jp/contests/panasonic2020/tasks/panasonic2020_d) - [ACコード](https://atcoder.jp/contests/panasonic2020/submissions/28769923)
  - [ABC119 C Synthetic Kadomatsu](https://atcoder.jp/contests/abc119/tasks/abc119_c) - [ACコード](https://atcoder.jp/contests/abc119/submissions/28774935) 「どの操作をするか」の選択を完全に自由に探索するのではない点に注意 [別解](https://atcoder.jp/contests/abc119/submissions/26061046)
  - [ABC165 C Many Requirements](https://atcoder.jp/contests/abc165/tasks/abc165_c) - [ACコード](https://atcoder.jp/contests/abc165/submissions/28775412) [別解](https://atcoder.jp/contests/abc165/submissions/12627062)
- 幅優先探索
  - [ABC161 D Lunlun Number](https://atcoder.jp/contests/abc161/tasks/abc161_d) - [ACコード](https://atcoder.jp/contests/abc161/submissions/28773508)  
  初期状態を複数与えられるように改変するともっと[シンプルにできる](https://atcoder.jp/contests/abc161/submissions/28773376)  
  ただし、フィボナッチ数列のように[直書き](https://atcoder.jp/contests/abc161/submissions/11538255)する方が速い。

ABC119CやABC165Cは、Nが小さいこともあり、可能な場合を全て生成するgenerate&testアプローチで計算することもできる。
その方がコードの見通しはよいかもしれない。
ただし、探索を用いたアプローチの方が、途中までの結果を共有することができる分速度では有利になる。

- [ABC196 D Hanjo](https://atcoder.jp/contests/abc196/tasks/abc196_d) - ACコード

## 幅優先探索に関する蛇足

遅延リストをループバックさせることで、キューを模倣することもできる。
ただし、キューの中の要素数を明示的に管理する必要がある。
これを怠ると、キューが空になったときにハングアップする。

```haskell
bfs f i = xs
  where
    (xs,ss) = loop 1 (i:ss)
    loop 0 _ = ([],[])
    loop n (s:ss) =
      case f s of
        Left  r  -> let (xs1,ss1) = loop (pred n) ss in (r:xs1, ss1)
        Right ts -> let (xs1,ss1) = loop (pred n + length ts) ss in (xs1, ts ++ ss1)
```
