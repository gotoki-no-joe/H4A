---
order: -01000
---
# ダイクストラ法

グラフにおいて、辺の重みが非負であるとき、
指定した開始点から他の全ての頂点への経路で重みの和が最小のものを探すアルゴリズム。

## Data.Vector.Unboxed版

内部では `Data.Vector.Unboxed.Mutable` を使っているが、
STモナドで計算しているのでpureな計算になっている。

```haskell
import qualified Data.Heap as H
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV
import Control.Monad.ST
import Control.Monad

-- @gotoki_no_joe
dijkstraV :: (Ord w, Num w, Bounded w, MUV.Unbox w)  -- 頂点 Int, 辺の重み w
         => Int                           -- 頂点数
         -> (Int -> [(Int,w)])            -- 隣接頂点とその辺の重み、グラフの情報
         -> Int                           -- 開始点
         -> (UV.Vector w, UV.Vector Int)  -- 最短経路の重み、最短経路の前の頂点 (routeに渡すもの)
dijkstraV n graph start = runST (action graph)
  where
    action graph = do
      dist <- MUV.replicate n maxBound
      MUV.write dist start 0
      prev <- MUV.replicate n (-1)
      let queue = H.singleton (H.Entry 0 start)
      loop graph dist prev queue
      imdist <- UV.freeze dist
      imprev <- UV.freeze prev
      return (imdist, imprev)
    loop _ _ _ queue
      | H.null queue = return ()
    loop graph dist prev queue = do
      let Just (H.Entry cost u, queue1) = H.uncons queue
      du <- MUV.read dist u
      if du < cost then loop graph dist prev queue1 else do
        vds <- forM (graph u) (\(v, len) -> do
          let d1 = du + len
          dv <- MUV.read dist v
          if d1 >= dv then return (H.Entry (-1) (-1)) else do
            MUV.write dist v d1
            MUV.write prev v u
            return (H.Entry d1 v)
          )
        let queue2 = H.union queue1 $ H.fromList [e | e@(H.Entry p _) <- vds, p /= -1]
        loop graph dist prev queue2
```

`maxBound`をしれっと使っている。

`graph` を変に引数に渡しているのは、そうしないと重みの型 `w` を共有できないため。
型変数を共通化するGHC拡張オプションを使う手もあるが。

## Data.IntMap版

一応残しておく。

```haskell
import qualified Data.Heap as H
import qualified Data.IntMap as IM

-- @gotoki_no_joe
dijkstraI :: (Ord w, Num w)               -- v 頂点 w 辺の重み
         => (Int -> [(Int,w)])            -- 隣接頂点とその辺の重み、グラフの情報
         -> Int                           -- 開始点
         -> (IM.IntMap w, IM.IntMap Int)  -- 最短経路の重み、最短経路の前の頂点 (routeに渡すもの)
dijkstraI graph start = loop queue0 dist0 prev0
  where
    dist0 = IM.singleton start 0
    prev0 = IM.empty
    queue0 = H.singleton (H.Entry 0 start)
    loop queue dist prev
      | H.null queue = (dist, prev)
      | du < cost    = loop queue1 dist prev
      | otherwise    = loop queue2 dist1 prev1
      where
        Just (H.Entry cost u, queue1) = H.uncons queue
        du = dist IM.! u
        vds = [(v,d1) | (v,len) <- graph u, let d1 = du + len
                      , IM.notMember v dist || d1 < dist IM.! v]
        dist1 = IM.union (IM.fromList vds) dist
        prev1 = IM.union (IM.fromList [(v,u) | (v,_) <- vds]) prev
        queue2 = H.union (H.fromList [H.Entry d v | (v,d) <- vds]) queue1
```

[あのアルゴリズムはどこ？の27](/readings/whereis/27.dijkstra)より。
頂点をIntに限定しない汎用なMap版もこちらのページに。
