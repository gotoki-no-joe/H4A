import qualified Data.Heap as H
import qualified Data.Map as M

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV
import Control.Monad.ST
import Control.Monad

-- @gotoki_no_joe
dijkstra :: (Eq v, Ord v, Ord w, Num w) -- v 頂点 w 辺の重み
         => (v -> [(v,w)])              -- 隣接頂点とその辺の重み、グラフの情報
         -> v                           -- 開始点
         -> (M.Map v w, M.Map v v)      -- 最短経路の重み、最短経路の前の頂点 (routeに渡すもの)
dijkstra graph start = post $ until stop step (queue0, dist0, prev0)
  where
    -- 開始点から点vまでの既知の最短距離 d(v) 初期値は d(start) = 0 その他は∞
    dist0 = M.singleton start 0
    -- 既知の最短経路において、vからstartに一つ戻る頂点 prev(v) 初期値は何もなし
    prev0 = M.empty
    -- 探索済み頂点から到達できる頂点のキュー、既知の最小コストが優先度
    queue0 = H.singleton (H.Entry 0 start)
    -- 繰り返しはキューが空になったら完了
    stop (queue, _, _) = H.null queue
    -- 後処理
    post (_, dist, prev) = (dist, prev)
    -- 繰り返し処理
    step (queue, dist, prev)
    -- 先頭ノードが確定済みならスルーして次へ
      | du < cost = (queue1, dist, prev)
    -- 本題
      | otherwise = (queue2, dist1, prev1)
      where
        Just (H.Entry cost u, queue1) = H.uncons queue
        du = dist M.! u
        -- uから到達できる全てのvについて
        -- u経由でvに到達する経路のコスト du+l=d1 が、既知のコスト dist M.! v を下回っているものについて
        -- d(v)をd1に、prev(v)をuに更新し、Qのd1にvを登録する
        vds = [(v,d1) | (v,len) <- graph u, let d1 = du + len, M.notMember v dist || d1 < dist M.! v]
        dist1 = M.union (M.fromList vds) dist
        prev1 = M.union (M.fromList [(v,u) | (v,_) <- vds]) prev
        queue2 = H.union (H.fromList [H.Entry d v | (v,d) <- vds]) queue1

-- @gotoki_no_joe
dijkstraV :: (Ord w, Num w, Bounded w, MUV.Unbox w)  -- 頂点 Int, 辺の重み w
         => Int                           -- 頂点数
         -> (Int -> [(Int,w)])            -- 隣接頂点とその辺の重み、グラフの情報
         -> Int                           -- 開始点
         -> (UV.Vector w, UV.Vector Int)  -- 最短経路の重み、最短経路の前の頂点 (routeに渡すもの)
dijkstraV n graph start = runST (action graph)
  where
--    action :: (Num w, Bounded w, Ord w, MUV.Unbox w) => (Int -> [(Int,w)]) -> ST s (UV.Vector w, UV.Vector Int)
    action graph = do
      dist <- MUV.replicate n maxBound
      MUV.write dist start 0
      prev <- MUV.replicate n (-1)
      let queue = H.singleton (H.Entry 0 start)
      loop graph dist prev queue
      imdist <- UV.freeze dist
      imprev <- UV.freeze prev
      return (imdist, imprev)
{-    
    loop :: (Num w, MUV.Unbox w, Ord w)
         => (Int -> [(Int,w)])
         -> MUV.MVector s w
         -> MUV.MVector s Int
         -> H.Heap (H.Entry w Int)
         -> ST s () -}
    loop _ _ _ queue
      | H.null queue = return ()
    loop graph dist prev queue = do
      let Just (H.Entry  cost u, queue1) = H.uncons queue
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

-- graphの型が宙ぶらりんになるのを防ぐためだけに引数に渡している。
