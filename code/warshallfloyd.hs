{-
グラフの全ての頂点間の距離を一度に求める。ただし$$O(V^3)$$である。

頂点は整数で0からn-1まで、
グラフは、頂点a,b間に辺がないときRight ()、重みwの辺があるときLeft wを返す関数で与える。結果はArray ((1,1),(n,n)) で返す。
-}

import Data.Array
import Data.List

import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV

-- @gotoki_no_joe
warshallFloyd :: (Ord a, Num a)
              => Int                         -- 頂点数
              -> (Int -> Int -> Either a ()) -- グラフ
              -> Array (Int,Int) a           -- 距離
warshallFloyd n graph = result
  where
    range = [1..n]
    d0 = array ((1,1),(n,n)) [((i,j),graph i j) | i <- range, j <- range]
    dn = foldl' step d0 range
    result = array ((1,1),(n,n))
      [((i,j),w) | i <- range, j <- range, let Left w = dn ! (i,j)]
    step d k = d //
      [((i,j), dikj)
      | i <- range, j <- range
      , let dikj = plus (d ! (i,k)) (d ! (k,j))
      , d ! (i,j) > dikj
      ]
    plus (Left a) (Left b) = Left (a+b)
    plus _ _ = Right ()

warshallFloydV :: Int                         -- 頂点数
              -> [(Int,Int,Int)]             -- グラフ
              -> (Int -> Int -> Int)           -- 距離
warshallFloydV n graph = reader n $ UV.create action
  where
    reader n v i j = v UV.! (i*n+j)
    action :: ST s (MUV.MVector s Int)
    action = do
      vec <- MUV.replicate (n * n) maxBound
      forM_ graph (\(i,j,w) -> MUV.write vec (i*n+j) w)
      forM_ [0..n-1] (\k ->
        forM_ [0..n-1] (\i -> do
          dik <- MUV.read vec (i*n+k)
          when (dik < maxBound) (
            forM_ [0..n-1] (\j -> do
              dkj <- MUV.read vec (k*n+j)
              when (dkj < maxBound) (MUV.modify vec (min (dik + dkj)) (i*n+j))
              )
            )
          )
        )
      return vec

