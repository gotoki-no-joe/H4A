{-
組み合わせの場合の数 nCk
-}

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV
import Control.Monad

-- https://algo-logic.info/combination/
combBuilder :: Int -> Int -> (Int -> Int -> Int)
combBuilder modBase ub = comb
  where
    mul x y = mod (x * y) modBase
    facts = UV.fromList $ scanl mul 1 [1..ub]
    invs = UV.create $ do
      v <- MUV.new $ succ ub
      MUV.write v 0 1
      MUV.write v 1 1
      forM_ [2..ub] (\i -> do
        let (q,r) = divMod modBase i
        ir <- MUV.read v r
        MUV.write v i $ modBase - mul ir q
        )
      return v
    factinvs = UV.scanl1 mul invs
    comb n k = mul (facts UV.! n) $ mul (factinvs UV.! k) (factinvs UV.! (n-k))
