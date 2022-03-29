# 二項係数

ABC042D
https://atcoder.jp/contests/abc042/submissions/30156743

```haskell
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV
import Control.Monad
import Data.List

modBase = 10^9 + 7
r x = mod x modBase
mul x y = r (x * y)
add x y = r (x + y)

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
```
ABC034C
https://atcoder.jp/contests/abc034/submissions/22940225

```haskell
import Data.List

modBase = 1000000007

combMod n r = foldl' mul 1 [r+1..n] `mul` recipMod (foldl' mul 1 [2..n-r])

re a = mod a modBase

mul a b = re (a * b)

recipMod a = re u
  where
    (_,_,u,_) = until cond step (a, modBase, 1, 0)
    step (a,b,u,v) =
      let t = a `div` b
      in  (b, a - t * b, v, u - t * v)
    cond (_,b,_,_) = b /= 0
    you (_,_,u,_) = u
```
