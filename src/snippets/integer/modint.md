---
order: -05000
---
# 合同算術 (ModInt) (*)

何度もやらかしているので、`Num` 型クラスにこだわらずに作る方法を考える。

newtypeでもコンストラクタがかかっているために、Unboxedに入れることができないのは、
何か解決策があったような？

## base固定版

```haskell
modBase = 1000000007
modBase =  998244353

-- @gotoki_no_joe
newtype MI = MI Int
toInt (MI x) = x
miAdd  = miOp (+)
miMul  = miOp (*)
miPred = miUna pred
miNeg  = miUna negate
miRecip (MI a) = MI u
  where
    (_,_,u,_) = until stop step (a, modBase, 1, 0)
    step (a,b,u,v) = let t = div a b in (b, a - t * b, v, u - t * v)
    stop (_,b,_,_) = b == 0

miSum, miProd :: Foldable f => f MI -> MI
miSum  = foldl' {-'-} miAdd (MI 0)
miProd = foldl' {-'-} miMul (MI 1)

miReg x = MI (mod x modBase)
miOp op (MI x1) (MI x2) = miReg (op x1 x2)
miUna op (MI x) = miReg (op x)
```

## base複数版

baseが入り乱れる場合用、あまり必要性を感じない

```haskell
-- for your convenient
p1097 = 1000000007

newtype MI = MI Int Int

-- mkMI = MI p1097 としてファクトリを作っておくべき

miReg b x = MI b (mod x b)

miOp op (MI b1 v1) (MI b2 v2)
  | b1 /= b2 = error "base inconsistent"
  | otherwise = miReg b1 (op v1 v2)

miAdd = miOp (+)
miMul = miOp (*)
miPred = ...
miRecip = ...

```
