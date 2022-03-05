import Data.List

perm xs = permOfR (length xs) xs

permOfR 0 _ = [[]]
permOfR r xs = [a : ys | (a,as) <- one [] xs, ys <- permOfR (pred r) as]
  where
    -- 要素を一つ選び、残りの要素を付けて返す
    one bs [] = []
    one bs (a:as) = (a, rev bs as) : one (a:bs) as
    -- reverse bs ++ as
    rev bs as = foldl' {-'-} (flip (:)) as bs

combOf 0 _      = [[]]
combOf _ []     = [  ]
combOf n (a:as) = [a:bs | bs <- combOf (pred n) as] ++ combOf n as

prodOf0 0 _  = [[]]
prodOf0 n as = [a:bs | a <- as, bs <- prodOf0 (pred n) as]

prodOf n as = sequence $ replicate n as
