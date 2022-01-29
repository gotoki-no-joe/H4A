import Control.Monad

test1 = crt [(3,5),(4,7)] -- (18,35)

-- https://qiita.com/R_olldIce/items/3e2c80baa6d5e6f3abe9#37-%E5%AE%9F%E8%A3%85
{-
crt :: [(Int,Int)] -> Maybe (Int,Int)
crt = foldM step (0,1)
  where
    step (r0,m0) (r1,m1)
      | mod m0b m1b == 0 = if mod r0b m1b == r1b then Just (r0b, m0b) else Nothing
      | mod rd g /= 0 = Nothing
      | otherwise = Just (r0b + x * m0b, m0b * u1)
      where
        r1a = mod r1 m1
        (r0b,r1b,m0b,m1b) = if m0 >= m1 then (r0,r1a,m0,m1) else (r1a,r0,m1,m0) -- swap
        rd = r1b - r0b
        (g,im) = invGCD m0b m1b
        u1 = div m1b g
        x = mod (mod (div rd g) u1 * im) u1
-}

crt :: [(Int,Int)] -> Maybe (Int,Int)
crt = foldM step1 (0,1)
  where
    step1 (r0,m0) (r1,m1)
      | m0 < m1   = step2 (mod r1 m1) m1 r0 m0
      | otherwise = step2 r0 m0 (mod r1 m1) m1
    step2 r0 m0 r1 m1
      | mod m0 m1 == 0 = if mod r0 m1 == r1 then Just (r0, m0) else Nothing
      | r /= 0         = Nothing
      | otherwise      = Just (r0 + x * m0, m0 * u)
      where
        (g,im) = invGCD m0 m1
        (q, r) = divMod (r1 - r0) g
        u = div m1 g
        x = mod (mod q u * im) u

-- https://qiita.com/R_olldIce/items/cebb1f15bf482fddd85e#3-inv_gcd
invGCD :: Int -> Int -> (Int, Int)
invGCD a b
  | a1 == 0 = (b, 0)
  | otherwise = loop b a1 0 1
  where
    a1 = mod a b
    loop s 0 m0 m1 = (s, if m0 < 0 then m0 + div b s else m0)
    loop s t m0 m1 = loop t (s - t * u) m1 (m0 - m1 * u)
      where
        u = div s t

invGCD1 :: Int -> Int -> (Int, Int)
invGCD1 a b
  | a1 == 0 = (b, 0)
  | otherwise = post . head . filter stop . iterate step $ (b, a1, 0, 1)
  where
    a1 = mod a b
    step (s, t, m0, m1) = (t, s - t * u, m1, m0 - m1 * u) where u = div s t
    stop (_, t, _, _) = t == 0
    post (s, _, m0, m1) = (s, if m0 < 0 then m0 + div b s else m0)

testi1 = invGCD 3 5 -- (1,2)
testi2 = invGCD 20 15 -- (5,1)
