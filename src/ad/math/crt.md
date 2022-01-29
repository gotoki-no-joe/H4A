# 中国剰余定理

- [中国剰余定理 (CRT) の解説と、それを用いる問題のまとめ](https://qiita.com/drken/items/ae02240cd1f8edfc86fd)
- [【math編】AtCoder Library 解読 〜Pythonでの実装まで〜](https://qiita.com/R_olldIce/items/3e2c80baa6d5e6f3abe9)

使う分にはこれを移植すれば済むが、理解するのは大変そうだ...

ACLにも含まれている。

## 問題

$n$ 個の自然数 $m_i$ と整数 $r_i$ による
$n$ 連立合同式

$$
\begin{array}{l}
x \equiv r_0 \mod m_0 \\
x \equiv r_1 \mod m_1 \\
\vdots \\
x \equiv r_{n-1} \mod m_{n-1}
\end{array}
$$

が解をもつとき、それは
$z = \textrm{lcm}(m_0, m_1, \dots, m_{n-1})$
として
$x \equiv y \mod z$
となる $y, z$ を求める。

## コード

(r,m)の対のリストに対して(y,z)を返す。
空リストに対しては (0,1) が確かに解である。
解がないときは `Nothing` を返す。

```haskell
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
```

それぞれ以下のページのPythonコードをHaskellに翻訳しただけ。理解はしていない。

- [【math編】AtCoder Library 解読 〜Pythonでの実装まで〜 3.7. 実装](https://qiita.com/R_olldIce/items/3e2c80baa6d5e6f3abe9#37-%E5%AE%9F%E8%A3%85)
- [【internal_math編①】AtCoder Library 解読 〜Pythonでの実装まで〜 3.7. 実装](https://qiita.com/R_olldIce/items/cebb1f15bf482fddd85e#3-inv_gcd)

### 関連問題

- [ABC186 E Throne](https://atcoder.jp/contests/abc186/tasks/abc186_e) - [ACコード](https://atcoder.jp/contests/abc186/submissions/28756587)
- [ABC193 E Oversleeping](https://atcoder.jp/contests/abc193/tasks/abc193_e) - 【ACコード】

#### コーディングスタイル

`invGCD`のループ部分を、反復の代わりに `until` を使って書くのがよいらしい。

```haskell
invGCD1 :: Int -> Int -> (Int, Int)
invGCD1 a b
  | a1 == 0 = (b, 0)
  | otherwise = post . head . until stop step (b, a1, 0, 1)
  where
    a1 = mod a b
    step (s, t, m0, m1) = (t, s - t * u, m1, m0 - m1 * u) where u = div s t
    stop (_, t, _, _) = t == 0
    post (s, _, m0, m1) = (s, if m0 < 0 then m0 + div b s else m0)
```
