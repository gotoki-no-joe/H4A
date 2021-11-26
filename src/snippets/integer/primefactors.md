---
order: -03000
---
# 素因数分解（試し割り法）

```haskell
-- @gotoki_no_joe
primeFactors :: Int -> [Int]
primeFactors n = loop1 n
  where
    loop1 n
      | even n = 2 : loop1 (div n 2)
      | otherwise = loop2 n 3
    loop2 n f
      | n < f * f = [n | n /= 1]
      | otherwise =
          case divMod n f of
            (q, 0) -> f : loop2 q f
            _      ->     loop2 n (f+2)
```

素数リスト `primes` が手元にある場合

```haskell
-- @gotoki_no_joe
primeFactors :: Int -> [Int]
primeFactors n = loop n primes
  where
    loop 1 _ = []
    loop n pps@(p:ps)
      | n < p * p = [n]
      | otherwise =
          case divMod n p of
            (q, 0) -> p : loop q pps
            _      ->     loop n  ps
```

## 説明

あのアルゴリズムはどこ？の4より。

素朴には、対象nと素因数候補f=2から始めて、割りきれる限り、
nを商に置き換え、fを素因数として記録することを繰り返す。
割り切れなくなったらfを1増やして、n > f となるまで続ける。

2以外の素数は奇数なので、p=2の場合だけ特別扱いすることで、
3以上についてfを1増やす代わりに2増やす、としている。

素数リストが手元にある場合は、素因数の候補はそこから選択するだけで済む。

### 関連問題

- [ABC169 D Div Game](https://atcoder.jp/contests/abc169/tasks/abc169_d) - [ACコード](https://atcoder.jp/contests/abc169/submissions/22775113)
- [ABC177 E Coprime](https://atcoder.jp/contests/abc177/tasks/abc177_e) - [ACコード](https://atcoder.jp/contests/abc177/submissions/22737449)
- [ABC152 F Flatten](https://atcoder.jp/contests/abc152/tasks/abc152_e) - --ACコード--

他サイト

- [MojaCoder 楔数（くさびすう）](https://mojacoder.app/users/H20/problems/sphenic-number)
