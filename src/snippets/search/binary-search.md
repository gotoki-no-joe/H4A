---
order: -04000
---

# 二分探索

出典：あのアルゴリズムはどこ？の22.二分探索

整数列に対する単調な性質があり、満たす値と満たさない値があるとき、その境界を見つける。

第1引数は性質`p :: Int -> Bool`を与える。  
第2,第3引数はそれぞれ`p unsat == False` となるような`unsat :: Int`と
`p sat == True`となるような`sat :: Int`を与える。
`p unsat`や`p sat`は評価しないので、もう1だけ「外側」でもよい。
`unsat`と`sat`の大小関係は問わない。  
結果`(unsat,sat)`は、`unsat`と`sat`の差が1で、`p unsat == False`かつ`p sat == True`となる境界である。

```haskell
-- @gotoki_no_joe
binarySearch :: (Int -> Bool) -> Int -> Int -> (Int, Int)
binarySearch prop unsat sat = loop unsat sat
  where
    loop a b
      | ende   = (a, b)
      | prop m = loop a m
      | True   = loop m b
      where
        ende = a == m || b == m 
        m = div (a + b) 2
```

オリジナルの「[めぐる式](https://aotamasaki.hatenablog.com/entry/meguru_bisect)」では、
終了条件が `ende = abs (a-b) <= 1` となっている。

再帰を自分で書かないようにするなら、

```haskell
-- @gotoki_no_joe
binarySearch :: (Int -> Bool) -> Int -> Int -> (Int, Int)
binarySearch prop unsat sat = until goal step (unsat, sat)
  where
    goal (ng, ok) = abs (ok - ng) <= 1
    step (ng, ok) = if prop mid then (ng, mid) else (mid, ok)
      where
        mid = div (ok + ng) 2
```

### 関連問題

- [ARC109 B log](https://atcoder.jp/contests/arc109/tasks/arc109_b) - 【ACコード】
- [ABC174 E logs](https://atcoder.jp/contests/abc174/tasks/abc174_e) - [ACコード](https://atcoder.jp/contests/abc174/submissions/28794556)
- [ARC050 B 花束](https://atcoder.jp/contests/arc050/tasks/arc050_b) - [ACコード](https://atcoder.jp/contests/arc050/submissions/28796289)

他サイト

- [yukicoder No.1101 鼻水](https://yukicoder.me/problems/no/1101)