---
order: -01000
---
# あのアルゴリズムはどこ？

[あのアルゴリズムはどこ？　Pythonを使用してAtCoderの緑色や水色を目指す方に、30以上のアルゴリズムスニペットと100問以上の問題（ACコード付き）を紹介！](https://qiita.com/H20/items/1a066e242815961cd043)
をなぞる。

<hr/>


## 16.コンビネーション

[二項係数](../ad/math/combination/) を参照。

## 17. べき乗

modで特定の値のいろいろなべき乗を、上限ありで求める場合、事前計算しておいてもお釣りが出るという話。

```haskell
import qualified Data.Vector as V

-- a^b を 0<=b<=num の範囲で先に求める
-- a^b = powerVec a V.! b

num = 1000
modBase = 1000000007

powerVec a = V.iterateN num (mul base) 1

mul a b = mod (a * b) modBase
```

#### 関連問題

[HHKB プログラミングコンテスト 2020 E Lamps](https://atcoder.jp/contests/hhkb2020/tasks/hhkb2020_e)  
この問題の事前問題  
[ABC129 D Lamp](https://atcoder.jp/contests/abc129/tasks/abc129_d)は解けたが、想定解と違ったので上の問題に結びつかなかった。
TODO: 想定解でやりなおし

## 18.最大公約数、最小公倍数

Haskellでは`Prelude`に`gcd`と`lcm`がある。

3つ以上の値のそれも、繰り返し適用すれば求められる。

```haskell
gcdList, lcmList :: [Int] -> Int
gcdList = foldl1' gcd
lcmList = foldl1' lcm
```

#### 関連問題

- [ABC102 A Multiple of 2 and N](https://atcoder.jp/contests/abc102/tasks/abc102_a) - [ACコード](https://atcoder.jp/contests/abc102/submissions/22946229)
- [ARC105 B MAX-=min](https://atcoder.jp/contests/arc105/tasks/arc105_b) - [ACコード](https://atcoder.jp/contests/arc105/submissions/22946285)
- [ABC131 C Anti-Division](https://atcoder.jp/contests/abc131/tasks/abc131_c) - [ACコード](https://atcoder.jp/contests/abc131/submissions/22946480)
- [ARC110 A Redundant Redundancy](https://atcoder.jp/contests/arc110/tasks/arc110_a) - [ACコード](https://atcoder.jp/contests/arc110/submissions/22946589)

他サイト
- [yukicoder No.1464 Number Conversion](https://yukicoder.me/problems/no/1464)

## 19.中国剰余定理

[中国剰余定理](../ad/math/crt/)を参照。

## 20.DFS

[探索](../snippets/search/search/)および[木を扱う](../snippets/search/tree/)を参照。

## 21.二分探索 (bisect)

数値群の中で、ある値未満、以下、以上、より大きい、要素がいくつあるかを数える問題を考える。

Pythonには、整列された配列に対して、整列を保って新たな値を挿入するときの挿入位置を得る`bisect`関数があるようだ。
これは二分探索で効率的に挿入位置を求めるので、求められたその位置の添え字が、
その値より小さい値の個数となり、上の問題を解くために利用できる。

値に対して昇順に背番号を付け、値をキー、背番号を値とする `Data.Map` や `IntMap` を作る。
このMapに対して `lookupLT/LE/GE/GT` 系でアクセスすると、
問題の要素数を導く材料となる背番号が効率的に取り出せる。
Mapは内部的に、二分検索木をキーに対して構築しているので、これを辿ることは
添え字の中点をとる二分探索を実行することに相当している。

値を発見するだけでよければ `Data.Set`, `IntSet` も同様に使える。

#### 関連問題

- [ABC143 D Triangles](https://atcoder.jp/contests/abc143/tasks/abc143_d) - [ACコード](https://atcoder.jp/contests/abc143/submissions/28779419)  
棒の長さが1000以下の整数なので配列の添字にした[別解](https://atcoder.jp/contests/abc143/submissions/14003171)  
ここでさらに要素数が増えると線形に総和を取るのが難しいので、セグメント木のようなデータ構造を導入する必要が生じるか。
- [ABC077 C Snuke Festival](https://atcoder.jp/contests/abc077/tasks/arc084_a) - [ACコード](https://atcoder.jp/contests/abc077/submissions/28791823)  
問題に特化したアルゴリズムで、大きい方から順にmergesortのように比較して個数を数えることで、 $O(n)$ で計算する[別解](https://atcoder.jp/contests/abc077/submissions/28780649)
- [ABC184 F Programming Contest](https://atcoder.jp/contests/abc184/tasks/abc184_f) - [ACコード](https://atcoder.jp/contests/abc184/submissions/28792499)

前二つはより効率的な別解があり、関数型アプローチではこの計算をする機会が元々少ないのかもしれない。
184FはF問題だけあり、二分探索が本質的で、さらにそれを本質的にする、計算量を意識したテクニック「半分全列挙」が必要だった。

## 22.二分探索（その他）

[二分探索](/snippets/search/binary-search/)および
[三分探索](/snippets/search/ternary-search/)を参照。
