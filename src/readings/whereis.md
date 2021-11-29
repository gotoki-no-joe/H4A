---
order: -01000
---
# あのアルゴリズムはどこ？

https://qiita.com/H20/items/1a066e242815961cd043

## 2.整数除算の端数

[切り上げ除算](../snippets/integer/divrup/) を参照。

## 3.約数列挙

[約数列挙](../snippets/integer/factors/) を参照。

## 4.素因数分解

[素因数分解](../snippets/integer/primefactors/) を参照。

## 5.素数（エラトステネスの篩）

[素数（エラトステネスの篩）](../snippets/integer/primes/) を参照。

## 6.BIT全探索

[総当たり](../snippets/search/exhaustive/) を参照。

## 7.Union-Find

[Union-Find](../ad/misc/unionfind/) を参照。

## 8.クラスカル法

[クラスカル法](../ad/optimize/kruskal/) を参照。

## 9.リストの中身を連結・結合して文字列に

Python特有の事情によるものか？

Haskellでは `show` と `unwords` あたりのことを知っていればいい話だろうか、何が言いたいのかよくわからない。

#### 関連問題

- [天下一プログラマーコンテスト2012 予選A B](https://atcoder.jp/contests/tenka1-2012-quala/tasks/tenka1_2012_qualA_2)
- [ARC039 A A - B problem](https://atcoder.jp/contests/arc039/tasks/arc039_a)
- [ABC192 Kaprekar Number](https://atcoder.jp/contests/abc192/tasks/abc192_c) - [ACコード](https://atcoder.jp/contests/abc192/submissions/20409087)
- [ABC137 C Green Bin](https://atcoder.jp/contests/abc137/tasks/abc137_c) - [ACコード](https://atcoder.jp/contests/abc137/submissions/22746507)
- [ABC199 C IPFL](https://atcoder.jp/contests/abc199/tasks/abc199_c) - [ACコード](https://atcoder.jp/contests/abc199/submissions/22747708) 何というimperative Haskell...

「ABC137 Cについて、順番前後しますが、カウンターの章も参照のことお願いします。」とのこと。

## 10.ソートの大小関係を操る（多次元配列のソート）

特に「多次元配列の」とか狭めることなく一般化して、任意の大小関係でのソートが

```haskell
Data.List.sortBy :: (a -> a -> Ordering) -> [a] -> [a]
```

でできる。例えば降順のソートは `sortBy (flip compare)` でよい。  
また、中置演算子の両辺に前処理を挿入する

```haskell
Data.Function.on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
```

も組み合わせると、

```haskell
sortBy (compare `on` fst) -- 対の左側だけを比較
sortBy (compare `on` (!! 1)) -- リストのリストを1番目の要素で比較
```

などとできる。

#### 関連問題

- [ABC128 B Guidebook](https://atcoder.jp/contests/abc128/tasks/abc128_b) - [ACコード](https://atcoder.jp/contests/abc128/submissions/22763098)
- [キーエンス プログラミング コンテスト 2020 B Robot Arms](https://atcoder.jp/contests/keyence2020/tasks/keyence2020_b) - 【ACコード】
- [ABC113 C ID](https://atcoder.jp/contests/abc113/tasks/abc113_c) - [ACコード](https://atcoder.jp/contests/abc113/submissions/22769268) 別アプローチの、Pごとに選り分けてからyで整列することを出題者は意図していそうな。

## 11.辞書

Python特有の事情で、辞書が存在しないキーへの代入を許さないものより許すものの方が使いやすいよ？という話かと。

Haskellでimmutableなコードを書いている分には関係してこないことかと思われる。
位置づけ的には、 `Data.Array.accumArray` のDP的な使い方を代わりにするべき文脈かもしれない。

#### 関連問題

- [ABC127 D Integer Cards](https://atcoder.jp/contests/abc127/tasks/abc127_d) - [ACコード](https://atcoder.jp/contests/abc127/submissions/12944460) Data.IntMapを使用
- [ABC188 F +1-1x2](https://atcoder.jp/contests/abc188/tasks/abc188_f) - 【ACコード】

他サイト
- [yukicoder No.1338 Giant Class](https://yukicoder.me/problems/no/1338) - 【ACコード】

## 12.カウンター

[数える](../snippets/integer/counter/) を参照。

## 13.ModInt

モジュロな数をPythonのクラスにしたものが便利だよ、という話らしい。
Haskellにはうまくfitさせられない感じなので、[考え中](modint/)ということで。

## 14.逆元

[モジュラ逆数](../snippets/integer/modrecip) を参照。

## 15.（モジュロの）階乗

（アルゴリズムロジックの方が、 $_nC_r$ にからめた話が書いてあってよい。）

要約すると、
大きな数の階乗とモジュロを要求される場合に、バカ正直に多倍長整数で計算していては間に合わないので、
乗算のたびにモジュロをとることで小さい整数に収めよう、ということと、
広い範囲に渡って階乗が必要な場合に、
それぞれの値に対して階乗関数を毎回呼び出さず、
$(n-1)!$ の結果に $n$ を乗じて $n!$ を求めなさい、ということ。

#### 関連問題

- [ABC 055 B Training Camp](https://atcoder.jp/contests/abc055/tasks/abc055_b) - [ACコード](https://atcoder.jp/contests/abc055/submissions/6332597)
- [ABC 065 C Reconciled?](https://atcoder.jp/contests/abc065/tasks/arc076_a) - [ACコード](https://atcoder.jp/contests/abc065/submissions/27526633)
- [ABC 185 C Duodecim Ferra](https://atcoder.jp/contests/abc185/tasks/abc185_c) - [ACコード](https://atcoder.jp/contests/abc185/submissions/18771391) Integer / [Intに収めた別解](https://atcoder.jp/contests/abc185/submissions/27551599)

他サイト
- [MojaCoder 入力1個数え上げ](https://mojacoder.app/users/bachoppi/problems/oneinput) - 【ACコード】良問です。だそうです。

## 16.コンビネーション

[二項係数](../ad/math/combination/) を参照。
