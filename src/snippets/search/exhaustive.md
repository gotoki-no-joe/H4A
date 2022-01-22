---
order: -01000
---
# 総当たり

## 説明

$n$ とおりからひとつを選ぶ選択肢が $k$ 個あるとき、
全ての組み合わせは $n^k$ とおりとなる。
特に $n=2$ の場合の全ての組み合わせが、
$k$ 桁の2進数のカウントアップ（0...0 ～ 1...1）で生成できることから、
BIT全探索と呼ぶ界隈もあるようだが、多分専門用語ではない。

### 総当たり探索

$n$ 通りの選択肢から一つを選ぶ選択を、全部で $k$ 回行う必要があるとする。
選択を $m$ 回行った結果の中間状態を `s` とする。
もう一度選択を行うと、結果の状態は $n$ 個派生する。
この $m+1$ 回めの選択による状態変化を求める情報を `a` とする。
`s` と `a` から、 $n$ 個の状態を生成してリストで返す関数を `step :: a -> s -> [s]` とし、
一度も選択を行っていない初期状態を `ini` とすると、
$k$ 個の `a` のリストから、 $n^k$ 個の最終状態は次のようにして求められる。

```haskell
exhaustiveSearch :: (a -> s -> [s]) -> s -> [a] -> [s]
exhaustiveSearch step ini = foldl (\ss a -> concatMap (step a) ss) [ini]
```

### BIT全探索

選択が2択で、片方の選択では状態は変化しない、という特別な場合を「BIT全探索」と呼んでいるようだ。
状態 `s` に対して情報 `a` の選択により変化する状態を返す関数を `f :: a -> s -> s` としたとき、
$2^k$ 個の最終状態は次のように `exhaustiveSearch` を利用して求められる。

```haskell
bitSearch f = exhaustiveSearch (\a s -> [s, f a s])
```

状態が変化しない側の状態リストは元のままなので、次のように専用の関数にもできる。  
（上の定義とは結果の並び順が異なる。）

```haskell
bitSearch :: (a -> s -> s) -> s -> [a] -> [s]
bitSearch f ini = foldl (\ss a -> map (f a) ss ++ ss) [ini]
```

## 関連問題

- [ABC167 C Skill Up](https://atcoder.jp/contests/abc167/tasks/abc167_c) - [ACコード](https://atcoder.jp/contests/abc167/submissions/22739273) bitSearch
- [ABC182 C To 3](https://atcoder.jp/contests/abc182/tasks/abc182_c) - [ACコード](https://atcoder.jp/contests/abc182/submissions/22739342) この問題は[より特化した別解](https://atcoder.jp/contests/abc182/submissions/18003556)がある
- [ABC190 C Bowls and Dishes](https://atcoder.jp/contests/abc190/tasks/abc190_c) - [ACコード](https://atcoder.jp/contests/abc190/submissions/22739391) exhaustiveSearch
- [ARC114 A Not coprime](https://atcoder.jp/contests/arc114/tasks/arc114_a) - 【ACコード】
- [ABC197 C ORXOR](https://atcoder.jp/contests/abc197/tasks/abc197_c) - 【ACコード】
- [ABC128 C Switches](https://atcoder.jp/contests/abc128/tasks/abc128_c) - 【ACコード】
- [ABC147 C HonestOrUnkind2](https://atcoder.jp/contests/abc147/tasks/abc147_c) - 【ACコード】
- [ABC104 C All Green](https://atcoder.jp/contests/abc104/tasks/abc104_c) - 【ACコード】

<!--
### 英語でなんて言うの？

「総当り戦」はround-robinというらしいが、計算機関係ではこれは別の意味があるので使えない。  
「総当たり攻撃」bruite-force attackは計算機関係の用語であるが、これもね…
-->
