---
order: -01000
---
# 切り上げ除算

```haskell
-- @gotoki_no_joe
divrup x y = negate $ div (negate x) y
```

## 説明

あのアルゴリズムはどこ？の2より。

負の数に関して、 `quotRem` は商を0に寄せ、 `divMod` は商を負の無限大に寄せる。
上のコードはこれを利用している。

除数、被除数のどちらも正の範囲では、`divMod` と `quotRem` はどちらも切り捨てで除算を行う。
剰余が0でないときに結果を1増やすことで、素直に切り上げ除算を実現できる。

```haskell
divrup x y =
  case divMod x y of
    (q, 0) -> q
    (q, _) -> succ q
```

被除数に除数を足してから除算すると、商は1増える。 $(a+b) / b = a/b + 1$  
足し込む数を(除数-1)にしておくと、余りがある全ての場合の結果だけが1増えて、切り上げ除算になる。

```haskell
divrup x y = div (x+y-1) y
```

### 関連問題

- [ABC176 A Takoyaki](https://atcoder.jp/contests/abc176/tasks/abc176_a) - [ACコード](https://atcoder.jp/contests/abc176/submissions/22555330)
- [ABC195 B Many Oranges](https://atcoder.jp/contests/abc195/tasks/abc195_b) - [ACコード](https://atcoder.jp/contests/abc195/submissions/22556731)
- [ABC046 C AtCoDeerくんと選挙速報](https://atcoder.jp/contests/abc046/tasks/arc062_a) - [ACコード](https://atcoder.jp/contests/abc046/submissions/23709081)
