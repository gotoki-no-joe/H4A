---
order: -03000
---
# AtCoder Library

- [AC(AtCoder) Library Document (production)](https://atcoder.github.io/ac-library/production/document_ja/)
- [ACL-for-python wiki!](https://github.com/shakayami/ACL-for-python/wiki)
- AtCoder Library 解読 〜Pythonでの実装まで〜
  - [DSU編](https://qiita.com/R_olldIce/items/93b8f13e0d33da4ac331)
  - [Fenwick_Tree編](https://qiita.com/R_olldIce/items/f2f7930e7f67963f0493)
  - [math編](https://qiita.com/R_olldIce/items/3e2c80baa6d5e6f3abe9)
  - [Segtree編](https://qiita.com/R_olldIce/items/32cbf5bc3ffb2f84a898)

## データ構造

- Fenwick Tree フェネック木
- segtree　[セグメント木](/ad/query/segment-tree)
- lazysegtree 遅延セグメント木
- string 文字列アルゴリズム詰め合わせ
  - suffix array 
  - lcp array
  - z algorithm

## 数学

- math 数論的アルゴリズム詰め合わせ
  - pow_mod　$x^n \mod m$
  - inv_mod　mod逆元　→[モジュラ逆数](/snippets/integer/modrecip)
  - crt　→[中国剰余定理](/ad/math/crt)
  - floor_sum　$\sum_{i=0}^{n-1} \lfloor\frac{ai+b}{m}\rfloor$
- convolution　畳み込み
- modint　自動でmodを取る整数

## グラフ

- dsu　→[Union-Find](/ad/misc/unionfind)
- maxflow 最大フロー問題
- mincostflow Minimum-cost flow problem
- scc 有向グラフを強連結成分分解
- twosat 2-SATを解く
