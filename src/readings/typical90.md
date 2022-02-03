---
order: -02000
---
# 競プロ典型 90 問

https://atcoder.jp/contests/typical90

問題を知らせるtweetについていた解説が簡潔で明解なのでそちらを参照

一言ポイントは[『競プロ典型 90 問』非公式難易度表・ソースコード共有](https://docs.google.com/spreadsheets/d/1GG4Higis4n4GJBViVltjcbuNfyr31PzUY_ZY1zh2GuI/view)などより。

1. [Yokan Party](https://atcoder.jp/contests/typical90/tasks/typical90_a)
　[ACコード](https://atcoder.jp/contests/typical90/submissions/25824596)  
★4
　[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/001.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/001.jpg?raw=true)
　貪欲法、二分探索
2. [Encyclopedia of Parentheses](https://atcoder.jp/contests/typical90/tasks/typical90_b)
　[ACコード](https://atcoder.jp/contests/typical90/submissions/28855241)  
★3
　[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/002.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/002.jpg?raw=true)
　bit全探索  
[丁寧に生成する別解](https://atcoder.jp/contests/typical90/submissions/21988452) の方が好み。
3. [Longest Circular Road](https://atcoder.jp/contests/typical90/tasks/typical90_c)
　[ACコード](https://atcoder.jp/contests/typical90/submissions/25826842)  
★4
　[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/003.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/003.jpg?raw=true)
　DFS、木の直径
4. [Cross Sum](https://atcoder.jp/contests/typical90/tasks/typical90_d)
　[ACコード](https://atcoder.jp/contests/typical90/submissions/23707569)  
★2
　[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/004.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/004.jpg?raw=true)
　適切な前処理、包除原理  
しかし素朴に`transpose`していては間に合わなかった
5. [Restricted Digits](https://atcoder.jp/contests/typical90/tasks/typical90_e)  
★7
　[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/005.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/005.jpg?raw=true)
　行列累乗・ダブリング　余りを持って桁 DP（小課題 1）　同じ遷移の DP は行列累乗（満点）
6. [Smallest Subsequence](https://atcoder.jp/contests/typical90/tasks/typical90_f)  
★5
　[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/006.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/006.jpg?raw=true)
7. [CP Classes](https://atcoder.jp/contests/typical90/tasks/typical90_g)  
★3
　[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/007.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/007.jpg?raw=true)
　要素の検索はソートして二分探索  
命令型の二分探索はHaskellのIntMapだという[別解](https://atcoder.jp/contests/typical90/submissions/21994000)で解いた。
8. [AtCounter](https://atcoder.jp/contests/typical90/tasks/typical90_h)
　[ACコード](https://atcoder.jp/contests/typical90/submissions/22045597) DPが後ろからで不明瞭か  
★4
　[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/008.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/008.jpg?raw=true)
　状態 DP による高速化
