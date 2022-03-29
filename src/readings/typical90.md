---
order: -02000
---
# 競プロ典型 90 問

[競プロ典型 90 問](https://atcoder.jp/contests/typical90)

問題を知らせるtweetについていた解説が簡潔で明解なのでそちらを参照

一言ポイントは[『競プロ典型 90 問』非公式難易度表・ソースコード共有](https://docs.google.com/spreadsheets/d/1GG4Higis4n4GJBViVltjcbuNfyr31PzUY_ZY1zh2GuI/view)などより。


## ★3 (20問)



- [32. AtCoder Ekiden](https://atcoder.jp/contests/typical90/tasks/typical90_af)
　[ACコード](https://atcoder.jp/contests/typical90/submissions/29178035)  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/032.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/032.jpg?raw=true)
　小さい制約は順列全探索
- [38. Large LCM](https://atcoder.jp/contests/typical90/tasks/typical90_al)  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/038.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/038.jpg?raw=true)
　オーバーフローに注意  
Integerで押し切る[別解](https://atcoder.jp/contests/typical90/submissions/24802503)
- [44. Shift and Swapping](https://atcoder.jp/contests/typical90/tasks/typical90_ar)
　[ACコード](https://atcoder.jp/contests/typical90/submissions/29181077)  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/044.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/044.jpg?raw=true)
　見かけ上の変化をメモ
- [46. I Love 46](https://atcoder.jp/contests/typical90/tasks/typical90_at)
　[ACコード](https://atcoder.jp/contests/typical90/submissions/29181296)  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/046.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/046.jpg?raw=true)
　同じ意味のものをまとめて考える
- [48. I will not drop out](https://atcoder.jp/contests/typical90/tasks/typical90_av)
　[ACコード](https://atcoder.jp/contests/typical90/submissions/29181491)  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/048.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/048.jpg?raw=true)
　上界と下界を見積もる  
部分点が満点の半分を超えているという設定から、降順にソートすると部分点が満点までの残りより先に来ることが保証される、というのがポイントだった。その条件に気づかず、部分点の優先度付きキューを作り、部分点を取得するたびに満点までの残りをキューに追加するというアプローチで[解いていた](https://atcoder.jp/contests/typical90/submissions/24804113)が、もし前半1点後半一億点という問題があると、これでは見落とすので嘘解だ。
- [50. Stair Jump](https://atcoder.jp/contests/typical90/tasks/typical90_ax)
　[ACコード](https://atcoder.jp/contests/typical90/submissions/24805224)  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/050.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/050.jpg?raw=true)
　漸化式を立ててDPをしよう
- [52. Dice Product](https://atcoder.jp/contests/typical90/tasks/typical90_az)
　[ACコード](https://atcoder.jp/contests/typical90/submissions/24808554)  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/052.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/052.jpg?raw=true)
　因数分解をしよう  
因数分解の処理が解答プログラムに入るという意味ではない…
- [64. Uplift](https://atcoder.jp/contests/typical90/tasks/typical90_bl)
　[ACコード](https://atcoder.jp/contests/typical90/submissions/29182431)  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/064.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/064.jpg?raw=true)
　階差を考えよう
- [69. Colorful Blocks 2](https://atcoder.jp/contests/typical90/tasks/typical90_bq)
　[ACコード](https://atcoder.jp/contests/typical90/submissions/25184799)  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/069.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/069.jpg?raw=true)
　a^b mod m は繰り返し二乗法
- [75. Magic For Balls](https://atcoder.jp/contests/typical90/tasks/typical90_bw)
　[ACコード](https://atcoder.jp/contests/typical90/submissions/24930672)  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/075.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/075.jpg?raw=true)
　O(√N) での素因数分解・約数列挙
- [76. Cake Cut](https://atcoder.jp/contests/typical90/tasks/typical90_bx)
　[ACコード](https://atcoder.jp/contests/typical90/submissions/25718604)  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/076.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/076.jpg?raw=true)
　円環を列にして二倍にする  
単純に2倍する代わりに1/10以上になる分だけにして節約し、尺取り法も使用
- [79. Two by Two](https://atcoder.jp/contests/typical90/tasks/typical90_ca)
　[ACコード](https://atcoder.jp/contests/typical90/submissions/25729799)  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/079.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/079.jpg?raw=true)
　操作順序によらない
- [82. Counting Numbers](https://atcoder.jp/contests/typical90/tasks/typical90_cd)
　[ACコード](https://atcoder.jp/contests/typical90/submissions/25743266) 割り切れる側を選んで割る別解  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/082.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/082.jpg?raw=true)
　部分問題に分解する 数列の和の公式
- [84. There are two types of characters](https://atcoder.jp/contests/typical90/tasks/typical90_cf)
　ACコード [解法1](https://atcoder.jp/contests/typical90/submissions/29187614)
　[解法2](https://atcoder.jp/contests/typical90/submissions/25805705)  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/084.jpg?raw=true)
　[解説1](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/084-01.jpg?raw=true)
　[解説2](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/084-02.jpg?raw=true)
　余事象+ランレングス圧縮 累積的に計算しよう

## ★4 (14問 2残し)

- [1. Yokan Party](https://atcoder.jp/contests/typical90/tasks/typical90_a)
　[ACコード](https://atcoder.jp/contests/typical90/submissions/25824596)  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/001.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/001.jpg?raw=true)
　貪欲法、二分探索
- [3. Longest Circular Road](https://atcoder.jp/contests/typical90/tasks/typical90_c)
　[ACコード](https://atcoder.jp/contests/typical90/submissions/25826842)  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/003.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/003.jpg?raw=true)
　DFS、木の直径
- [8. AtCounter](https://atcoder.jp/contests/typical90/tasks/typical90_h)
　[ACコード](https://atcoder.jp/contests/typical90/submissions/22045597) DPが後ろからで不明瞭か  
　[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/008.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/008.jpg?raw=true)
　状態 DP による高速化
- [12. Red Painting](https://atcoder.jp/contests/typical90/tasks/typical90_l)
　[ACコード](https://atcoder.jp/contests/typical90/submissions/29123081)  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/012.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/012.jpg?raw=true)
　連結判定は Union-Find  
Unboxed版のUnionFindを改造すると[もっと速くできる](https://atcoder.jp/contests/typical90/submissions/25359675)
- [26. Independent Set on a Tree]()
　[ACコード](https://atcoder.jp/contests/typical90/submissions/29191166)  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/026.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/026.jpg?raw=true)
　二部グラフの性質を使おう  
素朴に葉から塗り分けたノードを集めて`concat`すると[TLEした](https://atcoder.jp/contests/typical90/submissions/29190973)。`concat`を避けるためだけに[RoseTreeを持ち出した大袈裟なACコード](https://atcoder.jp/contests/typical90/submissions/25371867)。上のACコードは、情報を集約する計算をStateモナドに包んで、根からの深さ優先走査の間を持ちまわせるようにした。
- [28. Cluttered Paper](https://atcoder.jp/contests/typical90/tasks/typical90_ab)
　[ACコード](https://atcoder.jp/contests/typical90/submissions/25453499)  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/028.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/028.jpg?raw=true)
　領域加算は二次元いもす法
- [34. There are few types of elements](https://atcoder.jp/contests/typical90/tasks/typical90_ah)
　[ACコード](https://atcoder.jp/contests/typical90/submissions/25837516)  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/034.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/034.jpg?raw=true)
　単調性を利用した尺取り法  
数字ごとのカウンタとしてIntMapを直接使うと[TLE](https://atcoder.jp/contests/typical90/submissions/25836511)する。
同じロジックをTypeScriptで書いたら[AC](https://atcoder.jp/contests/typical90/submissions/25835948)なのだが。
カウンタとしてmutable vectorを用いて、添え字を圧縮する「座標変換」を行ったものが上のAC。
- [42. Multiple of 9](https://atcoder.jp/contests/typical90/tasks/typical90_ap)
　[ACコード](https://atcoder.jp/contests/typical90/submissions/25845587)  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/042.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/042.jpg?raw=true)
　9 の倍数の性質
- [43. Maze Challenge with Lack of Sleep]()
　[ACコード]  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/043.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/043.jpg?raw=true)
　拡張 BFS／拡張ダイクストラ
- [58. Original Calculator](https://atcoder.jp/contests/typical90/tasks/typical90_bf)
　[ACコード](https://atcoder.jp/contests/typical90/submissions/29195349)  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/058.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/058.jpg?raw=true)
　周期性を考える  
べき乗を使った[別解ACコード](https://atcoder.jp/contests/typical90/submissions/29194545)  
解説のやり方はmutableなので抵抗あるが、時間計算量も空間計算量も圧倒的だ。
- [63. Monochromatic Subgrid](https://atcoder.jp/contests/typical90/tasks/typical90_bk)
　[ACコード](https://atcoder.jp/contests/typical90/submissions/29197230)  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/063.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/063.jpg?raw=true)
　変な制約に着目する 状態数が少ない変量を全探索
- [70. Plant Planning](https://atcoder.jp/contests/typical90/tasks/typical90_br)
　[ACコード](https://atcoder.jp/contests/typical90/submissions/29197457)  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/070.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/070.jpg?raw=true)
　x, y 独立に考える 中央値で絶対値の総和が最小に
- [72. Loop Railway Plan](https://atcoder.jp/contests/typical90/tasks/typical90_bt)
　[ACコード]  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/072.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/072.jpg?raw=true)
　"何通りか" の感覚 バックトラック
- [85. Multiplication](https://atcoder.jp/contests/typical90/tasks/typical90_cg)
　[ACコード](https://atcoder.jp/contests/typical90/submissions/29208366)  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/085.jpg?raw=true)
　[解説1](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/085-01.jpg?raw=true)
　[解説2](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/085-02.jpg?raw=true)
　約数の個数は少ない  
探索の範囲を別の方向で絞った[別解](https://atcoder.jp/contests/typical90/submissions/25806451)

## ★5

- [6. Smallest Subsequence](https://atcoder.jp/contests/typical90/tasks/typical90_f)
　[ACコード](https://atcoder.jp/contests/typical90/submissions/29049167)  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/006.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/006.jpg?raw=true)
　辞書順最小は前から貪欲法  
[別解](https://atcoder.jp/contests/typical90/submissions/29041667)
実際は $O(nk)$ なので、テストケースの盲点を突いている気がする。最後の `take k` は `loop` のサンクを切り捨てる小技
- [13. Passing](https://atcoder.jp/contests/typical90/tasks/typical90_m)
　[ACコード](https://atcoder.jp/contests/typical90/submissions/29123492)  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/013.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/013.jpg?raw=true)
　各頂点への最短経路はダイクストラ
- [21. Come Back in One Piece](https://atcoder.jp/contests/typical90/tasks/typical90_u)
　[ACコード](https://atcoder.jp/contests/typical90/submissions/29172961)  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/021.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/021.jpg?raw=true)
　強連結成分分解（SCC）をしよう  
標準ライブラリData.GraphのSCC計算を利用しただけ。自前で用意するべきか？
- [29. Long Bricks](https://atcoder.jp/contests/typical90/tasks/typical90_ac)
　[ACコード]  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/029.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/029.jpg?raw=true)
　座標圧縮で効率化（小課題 2） 区間に対する処理はセグメント木（満点）
- [30. K Factors](https://atcoder.jp/contests/typical90/tasks/typical90_ad)
　[ACコード](https://atcoder.jp/contests/typical90/submissions/25455930)  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/030.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/030.jpg?raw=true)
　素因数列挙の計算量は O(N log log N)
- [36. Max Manhattan Distance](https://atcoder.jp/contests/typical90/tasks/typical90_aj)
　[ACコード]  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/036.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/036.jpg?raw=true)
　マンハッタン距離は 45 度回転
- [37. Don't Leave the Spice](https://atcoder.jp/contests/typical90/tasks/typical90_ak)
　[ACコード]  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/037.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/037.jpg?raw=true)
　DP をセグメント木 (RMQ) で高速化
- [39. Tree Distance](https://atcoder.jp/contests/typical90/tasks/typical90_am)
　[ACコード]  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/039.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/039.jpg?raw=true)
　答えへの貢献度を考える：主客転倒
- [51. Typical Shop](https://atcoder.jp/contests/typical90/tasks/typical90_ay)
　[ACコード](https://atcoder.jp/contests/typical90/submissions/29213303)  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/051.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/051.jpg?raw=true)
　半分全列挙をしよう
- [56. Lucky Bag](https://atcoder.jp/contests/typical90/tasks/typical90_bd)
　[ACコード](https://atcoder.jp/contests/typical90/submissions/29217475)  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/056.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/056.jpg?raw=true)
　DP復元  
DPが済んだ後で復元する代わりに、可能な手をひとつ記録しながら進行しながら進める亜種で解いた。
最低価格+αのαだけをDPすることで幅を狭めると
[ArrayでもACする](https://atcoder.jp/contests/typical90/submissions/29214453)が、
素直にS円まで張ると[Mutable Vectorを用いないとTLE](https://atcoder.jp/contests/typical90/submissions/29216611)
してしまう  
復元を遅延評価で後回しにするのと、DPを遅らせずに進めるのをバランスさせるのが難しい。素直に実装したACコードは速いがHaskellらしくない。
- [60. Chimera](https://atcoder.jp/contests/typical90/tasks/typical90_bh)
　[ACコード](https://atcoder.jp/contests/typical90/submissions/29971580)  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/060.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/060.jpg?raw=true)
　両側から考える 最長増加部分列（LIS）  
ACにはランダムアクセスな配列が必要だった。  
IntMapなどを使ってpureに書くと[どうしても間に合わない](https://atcoder.jp/contests/typical90/submissions/29966985)
- [66. Various Arrays](https://atcoder.jp/contests/typical90/tasks/typical90_bn)
　[ACコード]  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/066.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/066.jpg?raw=true)
　期待値の線形性
- [68. Paired Information](https://atcoder.jp/contests/typical90/tasks/typical90_bp)
　[ACコード]  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/068.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/068.jpg?raw=true)
　クエリ先読み
- [73. We Need Both a and b](https://atcoder.jp/contests/typical90/tasks/typical90_bu)
　[ACコード]()  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/073.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/073.jpg?raw=true)
　木 DP に慣れる
- [81. Friendly Group]()
　[ACコード]()  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/081.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/081.jpg?raw=true)
　データを二次元にプロットする 二次元累積和
- [86. Snuke's Favorite Arrays]()
　[ACコード]()  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/086.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/086.jpg?raw=true)
　bit ごとに独立に考える
- [87. Chokudai's Demand]()
　[ACコード]()  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/087.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/087.jpg?raw=true)
　ワーシャルフロイド法 明らかに一定以上で満たす→答えで二分探索

## ★6

- [9. Three Point Angle](https://atcoder.jp/contests/typical90/tasks/typical90_i)
　[ACコード](https://atcoder.jp/contests/typical90/submissions/29052811)  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/009.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/009.jpg?raw=true)
　3つの真ん中を決め打ち全探索、偏角ソート
- [11. Gravy Jobs](https://atcoder.jp/contests/typical90/tasks/typical90_k)  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/011.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/011.jpg?raw=true)
　仕事は締切の早い順に（小課題 2）ソート順に DP（満点）
- [15. Don't be too close](https://atcoder.jp/contests/typical90/tasks/typical90_o)  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/015.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/015.jpg?raw=true)
　調和級数は $O(N \log N)$
- [19. Pick Two](https://atcoder.jp/contests/typical90/tasks/typical90_s)  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/019.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/019.jpg?raw=true)
　列の操作は区間DP
- [31. VS AtCoder]()
　[ACコード]  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/031.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/031.jpg?raw=true)
　Grundy 数を知っていますか？
- [45. Simple Grouping]()
　[ACコード]  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/045.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/045.jpg?raw=true)
　ビットDP	部分集合の部分集合は $3^N$ 通り

<!--
- [. ]()
　[ACコード]()  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/061.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/061.jpg?raw=true)
　**
-->


```
049	Flip Digits 2（★6）	2 sec	1024 MB	提出
054	Takahashi Number（★6）	2 sec	1024 MB	提出
057	Flip Flap（★6）	2 sec	1024 MB	提出
062	Paint All（★6）	2 sec	1024 MB	提出
074	ABC String 2（★6）	1 sec	1024 MB	提出
080	Let's Share Bit（★6）	2 sec	1024 MB	提出
083	Colorful Graph（★6）	3 sec	1024 MB	提出
088	Similar but Different Ways（★6）	2 sec	1024 MB	提出
```

## ★7

- [5. Restricted Digits](https://atcoder.jp/contests/typical90/tasks/typical90_e)  
[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/005.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/005.jpg?raw=true)
　行列累乗・ダブリング　余りを持って桁 DP（小課題 1）　同じ遷移の DP は行列累乗（満点）
- [17. Crossing Segments](https://atcoder.jp/contests/typical90/tasks/typical90_q)  
★7
　[出題](https://github.com/E869120/kyopro_educational_90/blob/main/problem/017.jpg?raw=true)
　[解説](https://github.com/E869120/kyopro_educational_90/blob/main/editorial/017.jpg?raw=true)
　余事象を考える（小課題 2）　BIT で高速化（満点）

```
023	Avoid War（★7）	8 sec	2048 MB	提出
025	Digit Product Equation（★7）	2 sec	1024 MB	提出
035	Preserve Connectivity（★7）	2 sec	1024 MB	提出
040	Get More Money（★7）	2 sec	1024 MB	提出
041	Piles in AtCoder Farm（★7）	2 sec	1024 MB	提出
047	Monochromatic Diagonal（★7）	2 sec	1024 MB	提出
053	Discrete Dowsing（★7）	2 sec	1024 MB	提出
059	Many Graph Queries（★7）	3 sec	1024 MB	提出
065	RGB Balls 2（★7）	3 sec	1024 MB	提出
071	Fuzzy Priority（★7）	3 sec	1024 MB	提出
077	Planes on a 2D Plane（★7）	1.5 sec	1024 MB	提出
089	Partitions and Inversions（★7）	3 sec	1024 MB	提出
090	Tenkei90's Last Problem（★7）
```
