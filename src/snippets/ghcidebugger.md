---
order: -95000
---
# GHCi デバッガ

http://www.kotha.net/ghcguide_ja/latest/ghci-debugger.html

実行前

- `:b[reak] 行番号 or 関数名` ブレークポイントを設置する
- `:show breaks` ブレークポイントの一覧を表示
- `:delete 番号 or *` ブレークポイントを削除
- `:step 式` 式のステップ評価を開始
- `:trace 式` トレースを開始
- `:set -fbreak-on-exception` 例外(Ctrl-Cも含む)でブレークするよう設定

ブレーク中

- `:print` 変数の値を表示（サンクはそのまま）
- `:force` 変数の値を表示（サンクを評価）
- `:list` 現在位置の前後のソースを表示

実行制御

- `:continue` 実行を再開
- `:step` ステップ実行

トレース実行下でのブレーク中

- `:hist[ory]` 評価ステップの履歴を表示
- `:back` ステップ履歴を戻る
- `:forward` ステップ履歴を進める
