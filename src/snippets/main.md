---
order: -10000
---
# メインルーチン

## コード

### String版

```haskell
import Control.Applicative
import Control.Monad

main = do
-- 読み込み
  n <- readLn
  [a,b,c] <- getLnInts
  xys <- replicateM n getLnInts
-- 本体
  let ans = compute n a b c xys
-- 出力
  print ans
  putStrLn $ if ans then "Yes" else "No"
  putStrLn ans
  putStrLn $ unwords $ map show ans
  putStrLn $ foldr ($) "" $ intersperse (' ' :) $ map shows ans
  mapM_ print ans

getLnInts :: IO [Int]
getLnInts = map read . words <$> getLine

compute :: Int -> [Int] -> [[Int]] -> [Int Bool]
compute n a b c xys =
```

### ByteString版

```haskell
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.List

main = do
-- 読み込み
  [n] <- bsGetLnInts
  [a,b,c] <- bsGetLnInts
  xys <- replicateM n bsGetLnInts
-- 本体
  let ans = compute n a b c xys
-- 出力
  print ans
  putStrLn ans
  putStrLn $ if ans then "Yes" else "No"
  putStrLn $ foldr ($) "" $ intersperse (' ' :) $ map shows ans
  mapM_ print ans

bsGetLnInts :: IO [Int]
bsGetLnInts = unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine

compute :: Int -> [Int] -> [[Int]] -> [Int Bool]
compute n a b c xys = ...
```

## 説明

### 基本

競技プログラミングでは、参加者がプログラミング言語を自由に選択できるようにするため、
プログラムが処理するべき入力データはファイルまたは標準入力からテキストで読み込み、
処理結果の出力データもテキストで出力するようになっていることが多い。

Haskellで入出力を扱うには、IOモナドを使う。ざっくり、do ブロックの中の行が、順に実行される。
最も素朴な方法としては、以下のコードをテンプレートとし、問題の入力形式に合わせて使用できる。
コピペして、問題の形式に合わせて読み込み部をトリミングし、出力部を選ぶ。
`compute`の型シグネチャで`read`の型を固定する。
AtCoderでときどきある、"Yes"か"No"を出力するタイプの問題は、Boolで返して16行めを使う。

```haskell #
main = do
-- 標準入力から1行読み、String型の値をl1に束縛する
  l1 <- getLine
  l2 <- getLine
-- 標準入力から（残り）全てを読み込み、String型の値をcoに束縛する
  co <- getContents
-- l1をRead型クラスの型の値として解釈する。純粋な計算はletで行う
  let n = read l1
  let [a,b,c] = map read $ words l2
  let xys = map (map read . words) $ lines co
-- 各引数の型はcomputeのシグネチャで指定すると見通しがよい
  let ans = compute n a b c xys
-- Show型クラスの値を標準出力にテキストで出力する
  print ans
-- Yes/Noで答える問題は、Boolで返してこれで出力する
  putStrLn $ if ans then "Yes" else "No"
-- String型は、ダブルクオートがつかないようにこちらで出力する
  putStrLn ans
-- 同じ型の値を複数、空白を挟んで一行に表示する
  putStrLn $ unwords $ map show ans
-- 同じ型の値を複数、１行に一つずつ表示する
  mapM_ print ans

-- AtCoderでよくある型に編集しやすくしている
compute :: Int -> [Int] -> [[Int]] -> [Int Bool]
compute n a b c xys =
```

「二つの値が書かれた行がn行続く」のような場合、この二つの値をリストでなくタプルで扱いたくなるが、そこは割り切った方がよい。その変換は単純なタイムロスになる。

### Applicative

ここで、Control.Applicativeの機能を使うと、モナドの結果にさらに純粋な関数を適用できる。これにより、上のコードの読み込み部分がコンパクトにできる。

```haskell
import Control.Applicative

main = do
-- 値が1つのとき
  n <- readLn -- read <$> getLine
-- 複数の値のとき
  [a,b,c] <- map read . words <$> getLine
-- 文字列のまま読むとき
  s <- getLine
-- (残り)全部読んで行の内容のリストのリストに一度に読み込む
  xys = map (map read . words) . lines <$> getContents
-- 本体
  let ans = compute n a b c xys
-- 出力
  print ans
  putStrLn ans
  putStrLn $ if ans then "Yes" else "No"
  putStrLn $ unwords $ map show ans
  putStrLn $ foldr ($) "" $ intersperse (' ' :) $ map shows ans
  mapM_ print ans

compute :: Int -> [Int] -> [[Int]] -> [Int Bool]
compute n a b c xys =
```

### 個数をちゃんと数える

「残り全部を読む」でAtCoderについてたいていの場合は用が足りるが、
指定された行数をちゃんと読む方がいいような気がしてきたのでもう少し直すと次のようになる。

```haskell
import Control.Monad

main = do
  n <- readLn
  xys <- replicateM n (map read . words <$> getLine)
  ...
```

### ByteString

Haskellのしている、文字列を文字のリストとして扱うやり方は、Preludeの関数で処理できるためとっつきやすいが、処理効率の観点からは絶望的である。
$2 \times 10^5$ 個のデータの組を読み込む、というような場面でString版のテンプレートを用いると、読み込みでかなりの時間を消費する。
Data.ByteStringライブラリを利用すると、この問題に対処できる。

行に数が1つだけの場合、

```haskell
  Just (n,_) <- BS.readInt li <$> BS.getLine
```

のようにして読み込めるが、任意個の数からなる１行を読み込むアクション `bsGetLnInts` で全て統一的に扱うようにしてみた。

出力側が問題になることはあまりないのでこちらはString版から変更はない。
コードは上に示した。

## 正格評価ディレクティブ

```haskell
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
```

遅延評価のためにサンクが溜まって無駄に重いような気がする場合、コンパイラディレクティブの指定で、可能な箇所を先行評価するように指示できる。

しかし、大抵の場合は単なる思いすごしで、それほどの効果は得られない。
GHCは充分に速い。
