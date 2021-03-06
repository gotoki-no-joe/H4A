# Numeric

10進以外の整数値表記の読み書きをするモジュール。

```haskell
import Numeric
```

## 出力

`type ShowS = String -> String` は、表示したい何かを文字列化したものを、
与えられた文字列の前に付け加える、という関数の形にしたもので、`(++)`を回避するためのテクニック。

```haskell
-- 非負整数を表示する
-- 10進表記
showInt :: Integral a => a -> ShowS
-- 8進表記
showOct :: (Integral a, Show a) => a -> ShowS
-- 16進表記
showHex :: (Integral a, Show a) => a -> ShowS
```

これらは次の基本関数から作られているので、これの応用で2進表記も、16進を超えるものでも何でも作れる。
第２引数は `Data.Char.intToDigit` のような関数を指定する。

```haskell
showIntAtBase :: (Integral a, Show a)
              => a             -- 基数
              -> (Int -> Char) -- 文字化関数
              -> a             -- 表示する数
              -> ShowS
```

2進表記にするには次のようにすればよい。

```haskell
import Numeric
import Data.Char

showBinary :: Int -> ShowS
showBinary = showIntAtBase 2 intToDigit
```

## 入力

n進表記を読み込むこともNumericモジュールでできる。

```haskell
-- 符号なし整数を文字列から読み込む
-- 10進表記
readDec :: (Eq a, Num a) => ReadS a

-- 8進表記
readOct :: (Eq a, Num a) => ReadS a

-- 16進表記
readHex :: (Eq a, Num a) => ReadS a
```

`type ReadS a = String -> [(a,String)]` 型は、
文字列からa型の値の読み込みを試み、読み込めた値と残りの文字列を返す。
ただし、読み込み方が一意でない場合に対応するために、可能な全ての読み込み方をリストにして返す。

```text
> readHex "a0ZZ"
[(160,"ZZ")]
> readOct "129"
[(10,"9")]
```

（全ての読み込み方といっても、`readDec "129X" => [(1,"29X"),(12,"9X"),(129,"X")]` となる訳ではないようだ。）

これらはやはりプリミティブのインスタンスとして作られている。  
第２引数は `Data.Char.isDigit` のような関数を、  
第３引数は `Data.Char.digitToInt` のような関数を指定する。

```haskell
readInt :: Num a	 
        => a    -- 基数
        -> (Char -> Bool) -- 文字がこの表記の数字であるか判定する述語
        -> (Char -> Int)  -- 文字を1桁の数にする関数
        -> ReadS a
```

2進表記を読むには次のようにすればよい。

```haskell
import Numeric
import Data.Char

readBinary :: ShowS Int
readBinary = readInt 2 (\c -> c == '0' || c == '1') digitToInt
```
