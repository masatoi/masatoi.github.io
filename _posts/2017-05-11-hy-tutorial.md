---
layout: post
title: "ゼロから始めるHy (hylang)"
description: ""
category: 
tags: [lisp,hylang,python]
---
{% include JB/setup %}

* TOC
{:toc}

# Hyについて

-   [hylang/hy: A dialect of Lisp that's embedded in Python](https://github.com/hylang/hy){:target="_blank"}
-   [ドキュメント: Hy's documentation](http://docs.hylang.org/en/stable/index.html){:target="_blank"}

Hy(あるいはhylang)はPython上で動くLisp方言である。
構文はClojure風ではあるが、関数型というわけでもなくマルチパラダイム言語なので、使用感はむしろCommon Lispに近い印象がある。
他のLisp系言語と同様にREPLでのインタラクティブな開発ができ、マクロを使って構文を拡張できる。

HyのコードはPythonのAST(抽象構文木)へ変換され、そこから先は通常のPythonと同じように実行される。そのためPythonと高度な互換性があり、HyからPythonを呼ぶこともできるし、PythonからHyを呼ぶこともできる。

ASTを経由してHyのコードをPythonのソースコードやバイトコードに変換することもできる。
HyにはPythonのバイトコードへのコンパイラ(`hyc`)と、HyのソースコードからPythonのソースコードへのトランスパイラ(`hy2py`)が付属している。

# インストール

## Hyのインストール

Hyの現行の安定版のバージョンは0.20.0であり、1.0系のアルファ版の開発が進んでいる。Pythonのバージョンは3.6以降をサポートしている。1.0系ではかなりAPIが変わる予定だが、ここでは0.20.0時点について解説する。

Hy自体のインストールは以下のようにする。

```
$ pip install hy

# alpha版をインストールする場合
$ pip install hy==1.0a3
```
これで`hy`コマンド、`hyc`コマンド、`hy2py`コマンドがインストールされる。

Hyのバージョンを確認する。
```
$ hy -v
hy 0.20.0
```
以降は単にhyコマンドを打ち込めばREPLが起動する。終了時にはREPLに`(quit)`と打ち込む。

### Hello, world

次に、`hello.hy`というファイルに以下のように書いてみよう。

```
(print "Hello, world!")
```
この`.hy`拡張子のファイルをhyコマンドの引数として与えれば、そのファイルを実行する。
```
$ hy hello.hy
Hello, world!
```
次に、Pythonのバイトコードへコンパイルしてみる。これには`hyc`コマンドを使う。
```
$ hyc hello.hy
Compiling hello.hy
```
これを実行すると、hello.hyと同じディレクトリに`__pycache__`という名前のディレクトリが出来ている。この中にバイトコンパイルに使用したPython実装に対するバイトコードが入っている。

バイトコードは以下のようにpythonコマンドで実行できる。
```
$ python __pycache__/hello.cpython-38.pyc 
Hello, wold!
```
HyからPythonへのトランスパイルには、`hy2py`コマンドを使う。[^1]
```
$ hy2py hello.hy
print('Hello, wold!')
```
[^1]:ただし現状ではgensymを使うマクロがあったりするとエラーになってしまう模様。

また、Hyコマンドを`--spy`オプションを付けて起動すると、式の評価と同時にhy2pyの結果がプリントされる。

```
$ hy --spy
hy 0.20.0 using CPython(default) 3.8.3 on Linux
=> (print "Hello, world!")
print('Hello, world!')

Hello, world!
```

# 開発環境の整備
## Emacs(hy-mode)
### インストール

- [hy-mode](https://github.com/hylang/hy-mode){:target="_blank"}

MELPAからインストールできるので、Emacsの設定ファイル(~/.emacsなど)にpackage-install用の設定を書いておく。
``` elisp
(require 'package)
(add-to-list 'package-archives 
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
```

`M-x package-install RET hy-mode`とすればインストールされる。

### Emacsの設定例
Emacsの設定ファイルに.hy拡張子のファイルを読み込むとhy-modeに切り替わる設定を書いておく。

``` elisp
(autoload 'hy-mode "hy-mode"
  "Mode for editing Hylang source files"
  t)

(setq auto-mode-alist (append '(("\\.hy$" . hy-mode)) auto-mode-alist))
```

また、式を評価したときの動きが気に入らなかったのでカスタマイズしておく。
```elisp
(add-hook
 'hy-mode-hook
 (lambda ()
   ;; 式を評価するとREPLにカーソルが行ったままになってしまうので、コードのバッファに戻ってくるようにする
   (defun my-hy-shell-eval-last-sexp ()
     (interactive)
     (let ((c (current-buffer)))
       (hy-shell-eval-last-sexp)
       (switch-to-buffer-other-window c)))

   (defun my-hy-shell-eval-region ()
     (interactive)
     (let ((c (current-buffer)))
       (hy-shell-eval-region)
       (switch-to-buffer-other-window c)))

   (define-key hy-mode-map "\C-x\C-e" 'my-hy-shell-eval-last-sexp)
   (define-key hy-mode-map "\C-c\C-r" 'my-hy-shell-eval-region)))
```


### REPLの起動
Emacsで.hy拡張子のファイルを開くと`hy-mode`になる。この状態で`M-x run-hy`とすると、`*Hy*`バッファにREPLが表示される。

以降は`C-x C-e`で式ごとに評価したり、`C-c C-r`でリージョンで評価したりできるようになる。

![hy-mode.png](/images/hy-mode.png)

# Hyのチュートリアル

- [公式のHyのチュートリアル](http://docs.hylang.org/en/stable/tutorial.html){:target="_blank"}

## 関数定義

関数定義は以下のようにする。docstringの位置が違う以外はほぼClojureと同じだ。

``` lisp
(defn fact [n]
  "Docstring"
  (if (= n 0)
    1
    (* n (fact (- n 1)))))

(print (fact 10))
;; 3628800
```

### 引数オプション（ラムダリスト）

関数やマクロのラムダリストには`&optional`や`&rest`や`&kwargs`を指定できる。

`&optional`はCommon Lispのオプショナル引数とキーワード引数が混ざったようなもので、キーワードを指定しないとオプショナル引数のように振る舞うが、キーワードの名前を直接指定することで最初のキーワード引数を飛ばして二番目のキーワードだけを指定するようなことができる。

``` lisp
(defn optional-arg [pos1 pos2 &optional keyword1 [keyword2 42]]
  [pos1 pos2 keyword1 keyword2])

(optional-arg 'pos1 'pos2 'key1)         ; => ['pos1', 'pos2', 'key1', 42]
(optional-arg 'pos1 'pos2 :keyword2 420) ; => ['pos1', 'pos2', None, 420]
```

`&rest`は可変長の引数を取り、それを関数/マクロの中で1つのリスト(実際にはPythonのタプル型)として扱える。

例えば次のコードは可変長の引数を取ってその総和を取る関数の定義になる。

``` lisp
(defmacro incf [var &optional [diff 1]]
  `(setv ~var (+ ~var ~diff)))

(defn plus [&rest args]
  (let ((sum 0))
    (for [i args] (incf sum i))
    sum))
```

`&kwargs`は&restのキーワード版みたいなもので、&restがタプルに対応したのに対して、&kwargsはディクショナリ型に対応する。

``` lisp
(defn some-func [foo bar &rest args &kwargs kwargs]
  (import pprint)
  (pprint.pprint (, foo bar args kwargs)))

(some-func 'foo 'bar 'arg1 'arg2)
;; => ('foo', 'bar', ('arg1', 'arg2'), {})

(some-func 'foo 'bar 'arg1 'arg2 :key1 'val1 :key2 'val2)
;;=> ('foo', 'bar', ('arg1', 'arg2'), {'key1': 'val1', 'key2': 'val2'})
```

## 変数
変数の宣言と代入の区別はないように見える。それぞれに`def`と`setv`が用意されているが多分同じもので、単なる代入演算子( Pythonの`=` )だと思われる。
``` lisp
(def x 10)
(setv y 20)
(+ x y) ; => 30
```

PythonはLisp-1なので、HyもLisp-1である。すなわち、関数と変数で名前空間が分かれていない。従って、無名関数を使ってSchemeっぽい関数定義もできる。無名関数はClojureと同様に`fn`で作れる。

``` lisp
(def fact2
  (fn [n]
    (if (= n 0)
      1
      (* n (fact2 (- n 1))))))
```

### letが無い！？ → マクロで定義する
どうも`let`が用意されていないように見える。Pythonとの対応的には全てを`setv`でやれということなのだろうか。とはいえletはラムダ式のシンタックスシュガーでしかないことを思い起せば簡単に定義できる。([参考: On Lisp --- 古典的なマクロ](http://www.asahi-net.or.jp/~kc7k-nd/onlispjhtml/classicMacros.html){:target="_blank"})

マクロ定義はCommon LispやClojureの`defmacro`とほとんど同じになっている。バッククォートとアンクォートを使うところも同じだ。アンクォートにはClojureと同じく`~`、スプライシングアンクォート(リストの埋め込み)には`~@`を使う。

マクロの展開形を確認するためには`macroexpand`を使う。また、変数捕捉を避けるために`gensym`で重複しないことが保証されているシンボルを作れる。

``` lisp
(defmacro let [var-pairs &rest body]
  (setv var-names (list (map first  var-pairs))
        var-vals  (list (map second var-pairs)))
  `((fn [~@var-names] ~@body) ~@var-vals))

(macroexpand '(let ((one 1)
                    (two 2)
                    (three 3))
                (print one)
                (print two)
                (print three)))
;; (('fn' ['one' 'two' 'three'] ('print' 'one') ('print' 'two') ('print' 'three')) 1 2 3)

(let ((one 1)
      (two 2)
      (three 3))
  (print one)
  (print two)
  (print three))
;; 1
;; 2
;; 3
```
## 制御構造
まず真理値だが、基本Pythonと同じで、NoneとFalse、数字の0、空のシーケンス、空のディクショナリは偽、それ以外は真になる。それにしても0が偽になるのはどうかと思う。

``` lisp
(if (or None False 0 0.0 '() [] {}) 't 'f) ; => 'f'
```
`cond`はClojureと少し違って、角括弧が必要なことに注意。

``` lisp
(let ((i 20))
  (cond [(> i 30) (print "That variable is too big!")]
        [(< i 10) (print "That variable is too small!")]
        [True     (print "That variable is jussssst right!")]))
;; That variable is jussssst right!
```

副作用を目的とした順次実行にはClojureと同様に`do`を使う。これはCommon Lispのprogn、Schemeのbeginに相当する。Common LispとSchemeではdoはループのためのマクロなのでややこしい。

ifでは評価される部分に式が一つしか書けないので、複数の処理を書くためにはdoを使って処理をまとめる必要がある。

``` lisp
(if True
  (do
    (print "this is if true")
    (print "and why not, let's keep talking about how true it is!"))
  (print "this one's still simply just false"))
;; this is if true
;; and why not, let's keep talking about how true it is!
```

上の例でifにelse部分が無い場合は、`when`を使った方が意図が明確になる。(副作用を目的としていることが分かるので)

``` lisp
(when True
  (print "this is if true")
  (print "and why not, let's keep talking about how true it is!"))
;; this is if true
;; and why not, let's keep talking about how true it is!
```

繰り返しには`for`を使う。これはPythonのforそのままで、局所変数とシーケンスの組に続いてループ本体を書く。

``` lisp
(for [i (range 4)]
  (print (+ "'i' is now at " (str i))))

(for [i '(0 1 2 3)]
  (print (+ "'i' is now at " (str i))))

(for [i [0 1 2 3]]
  (print (+ "'i' is now at " (str i))))
  
;; 'i' is now at 0
;; 'i' is now at 1
;; 'i' is now at 2
```

関数定義のところで再帰でfactを書いたが、`(fact 1000)`とかにすると再帰が深すぎるといってエラーになってしまう。Hyは末尾再帰最適化はしないが、Clojureと同様に`loop/recur`マクロがあるので末尾再帰呼び出しを単純なループに変換できる。

``` lisp
(require [hy.contrib.loop [loop]])

(defn fact3 [n]
  (loop [[cnt 1] [acc 1]]
        (if (= cnt n)
          acc
          (recur (inc cnt) (* acc cnt)))))

(fact3 1000) ; => 4023872600770937735437024339 ...
```

## Pythonの呼び出し
パッケージを読み込む`import`はPythonとほとんど同じで、実行時に評価される。複数のimportを一つにまとめられる。
``` python
import sys
import os.path
```
``` lisp
(import sys os.path)
```
次に、PythonとHyでの`from`や`as`の使い方の対応を見る。
``` python
from os.path import exists, isdir as is_dir, isfile as is_file
from sys import *
import numpy as np
```
``` lisp
(import [os.path [exists
                  isdir :as dir?
                  isfile :as file?]]
        [sys [*]]
        [numpy :as np])
```
ただし、マクロは実行時でなくコンパイル時に評価されるので、マクロを含むパッケージはimportの代わりに`require`を使って読み込む。
``` lisp
(require [hy.contrib.loop [*]])    ; loop/recurはここ
(require [hy.extra.anaphoric [*]]) ; アナフォリックマクロのパッケージ
```

次に、NumPyの配列を作って属性にアクセスしてみる。
``` lisp
(def arr (np.array [[1 2 3]
                    [4 5 6]
                    [7 8 9]]))

arr.ndim  ; => 2
arr.size  ; => 9
arr.dtype ; => dtype('int64')
```
メソッド呼び出しは次の2つの書き方が両方通る。
``` lisp
(arr.sum)  ; => 45
(.sum arr) ; => 45
```
ほかにも色々やってみる。
``` lisp
;; スカラー倍
(* arr 3)
;; array([[ 3,  6,  9],
;;        [12, 15, 18],
;;        [21, 24, 27]])

;; アダマール積(要素積)
(* arr arr)
;; array([[ 1,  4,  9],
;;        [16, 25, 36],
;;        [49, 64, 81]])

;; 行列積
(np.dot arr arr)
;; array([[ 30,  36,  42],
;;        [ 66,  81,  96],
;;        [102, 126, 150]])

;; 一様乱数で100x100行列を作って行列積を取る
(import [numpy.random :as rand])

(def bigarr1 (rand.rand 100 100))
(def bigarr2 (rand.rand 100 100))

(np.dot bigarr1 bigarr2)

;; array([[ 28.38096367,  28.63420504,  28.01482173, ...,  27.26330009,
;;          25.56717227,  27.39401733],
;;        [ 25.26386499,  23.78039895,  22.81641922, ...,  24.37012314,
;;          22.31017675,  22.20606049],
;;        [ 24.79624411,  23.11758526,  24.45533016, ...,  24.47093385,
;;          22.3951194 ,  24.02735416],
;;        ..., 
;;        [ 25.65465691,  25.7403632 ,  23.54518075, ...,  24.36247407,
;;          21.92434498,  23.04834359],
;;        [ 22.37135022,  21.32717967,  21.92101116, ...,  20.93922527,
;;          20.07961519,  20.54109093],
;;        [ 27.50945536,  25.99902791,  25.73058543, ...,  25.71283456,
;;          23.86456424,  25.27311888]])
```

# まとめ
- Hy = Python + S式 + マクロ
    - Clojureライクな構文
    - Pythonのライブラリがそのまま使える
- 次はChainerのサンプルコードをHyに翻訳してみる、かも
