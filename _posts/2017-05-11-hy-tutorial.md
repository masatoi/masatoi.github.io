---
layout: post
title: "ゼロから始めるHy(hylang)"
description: ""
category: 
tags: [lisp,hylang,python]
---
{% include JB/setup %}

* TOC
{:toc}

# Hy(hylang)について

-   [hylang/hy: A dialect of Lisp that's embedded in Python](https://github.com/hylang/hy){:target="_blank"}
-   [ドキュメント: Hy's documentation](http://docs.hylang.org/en/latest/){:target="_blank"}

HyはPythonのVM上で動くLisp方言で、Clojureによく似た構文を持つ。他のLispと同様にREPLでのインタラクティブな開発ができる。Lispなので本物のマクロが使える。

Pythonと高度な互換性がある。HyからPythonを呼ぶこともPythonからHyを呼ぶこともできる。

構文解析のオーバーヘッドが多少重いものの、Pythonのバイトコードへのコンパイラが付属しているので実用的には問題ない。またHyからPythonのソースコードへのトランスパイラも付属している。現状だとgensymの生成するシンボルをPythonのシンボルに変換できていないのでgensymを使うマクロがあるとエラーになってしまう。

# Hyのインストール
~~Python2でも3でも動くようだが、Python3の方がサポートされている機能が多い。~~
最新版のHyではPython3.3以降のみがサポートされる。自分は[pyenv](https://github.com/pyenv/pyenv){:target="_blank"}で最新のPython3.6.0をインストールした。それから以下のようにするとGithub上のHyの最新版がインストールできる。

```
pip install git+https://github.com/hylang/hy.git
```

## Emacsのhy-mode

簡易的なものだが、[Emacs用のhy-modeが用意されている](https://github.com/hylang/hy-mode){:target="_blank"}。MELPAから入るので、.emacsにpackage-install用の設定を書いて、
``` elisp
(require 'package)
(add-to-list 'package-archives 
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
```
しかるのちに`M-x package-install RET hy-mode`すればインストールされる。

### .emacsの設定例
``` elisp
;;; Hy-mode

(autoload 'hy-mode "hy-mode"
  "Mode for editing Hylang source files"
  t)

(setq auto-mode-alist (append '(("\\.hy$" . hy-mode)) auto-mode-alist))

(add-hook 'hy-mode-hook
	  (lambda ()
            (setq hy-font-lock-keywords
                  (append '(("(\\|)" . paren-face))
                          hy-font-lock-keywords))))
```

### REPLの起動
.hy拡張子のファイルを開くと、`hy-mode`になる。この状態で`M-x inferior-lisp`とすると、`*inferior-lisp*`バッファにREPLが表示される。

以降は`C-x e`で式ごとに評価したり、`C-c l`でファイルをロードしたりできるようになる。

![hy-mode.png](/images/hy-mode.png)

# Hyのチュートリアル

[Hyのチュートリアル](http://docs.hylang.org/en/latest/tutorial.html){:target="_blank"}を見ながら色々試してみた。

## 関数定義

docstringの位置が違う以外はほぼClojureと同じ(Clojureは何故あの位置なのだろう・・・)。

``` clojure
(defn fact [n]
  "Docstring"
  (if (= n 0)
    1
    (* n (fact (- n 1)))))
```

### 引数オプション（ラムダリスト）

関数やマクロのラムダリストには`&optional`や`&rest`や`&kwargs`を指定できる。

&optionalはCommon Lispのオプショナル引数とキーワード引数が混ざったようなもので、キーワードを指定しないとオプショナル引数のように振る舞うが、キーワードの名前を直接指定することで最初のキーワード引数を飛ばして二番目のキーワードだけを指定するようなことができる。

``` clojure
(defn optional-arg [pos1 pos2 &optional keyword1 [keyword2 42]]
  [pos1 pos2 keyword1 keyword2])

(optional-arg 'pos1 'pos2 'key1)         ; => ['pos1', 'pos2', 'key1', 42]
(optional-arg 'pos1 'pos2 :keyword2 420) ; => ['pos1', 'pos2', None, 420]
```

&restは可変長の引数を取り、それを関数/マクロの中で1つのリスト(実際にはPythonのタプル型)として扱える。
例えば次のコードは可変長の引数を取ってその総和を取る関数の定義になる。

``` clojure
(defmacro incf [var &optional [diff 1]]
  `(setv ~var (+ ~var ~diff)))

(defn plus [&rest args]
  (let ((sum 0))
    (for [i args] (incf sum i))
    sum))
```

&kwargsは&restのキーワード版みたいなもので、&restがタプルに対応したのに対して、&kwargsはディクショナリ型に対応する。

``` clojure
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
``` clojure
(def x 10)
(setv y 20)
(+ x y) ; => 30
```

PythonはLisp-1なので、HyもLisp-1である。すなわち、関数と変数で名前空間が分かれていない。従って、無名関数を使ってSchemeっぽい関数定義もできる。無名関数はClojureと同様に`fn`で作れる。

``` clojure
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

``` clojure
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

``` clojure
(if (or None False 0 0.0 '() [] {}) 't 'f) ; => 'f'
```
`cond`はClojureと少し違って、角括弧が必要なことに注意。

``` clojure
(let ((i 20))
  (cond [(> i 30) (print "That variable is too big!")]
        [(< i 10) (print "That variable is too small!")]
        [True     (print "That variable is jussssst right!")]))
;; That variable is jussssst right!
```

副作用を目的とした順次実行にはClojureと同様に`do`を使う。これはCommon Lispのprogn、Schemeのbeginに相当する。Common LispとSchemeではdoはループのためのマクロなのでややこしい。

ifでは評価される部分に式が一つしか書けないので、複数の処理を書くためにはdoを使って処理をまとめる必要がある。

``` clojure
(if True
  (do
    (print "this is if true")
    (print "and why not, let's keep talking about how true it is!"))
  (print "this one's still simply just false"))
;; this is if true
;; and why not, let's keep talking about how true it is!
```

上の例でifにelse部分が無い場合は、`when`を使った方が意図が明確になる。(副作用を目的としていることが分かるので)

``` clojure
(when True
  (print "this is if true")
  (print "and why not, let's keep talking about how true it is!"))
;; this is if true
;; and why not, let's keep talking about how true it is!
```

繰り返しには`for`を使う。これはPythonのforそのままで、局所変数とシーケンスの組に続いてループ本体を書く。

``` clojure
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

``` clojure
(require [hy.contrib.loop [loop]])

(defn fact3 [n]
  (loop [[i n] [acc 1]]
        (if (zero? i)
          acc
          (recur (dec i) (* acc i)))))

(fact3 1000) ; => 4023872600770937735437024339 ...
```

## Pythonの呼び出し
パッケージを読み込む`import`はPythonとほとんど同じで、実行時に評価される。複数のimportを一つにまとめられる。
``` python
import sys
import os.path
```
``` clojure
(import sys os.path)
```
次に、PythonとHyでの`from`や`as`の使い方の対応を見る。
``` python
from os.path import exists, isdir as is_dir, isfile as is_file
from sys import *
import numpy as np
```
``` clojure
(import [os.path [exists
                  isdir :as dir?
                  isfile :as file?]]
        [sys [*]]
        [numpy :as np])
```
ただし、マクロは実行時でなくコンパイル時に評価されるので、マクロを含むパッケージはimportの代わりに`require`を使って読み込む。
``` clojure
(require [hy.contrib.loop [*]])    ; loop/recurはここ
(require [hy.extra.anaphoric [*]]) ; アナフォリックマクロのパッケージ
```

次に、NumPyの配列を作って属性にアクセスしてみる。
``` clojure
(def arr (np.array [[1 2 3]
                    [4 5 6]
                    [7 8 9]]))

arr.ndim  ; => 2
arr.size  ; => 9
arr.dtype ; => dtype('int64')
```
メソッド呼び出しは次の2つの書き方が両方通る。
``` clojure
(arr.sum)  ; => 45
(.sum arr) ; => 45
```
ほかにも色々やってみる。
``` clojure
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
