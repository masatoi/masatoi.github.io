---
layout: post
title: "関数"
description: ""
category: 
tags: [cl-book]
---
{% include JB/setup %}

* TOC
{:toc}

## 関数の定義方法

Common Lispでは、関数を定義する基本的な方法として`defun`が用意されています。
`defun`は関数名とパラメータリスト、そして関数の本体を指定することで関数を定義します。
本体の最後の式の値が返り値となります。
以下は、`defun`を使用した基本的な関数定義の例です。

```lisp
(defun add-two-numbers (a b)
  (+ a b))
```

この関数 `add-two-numbers` は、2つの引数`a`と`b`を受け取り、それらを加算した結果を返します。
定義した関数は以下のように呼び出すことができます。

```lisp
(add-two-numbers 3 5)
; => 8
```

## 無名関数（lambda）

関数を一時的に利用したい場合や、関数に名前を付ける必要がない場合、lambda式を使用して関数オブジェクトを直接作ることができます。

```lisp
(lambda (x) (* x x))
```

このlambda式は、引数`x`を平方する無名関数を作ります。この無名関数をその場で呼び出すこともできます。

```lisp
(funcall (lambda (x) (* x x)) 4)
; => 16
```

無名関数は、関数を引数として受け取る高階関数（例: `mapcar`や`reduce`）と組み合わせる際にも便利です。

```lisp
(mapcar (lambda (x) (* x x)) '(1 2 3 4))
; => (1 4 9 16)
```

## 関数定義の柔軟性

Common Lispの関数定義には以下のような柔軟な特徴があります。

- パラメータリストの柔軟性: パラメータリストには、必須引数だけでなく、デフォルト値を持つオプショナル引数や任意の数の引数を受け取るための機構（`optional`, `rest`）が用意されています（詳細は2.2で解説）
- 再定義可能性: Common Lispでは、既存の関数を動的に再定義することが可能です。これにより、システム全体を再ロードすることなく、特定の関数の振る舞いのみを即座に変更できます

```lisp
(defun square (n) (* n n))
(defun cube (n) (* n (square n)))

;; squareを最適化されたバージョンで再定義
(defun square (n)
  (declare (type fixnum n)
           (optimize (speed 3) (safety 0)))
  (* n n))

;; cubeはそのまま使える
(cube 3) ; => 27
```

- 独自の名前空間の利用: 関数は関数専用の名前空間に属しており、シンボルの名前空間とは区別されています。そのため、関数名と同じ名前を持つ変数を定義しても衝突することはありません
  - 関数名のシンボルから関数の実体を参照するには `symbol-function` を用います。これと`setf`を組み合わせることで`defun`を使わずに関数を定義することもできます
  - 通常の名前空間に存在する関数オブジェクトを呼び出すには`funcall`を使います。

```lisp
;; 関数と変数では名前空間が分かれている
(defun my-function () "This is a function")
(setf my-function "This is a variable")
(my-function) ; => "This is a function"
my-function   ; => "This is a variable"

;; 関数の名前空間に関数オブジェクトを設定することで関数定義と同じことができる
(setf (symbol-function 'square) (lambda (x) (* x x)))
(square 3) ; => 9

;; 変数の名前空間に関数オブジェクトを設定するとfuncallが必要
(setf square-as-variable (lambda (x) (* x x)))
(funcall square-as-variable 3) ; => 9
```

## 局所関数

局所関数は、特定のスコープ内でのみ利用可能な関数です。Common Lispでは `flet` や `labels` で局所関数を定義します。
これにより、プログラムの構造化、名前の衝突回避、コードの可読性向上が可能です。

### flet: シンプルな局所関数の定義

`flet`は、スコープ内で一時的に利用する関数を定義します。
定義された関数は再帰呼び出しをサポートしません。

```lisp
(defun calculate-sum (numbers)
  (flet ((square (x) (* x x)))
    (mapcar #'square numbers)))
```

この例では、局所関数`square`を定義し、`numbers`の各要素を平方する処理を実現しています。

### labels: 再帰や相互再帰をサポートする局所関数の定義

`labels`は再帰や相互再帰を必要とする局所関数を定義する場合に使用します。

例: 再帰を用いた階乗計算
```lisp
(defun calculate-factorial (n)
  (labels ((factorial (x acc)
             (if (zerop x)
                 acc
                 (factorial (1- x) (* x acc))))) ; 再帰呼び出し
    (factorial n 1)))
```

この例では、局所関数`factorial`が再帰的に自身を呼び出して階乗を計算しています。

`flet` は再帰呼び出しをサポートしませんが、`labels` は再帰および相互再帰をサポートします。また、`labels`では複数の関数を並べて定義したときに先に定義したものを後の定義から使えます。

例: 相互再帰を利用した偶数・奇数の判定
```lisp
(defun is-even (n)
  (labels ((evenp (x) (if (zerop x) t (oddp (1- x))))
           (oddp (x) (if (zerop x) nil (evenp (1- x)))))
    (evenp n)))
```

再帰の有無で使い分けることで意図を明確化できます。

局所関数は、プログラムのスコープを制御し、命名の衝突を回避するために非常に有効な手段です。
`flet`と`labels`を適切に使い分けることで、コードの構造化と可読性を向上させることができます。これらを活用し、簡潔でメンテナンスしやすいコードを目指しましょう。


## パラメータリスト

Common Lispの関数定義には、さまざまなパラメータ指定方法があります。
これにより、より柔軟な関数のインターフェースを設計することが可能です。

### 必須パラメータ

必須パラメータは、関数が呼び出される際に常に指定される必要がある引数です。
`defun`で単純にパラメータを列挙することで定義します。

```lisp
(defun add (a b)
  (+ a b))

(add 3 5) ; => 8
```

### オプショナルパラメータ

オプショナルパラメータは、引数が指定されない場合にデフォルト値を使用する引数です。
`&optional`を使って定義します。

```lisp
(defun greet (name &optional (greeting "Hello"))
  (format t "~A, ~A!~%" greeting name))

(greet "Alice")      ; => Hello, Alice!
(greet "Alice" "Hi") ; => Hi, Alice!
```

### キーワードパラメータ

キーワードパラメータは、引数を名前付きで指定できる引数です。
`&key`を使って定義します。

```lisp
(defun configure (width &key (height 100) (color "blue"))
  (format t "Width: ~A, Height: ~A, Color: ~A~%" width height color))

(configure 200 :height 150 :color "red")
; => Width: 200, Height: 150, Color: red

(configure 200)
; => Width: 200, Height: 100, Color: blue
```

### restパラメータ (可変長引数)

restパラメータは、任意の数の引数をリストとして受け取る引数です。
`&rest`を使って定義します。

```lisp
(defun sum (&rest numbers)
  (reduce #'+ numbers))

(sum 1 2 3 4) ; => 10
(sum) ; => 0
```

これらのパラメータリストの組み合わせにより、関数の引数設計は柔軟かつ強力になります。
次節では、それぞれのパラメータタイプを活用した実践例を紹介します。
