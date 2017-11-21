---
layout: post
title: "型宣言付きのdefunとletを定義してみる"
description: ""
category: 
tags: [lisp]
---
{% include JB/setup %}

Common Lispは型宣言をオプショナルで付けることができるので必要なら速いコードを書くこともできるが、型宣言を付けて書くとコードがごちゃごちゃしてタイプ量が増えるし、間違いの元にもなる。Juliaっぽく型指定できないだろうか？

### tak関数のベンチマーク

- [M.Hiroi's Home Page / Julia Language Programming](http://www.geocities.jp/m_hiroi/light/julia.html){:target="_blank"}

Juliaでtak関数を定義してみると、

```julia
function tak(x::Int, y::Int, z::Int)
  if x <= y
    z
  else
    tak(tak(x - 1, y, z), tak(y - 1, z, x), tak(z - 1, x, y))
  end
end
```

```
julia> @time(tak(22,11,0))
  1.607021 seconds (636 allocations: 38.822 KiB)
11
```
となってけっこう速い。これは素のSBCLよりも速い。

```common_lisp
(defun tak (x y z)
  (if (<= x y)
      z
      (tak (tak (1- x) y z)
           (tak (1- y) z x)
           (tak (1- z) x y))))
```

```
CL-USER> (time (tak 22 11 0))
Evaluation took:
  4.704 seconds of real time
  4.704000 seconds of total run time (4.704000 user, 0.000000 system)
  100.00% CPU
  15,958,045,410 processor cycles
  0 bytes consed
  
11
```

これに最適化宣言をつけると、SBCLはJuliaよりも速くなる。

```common_lisp
;; ゴチャァ…
(locally (declare (ftype (function (fixnum fixnum fixnum) fixnum) tak))
  (defun tak (x y z)
    (declare (optimize (speed 3) (safety 0) (debug 0))
             (type fixnum x y z))
    (if (<= x y)
        z
        (tak (tak (1- x) y z)
             (tak (1- y) z x)
             (tak (1- z) x y)))))
```

```
CL-USER> (time (tak 22 11 0))
Evaluation took:
  1.313 seconds of real time
  1.312000 seconds of total run time (1.312000 user, 0.000000 system)
  99.92% CPU
  4,453,962,162 processor cycles
  0 bytes consed
  
11
```

### 型付きのdefun

速くはなったものの、declareが増えてごちゃごちゃしている。その点Juliaは仮引数に::Intなどを付けるだけなのですっきりしている。これを真似して型付きの関数定義`defnt`を定義する。

```common_lisp
(defmacro defnt (function-spec (&rest arg-specs) &body body)
  `(locally
       (declare (ftype (function ,(mapcar #'cadr arg-specs) ,(cadr function-spec))
                       ,(car function-spec)))
     (defun ,(car function-spec) ,(mapcar #'car arg-specs)
       (declare (optimize (speed 3) (safety 0) (debug 0))
                ,@(mapcar (lambda (arg arg-type)
                            (list 'type arg-type arg))
                          (mapcar #'car arg-specs)
                          (mapcar #'cadr arg-specs)))
       ,@body)))

;; スッキリ！！
;; 関数の返り値の型も指定する
(defnt (tak fixnum) ((x fixnum) (y fixnum) (z fixnum))
  (if (<= x y)
      z
      (tak (tak (1- x) y z)
           (tak (1- y) z x)
           (tak (1- z) x y))))

;; 多値を返す場合
(defnt (mulval (values fixnum double-float)) ((x fixnum) (y double-float))
  (values (floor y) (* x 1.0d0)))
```

これで多少すっきりした。

### 型付きのlet

実行時の型変換を起こさないためには、関数の仮引数だけではなくて、局所変数を束縛するのにも型宣言が必要になる。ということで型付きのlet、`tlet`を定義する。

```common_lisp
(defmacro tlet (bindings &body body)
  `(let (,@(mapcar (lambda (binding)
                     (subseq binding 0 2))
                   bindings))
     (declare ,@(mapcar (lambda (binding)
                          (list 'type (caddr binding) (car binding)))
                        bindings))
     ,@body))
     
(tlet ((x 1 fixnum)
       (y (+ 2 2) fixnum))
  (+ x y))
; => 5
```

### 実例: double-floatのベクトルの内積

```common_lisp
(defun make-dvec (input-dimension initial-element)
  (make-array input-dimension :element-type 'double-float :initial-element initial-element))

(defmacro dovec (vec var &body body)
  `(loop for ,var fixnum from 0 to (1- (length ,vec)) do ,@body))


;; 通常の書き方
(declaim (ftype (function ((simple-array double-float) (simple-array double-float))
                          double-float)
                dot))
(defun dot (x y)
  (declare (type (simple-array double-float) x y)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let ((result 0.0d0))
    (declare (type double-float result))
    (dovec x i (incf result (* (aref x i) (aref y i))))
    result))

;; defntとtletを使ったバージョン
(defnt (dot double-float) ((x (simple-array double-float))
                           (y (simple-array double-float)))
  (tlet ((result 0d0 double-float))
    (dovec x i (incf result (* (aref x i) (aref y i))))
    result))
```

```
CL-USER> (defparameter dvec1 (make-dvec 10 1d0))
DVEC1
CL-USER> (defparameter dvec2 (make-dvec 10 2d0))
DVEC2
CL-USER> (dot dvec1 dvec2)
20.0d0
```
