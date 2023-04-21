---
layout: post
title: "Common Lisp(SBCL)で素数判定の例で計測してみる"
description: ""
category: 
tags: [lisp]
---
{% include JB/setup %}

# Common Lisp(SBCL)で素数判定の例で計測してみる

この辺の記事を見かけてCommon Lispだとどうなるのか調べてみた。

- [プログラミング言語の実行速度比較（2023/4）](https://transparent-to-radiation.blogspot.com/2023/04/20234.html)
- [Scalaのメモリ使用量はJavaよりも多いか検証した](https://blog.3qe.us/entry/2023/04/21/041246)

できたコードは以下になる。それまでに素数判定した数をリストに記録していき、新たな数が記録した素数リスト内のいずれの数でも割り切れなければ新たな素数としてリストに追加する。

判定の際に素数リストの先頭から舐めていくので、新しい素数はリストの末尾に追加しなければならない。
Common Lispにはリストの先頭に破壊的に要素を追加する `push` はあるが、末尾に追加する組込みの関数などはない。
最初 `alexandria:appendf` とかを使って末尾に追加していたが、Nを増やしていくと非常に遅くなるので、リスト末尾のconsセルへの参照をリスト自体への参照とセットで持つような構造体を定義した。

リストの末尾に要素を追加するときは構造体に保存されているリスト末尾のcdrに新規に作ったconsセルへの参照を入れる。


```lisp
(defstruct list-with-tail
  (head nil)
  (tail nil))

(defun append-element (list-with-tail element)
  (declare (optimize (speed 3) (space 0) (safety 0)))
  (let ((new-cons (cons element nil)))
    (if (null (list-with-tail-head list-with-tail))
        (setf (list-with-tail-head list-with-tail) new-cons)
        (setf (cdr (list-with-tail-tail list-with-tail)) new-cons))
    (setf (list-with-tail-tail list-with-tail) new-cons)))

(defun %make-list-with-tail (&optional list)
  (if list
      (make-list-with-tail :head list :tail (last list))
      (make-list-with-tail)))

(defparameter *prime-numbers* (%make-list-with-tail))

(defun prime-number-p (n)
  (declare (optimize (speed 3) (space 0) (safety 0))
           (type fixnum n))
  (loop for pn fixnum in (list-with-tail-head *prime-numbers*) do
    (cond ((> (the fixnum (* pn pn)) n) (return))
          ((zerop (mod n pn)) (return-from prime-number-p nil))))
  t)

(defun calc (upper-limit)
  (declare (optimize (speed 3) (space 0) (safety 0))
           (type fixnum upper-limit))
  (loop for n fixnum from 2 to upper-limit do
    (when (prime-number-p n)
      (append-element *prime-numbers* n))))
```

これをSBCL 2.2.9, Linux(Ubuntu 22.04LTS), AMD Ryzen 7 5800Xで実行してみる。
最適化宣言ありで 9.491 seconds, 宣言なしで 40.855 seconds という結果になった。

```
;; with optimization
CL-USER> (time (calc 100000000))
Evaluation took:
  9.491 seconds of real time
  9.492471 seconds of total run time (9.492470 user, 0.000001 system)
  100.01% CPU
  36,071,756,272 processor cycles
  92,182,656 bytes consed

;; without optimization
CL-USER> (time (calc 100000000))
Evaluation took:
  40.855 seconds of real time
  40.854827 seconds of total run time (40.834829 user, 0.019998 system)
  [ Run times consist of 0.061 seconds GC time, and 40.794 seconds non-GC time. ]
  100.00% CPU
  155,249,277,362 processor cycles
  92,227,312 bytes consed
```

比較のためにリンク先のコードでCとPythonの場合で測ってみたらそれぞれ 0m7.996s, 4m35.535s だった。
さすがにCよりは若干遅いがコード量は格段に短い。

メモリ量は実行前後で87.9MBほど消費しており、Cの25MBに比べると3~4倍多い。やはり単なる配列よりは連結リストの方がメモリは必要。これに加えてランタイムのメモリ消費もある。

なお処理系起動やコマンドライン引数のパースの時間が入ってないので不公平なのではと思って、Roswellスクリプトとして実行してみたが、 `--disable-compression` オプションを付けてビルドして実行ファイルを作ると起動時間は誤差レベルだった。

```
$ ros build pn.ros --disable-compression
WARNING: :SB-EVAL is no longer present in *FEATURES*

$ time ./pn 100000000
Count: 5761455
9.585 secs
```
