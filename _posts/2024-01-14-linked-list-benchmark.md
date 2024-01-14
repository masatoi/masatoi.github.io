---
layout: post
title: "SBCLのリストのランダムアクセスが速すぎる件"
description: "SBCLのリストのランダムアクセスが速すぎる件"
category: 
tags: [lisp]
---
{% include JB/setup %}

* TOC
{:toc}

# データ構造をどう選択するかがスピード上重要になるという実験

連結リストに対するランダムアクセスと配列に対するランダムアクセスで実際にどの程度差が出るのものを確認する。

- SBCL 2.4.0
- AMD Ryzen 7 5800X 8-Core Processor
- Linux prime 5.15.0-91-generic #101-Ubuntu SMP x86_64 GNU/Linux

100万件のデータのランダムな位置に1億回アクセスする時間を測る。
連結リストであればシーケンシャルアクセスは速いがランダムアクセスは遅いというのが期待値。

## 連結リスト

```lisp
;;; linked-list

(defparameter lst (loop for i from 1 to 1000000 collect i))

(time
 (loop repeat 100000000
       do (nth (random 1000000) lst)))

;; Evaluation took:
;;   0.687 seconds of real time
;;   0.684590 seconds of total run time (0.683457 user, 0.001133 system)
;;   99.71% CPU
;;   2,621,812,964 processor cycles
;;   0 bytes consed
```

## 連結リスト(最適化宣言付き)

```lisp
(time
  (locally
      (declare (optimize (speed 3) (safety 0)))
    (loop repeat 100000000
          do (nth (random 1000000) lst))))

;; Evaluation took:
;;   0.328 seconds of real time
;;   0.328652 seconds of total run time (0.328427 user, 0.000225 system)
;;   100.30% CPU
;;   1,248,868,784 processor cycles
;;   0 bytes consed
```

### 型指定無しの固定長配列

```lisp
;;; non typed array

(defparameter arr-non-typed (make-array 1000000))
(loop for i from 0 below 1000000
      do (setf (aref arr-non-typed i) (1+ i)))

(time
 (loop repeat 100000000
       do (aref arr-non-typed (random 1000000))))

;; Evaluation took:
;;   1.484 seconds of real time
;;   1.485203 seconds of total run time (1.485024 user, 0.000179 system)
;;   100.07% CPU
;;   5,643,859,712 processor cycles
;;   0 bytes consed
```

### 型指定無しの固定長配列(最適化宣言付き)

```lisp
(time
 (locally
     (declare (optimize (speed 3) (safety 0)))
   (loop repeat 100000000
       do (aref arr-non-typed (random 1000000)))))

;; Evaluation took:
;;   0.332 seconds of real time
;;   0.333449 seconds of total run time (0.333405 user, 0.000044 system)
;;   100.30% CPU
;;   1,267,148,836 processor cycles
;;   0 bytes consed
```

### 型指定有りの固定長配列

```lisp
;;; typed array

(defparameter arr-typed (make-array 1000000 :element-type 'fixnum))
(loop for i from 0 below 1000000
      do (setf (aref arr-typed i) (1+ i)))

(time
 (loop repeat 100000000
       do (aref arr-typed (random 1000000))))

;; Evaluation took:
;;   1.564 seconds of real time
;;   1.563470 seconds of total run time (1.563470 user, 0.000000 system)
;;   99.94% CPU
;;   5,941,252,386 processor cycles
;;   0 bytes consed
```

### 型指定有りの固定長配列(最適化宣言付き)

```lisp
(time
 (locally
     (declare (optimize (speed 3) (safety 0)))
   (loop repeat 100000000
         do (aref arr-typed (random 1000000)))))

;; Evaluation took:
;;   0.328 seconds of real time
;;   0.329280 seconds of total run time (0.329280 user, 0.000000 system)
;;   100.30% CPU
;;   1,251,274,640 processor cycles
;;   0 bytes consed
```

実際には、固定長配列よりもリストの方が速いという結果になっている・・・！
最適化宣言付けると同じくらいになっている。

# 他の処理系では？

## CLISP (GNU CLISP 2.49.93+)
CLISPだと予想通りになっている。
リストのランダムアクセスは遅すぎて1億回もアクセスしていたら終わらないため1000回にしてある。

```
CL-USER> (defparameter lst (loop for i from 1 to 1000000 collect i))
LST

CL-USER> (time
 (loop repeat 1000
       do (nth (random 1000000) lst)))
Real time: 0.726303 sec.
Run time: 0.726299 sec.
Space: 9208 Bytes
NIL

CL-USER> (defparameter arr-typed (make-array 1000000 :element-type 'fixnum))
ARR-TYPED

CL-USER> (loop for i from 0 below 1000000
      do (setf (aref arr-typed i) (1+ i)))
NIL

CL-USER> (time
 (locally
     (declare (optimize (speed 3) (safety 0)))
   (loop repeat 1000
         do (aref arr-typed (random 1000000)))))
Real time: 0.001569 sec.
Run time: 0.001568 sec.
Space: 9264 Bytes
NIL
```

## Clozure CL (Version 1.12.2 (v1.12.2) LinuxX8664)
Clozure CLでもCLISPと同様の結果になった。

```
CL-USER> (time
 (loop repeat 1000
       do (nth (random 1000000) lst)))

(LOOP REPEAT 1000 DO (NTH (RANDOM 1000000) LST))
took 1,502,529 microseconds (1.502529 seconds) to run.
During that period, and with 16 available CPU cores,
     1,502,684 microseconds (1.502684 seconds) were spent in user mode
             0 microseconds (0.000000 seconds) were spent in system mode

CL-USER> (time
 (locally
     (declare (optimize (speed 3) (safety 0)))
   (loop repeat 1000
         do (aref arr-typed (random 1000000)))))

(LOCALLY (DECLARE (OPTIMIZE (SPEED 3) (SAFETY 0))) (LOOP REPEAT 1000 DO (AREF ARR-TYPED (RANDOM 1000000))))
took 19 microseconds (0.000019 seconds) to run.
During that period, and with 16 available CPU cores,
     25 microseconds (0.000025 seconds) were spent in user mode
      0 microseconds (0.000000 seconds) were spent in system mode
```

なのでSBCLが何か特殊なことをやっているのだと予想される。内部的には単純な連結リストではない？
SBCLのリスト処理は常々速すぎると思っていたが・・・

# ChatGPT4のコメント:
```
Common Lispで行ったこの実験の結果に関して、リストが配列よりも高速に動作するように見える理由について考察します。

キャッシュ効果: 実験で使用されたリストのサイズが小さかったり、アクセスパターンがキャッシュに最適化されていた場合、リストの方が高速に動作する可能性があります。しかし、通常の状況下では、連結リストは配列に比べてランダムアクセスにおいて遅いはずです。これは連結リストがメモリ内で非連続的に配置されるため、キャッシュミスが発生しやすいからです。

オプティマイザの影響: Common Lispのコンパイラは非常に強力なオプティマイザを持っています。あなたのコードにおいて (declare (optimize (speed 3) (safety 0))) が使用されており、これはコンパイラに最高レベルの最適化を行うよう指示しています。この最適化により、リストへのアクセスが予想以上に高速化されることがあります。

実装の詳細: Common Lispの実装によっては、リストや配列の内部表現が異なり、特定の操作に最適化されている場合があります。そのため、理論的な性能と実際の性能が異なることがあります。

測定方法: time マクロを使用した測定では、実行環境によって結果が大きく異なる可能性があります。また、測定時のシステムの状態（他のプロセスの動作など）によっても結果は変わります。

配列の種類: 配列が非型指定（arr-non-typed）と型指定（arr-typed）であることも考慮する必要があります。型指定配列は一般にメモリ使用量が少なく、アクセス速度が高速ですが、この実験では大きな差は見られませんでした。

これらの要因を考慮すると、リストが配列よりも高速に見えるのは、特定の条件下での測定結果であり、一般的なケースではない可能性が高いです。データ構造の選択は、常にアプリケーションの要件や動作する環境に応じて慎重に行う必要があります。
```
