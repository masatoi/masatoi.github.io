---
layout: post
title: "On Lisp読書会(2) 参加メモ"
description: "On Lisp読書会(2) 参加メモ"
category: 
tags: [lisp]
---
{% include JB/setup %}

* TOC
{:toc}

# On Lisp読書会@Shibuya.lisp

## 2章 関数

[本文](https://www.asahi-net.or.jp/~kc7k-nd/onlispjhtml/functions.html)

### 末尾再帰

末尾再帰とは再帰呼び出しから戻ってきた後に何も処理がない再帰のこと。
最近の処理系なら単純なループに変換してくれる。

例として、リストの長さを計算する関数の末尾再帰でないバージョンが出てくる。
`#>`はデバッグプリント用のリーダーマクロで事前にロードしておく。

```lisp
(ql:quickload :cl-debug-print)
(cl-debug-print:use-debug-print)

(defun our-length (lst)
  (if (null #>lst)
      0
      #>(1+ (our-length (cdr lst)))))

(our-length '(1 2 3))

; LST => (1 2 3)
; LST => (2 3)
; LST => (3)
; LST => NIL
; (1+ (OUR-LENGTH (CDR LST))) => 1
; (1+ (OUR-LENGTH (CDR LST))) => 2
; (1+ (OUR-LENGTH (CDR LST))) => 3
```

これを見るとlstが再帰呼び出しの度に縮退していき、空リストに到達してから長さに1ずつ足されていっているのが分かる。

この末尾再帰でないバージョンで100万件のリストの長さをカウントしようとするとstackoverflowになる。

```
(defun our-length (lst)
  (if (null lst)
      0
      (1+ (our-length (cdr lst)))))

(let ((huge-list (loop for i from 1 to 1000000 collect i)))
  (our-length huge-list))

;; Control stack exhausted (no more space for function call frames).
;; This is probably due to heavily nested or infinitely recursive function
;; calls, or a tail call that SBCL cannot or has not optimized away.

;; PROCEED WITH CAUTION.
;;    [Condition of type SB-KERNEL::CONTROL-STACK-EXHAUSTED]
```

次にdisassembleしてみる。
大体の見方は

- `RSI`などのRから始まるのがレジスタ
- `L0`などがラベル
- `CALL`が関数呼び出し
- `JMP`や`JNE`がジャンプで指定したラベル位置にジャンプする
  - `JNE`はJump Not Equalで、直前の`TEST`が失敗したときにジャンプするという条件分岐命令でもある

```lisp
(disassemble 'our-length)
; disassembly for OUR-LENGTH
; Size: 83 bytes. Origin: #x54CE23E3                          ; OUR-LENGTH
; 3E3:       498B4510         MOV RAX, [R13+16]               ; thread.binding-stack-pointer
; 3E7:       488945F8         MOV [RBP-8], RAX
; 3EB:       4881FE17010050   CMP RSI, #x50000117             ; NIL
; 3F2:       7505             JNE L1
; 3F4:       31D2             XOR EDX, EDX
; 3F6: L0:   C9               LEAVE
; 3F7:       F8               CLC
; 3F8:       C3               RET
; 3F9: L1:   8D46F9           LEA EAX, [RSI-7]
; 3FC:       A80F             TEST AL, 15
; 3FE:       7531             JNE L2
; 400:       488B5601         MOV RDX, [RSI+1]
; 404:       4883EC10         SUB RSP, 16
; 408:       B902000000       MOV ECX, 2
; 40D:       48892C24         MOV [RSP], RBP
; 411:       488BEC           MOV RBP, RSP
; 414:       E8C9DA65FB       CALL #x5033FEE2                 ; #<FDEFN OUR-LENGTH> <= 再帰呼び出し
; 419:       480F42E3         CMOVB RSP, RBX
; 41D:       488B75F0         MOV RSI, [RBP-16]
; 421:       BF02000000       MOV EDI, 2
; 426:       E8E5EA11FF       CALL #x53E00F10                 ; SB-VM::GENERIC-+
; 42B:       488B75F0         MOV RSI, [RBP-16]
; 42F:       EBC5             JMP L0
; 431: L2:   CC53             INT3 83                         ; OBJECT-NOT-LIST-ERROR
; 433:       18               BYTE #X18                       ; RSI(d)
; 434:       CC10             INT3 16                         ; Invalid argument count trap
```

42FのJMPと3F6のL0の間でループしており、その途中で再帰呼び出しのCALL命令があるのでループの度に関数が呼ばれていることが分かる。


次に、末尾最適化版の例が出てくる。
アキュームレータaccを導入することで計算の途中結果を関数の引数として次の再帰呼び出し時に渡している。

こちらはlstの縮退とaccへの加算が並行して進んでいることが分かる。

```lisp
(defun our-length-tco (lst)
  (labels ((rec (lst acc)
             #>lst #>acc
             (if (null lst)
                 acc
                 (rec (cdr lst) (1+ acc)))))
    (rec lst 0)))

(our-length-tco '(1 2 3))

; LST => (1 2 3)
; ACC => 0
; LST => (2 3)
; ACC => 1
; LST => (3)
; ACC => 2
; LST => NIL
; ACC => 3
```

これは100万要素のリストに対して呼び出してもstackoverflowエラーにならない。

```lisp
(defun our-length-tco (lst)
  (labels ((rec (lst acc)
             (if (null lst)
                 acc
                 (rec (cdr lst) (1+ acc)))))
    (rec lst 0)))

(let ((huge-list (loop for i from 1 to 1000000 collect i)))
  (our-length-tco huge-list))
; => 1000000
```

disassembleしてみると、4FAのJNEと4D0のL0の間でループになっており、間には加算以外の関数呼び出しはないことが分かる。

```lisp
(disassemble 'our-length-tco)
; disassembly for OUR-LENGTH-TCO
; Size: 84 bytes. Origin: #x54CE24B3                          ; OUR-LENGTH-TCO
; 4B3:       498B4510         MOV RAX, [R13+16]               ; thread.binding-stack-pointer
; 4B7:       488945F8         MOV [RBP-8], RAX
; 4BB:       488B75F0         MOV RSI, [RBP-16]
; 4BF:       4531C0           XOR R8D, R8D
; 4C2:       EB2F             JMP L1
; 4C4:       660F1F440000     NOP
; 4CA:       660F1F440000     NOP
; 4D0: L0:   8D46F9           LEA EAX, [RSI-7]
; 4D3:       A80F             TEST AL, 15
; 4D5:       752B             JNE L2
; 4D7:       488B7601         MOV RSI, [RSI+1]
; 4DB:       488975E8         MOV [RBP-24], RSI
; 4DF:       BF02000000       MOV EDI, 2
; 4E4:       498BD0           MOV RDX, R8
; 4E7:       E824EA11FF       CALL #x53E00F10                 ; SB-VM::GENERIC-+
; 4EC:       4C8BC2           MOV R8, RDX
; 4EF:       488B75E8         MOV RSI, [RBP-24]
; 4F3: L1:   4881FE17010050   CMP RSI, #x50000117             ; NIL
; 4FA:       75D4             JNE L0                          ; 再帰呼び出しがなく、L0へのジャンプ(ループになっている)
; 4FC:       498BD0           MOV RDX, R8
; 4FF:       C9               LEAVE
; 500:       F8               CLC
; 501:       C3               RET
; 502: L2:   CC53             INT3 83                         ; OBJECT-NOT-LIST-ERROR
; 504:       18               BYTE #X18                       ; RSI(d)
; 505:       CC10             INT3 16                         ; Invalid argument count trap
```

SBCLの場合は `(proclaim '(optimize speed))` などはしなくても末尾再帰最適化はやってくれるようだ。
再帰アルゴリズムの中には必ずしも末尾再帰の形にできないものもある。(quicksortとか)

```lisp
(defun quicksort (list)
  (if (null list)
      nil
      (let ((pivot (first list))
            (rest (rest list)))
        (append (quicksort (remove-if-not (lambda (x) (<= x pivot)) rest))
                (list pivot)
                (quicksort (remove-if-not (lambda (x) (> x pivot)) rest))))))
```

ここで最適化オプションの話が出てきており、1からnまでの整数の和を返す関数triangleの例が出てくる。

10億までの総和にすると10倍くらいの違いが出てくる。

```
(defun triangle (n)
  (labels ((tri (c n)
             (declare (optimize (speed 3) (safety 0))
                      (type fixnum n c))
             (if (zerop n)
                 c
                 (tri (the fixnum (+ n c))
                      (the fixnum (- n 1))))))
    (tri 0 n)))

(time (triangle 1000000000))

; Evaluation took:
;  0.252 seconds of real time
;  0.251020 seconds of total run time (0.251020 user, 0.000000 system)
;  99.60% CPU
;  953,885,348 processor cycles
;  0 bytes consed

(defun triangle-no-optimize (n)
  (labels ((tri (c n)
             (if (zerop n)
                 c
                 (tri (+ n c)
                      (- n 1)))))
    (tri 0 n)))

(time (triangle-no-optimize 1000000000))

; Evaluation took:
;  2.948 seconds of real time
;  2.950525 seconds of total run time (2.950434 user, 0.000091 system)
;  100.10% CPU
;  11,212,179,642 processor cycles
;  0 bytes consed
```

### コンパイル

`compile`関数で関数をコンパイルし、`compiled-function-p`で確認するという流れになっているのだが、SBCLの場合は普通に関数定義などを評価しただけでコンパイル済みになっていることが分かる。
clispのインタプリタで明示的にコンパイルする例を示した。

ここでSBCLはJITコンパイルか？という議論になった。
関数毎にインタラクティブに定義できるが、使われるときになってはじめてコンパイルされる(Just In Time)わけでないのでAhead of Timeコンパイルだろうという話になった。

インライン宣言の話にも軽く触れられている。4章のユーティリティ関数のところで実際に使っているのでそのときに紹介することにした。

## 3章 関数的プログラミング

関数的プログラミングとは、副作用を使わず、参照透明にしておくこと。(参照透明: 内部状態を持たず、同じ引数に対して常に返値を返すこと)
このようになっていると関数単位でテスト、デバッグがしやすい。REPL上でのインタラクティブな開発とも相性がよい。

副作用を使う例:

```lisp
;; 悪いポイント: 値を返さない。副作用のみを目的にしている
(defun bad-reverse (lst)
  (declare (optimize speed))
  (let* ((len (length lst))
         (ilimit (truncate (/ len 2))))
    (do ((i 0 (1+ i))
         (j (1- len) (1- j)))
        ((>= i ilimit))
      (rotatef (nth i lst) (nth j lst)))))

(setf lst '(a b c))
(bad-reverse lst)
; => NIL

lst ; => (C B A)
```
rotatefはsetfのように汎変数を受け取って、リスト要素のポインタ操作によって要素の置き換えを実現する。
そのためコンシングは発生しないが、リストは破壊的に変更される。
```lisp
(time (rotatef (car lst) (caddr lst)))
;  0 bytes consed

lst ; => (A B C)
```
