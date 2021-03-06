---
layout: post
title: "メモ: Common LispからRust/Goを呼び出す"
description: ""
category: 
tags: [lisp]
---
{% include JB/setup %}

RustはCの共有ライブラリを出力できるそうなのでCommon LispのCFFIから呼べるかどうか試してみた。

参考:
- [RubyからRustの関数をつかう → はやい](https://qiita.com/rejasupotaro/items/2aa57a67f4a97101950c){:target="_blank"}
- [rustでCで書いた関数を呼ぶ / Cからrustで書いた関数を呼ぶ](http://mmi.hatenablog.com/entry/2017/02/28/213656){:target="_blank"}

### Rustで共有ライブラリをつくる

以下のような内容の`fib.rs`ファイルをつくる。
```rust
#[no_mangle]
pub extern fn fib(n: u32) -> u32 {
    if n <= 1 {
        n
    } else {
        fib(n - 1) + fib(n - 2)
    }
}
```

次のようにビルドすると、`libfib.so`ファイルができる。
```
rustc --crate-type="dylib" fib.rs
```

### Common Lisp側から呼び出す

```common_lisp
(ql:quickload :cffi)

;; さきほどビルドした共有ライブラリを読み込む
(cffi:load-foreign-library "/home/wiz/program/rust/libfib.so")

;; ラッパー関数を定義
(cffi:defcfun "fib" :int (n :int))

(time (fib 40))
;; Evaluation took:
;;   1.189 seconds of real time
;;   1.192000 seconds of total run time (1.192000 user, 0.000000 system)
;;   100.25% CPU
;;   4,033,898,326 processor cycles
;;   0 bytes consed

;; => 102334155
```
普通にできた！！


なおこの場合Common Lispで書いた方が速い
- 使用マクロはここから: [型宣言付きのdefunとletを定義してみる](https://masatoi.github.io/2017/11/21/typed-defun){:target="_blank"}

```common_lisp
(defnt (fib2 fixnum) ((n fixnum))
  (if (<= n 1)
      n
      (+ (fib2 (- n 1))
         (fib2 (- n 2)))))

(time (fib2 40))
;; Evaluation took:
;;   0.787 seconds of real time
;;   0.788000 seconds of total run time (0.788000 user, 0.000000 system)
;;   100.13% CPU
;;   2,671,326,658 processor cycles
;;   0 bytes consed
  
;; => 102334155
```

### 追記: Rustの最適化オプションを有効化してみる

Rustにも最適化オプションがあるらしい

- [君のRustは20倍遅い - 簡潔なQ](http://qnighy.hatenablog.com/entry/2017/05/02/070000){:target="_blank"}

```
rustc --crate-type="dylib" -C opt-level=3 fib.rs
```

として上と同様の手順を踏んでみると、最適化ありのCommon Lisp版より速くなった。
```common_lisp
(time (fib 40))
;; Evaluation took:
;;   0.708 seconds of real time
;;   0.712000 seconds of total run time (0.712000 user, 0.000000 system)
;;   100.56% CPU
;;   2,404,129,682 processor cycles
;;   0 bytes consed
```

### 追記2: Goでもやってみる

GoにもCの共有ライブラリを出力できるビルドオプションがあるらしい

- [Golang で Shared Library を出力する。](https://qiita.com/yanolab/items/1e0dd7fd27f19f697285){:target="_blank"}

`libgofib.go`
```golang
package main

import (
    "C"
    "log"
)

//export fib
func fib(n int) int {
    if (n < 2) { return n }
    return fib(n - 2) + fib(n - 1)
}

func init() {
    log.Println("Loaded!!")
}

func main() {
}
```
これを以下のようにビルドする。
```
go build -buildmode=c-shared -o libgofib.so libgofib.go
```
Common Lispから呼び出す。
```common_lisp
(cffi:load-foreign-library "/home/wiz/program/golang/libgofib.so")
(cffi:defcfun "fib" :int (n :int))
(time (fib 40))

;; Evaluation took:
;;   0.733 seconds of real time
;;   0.736000 seconds of total run time (0.736000 user, 0.000000 system)
;;   100.41% CPU
;;   2,485,210,416 processor cycles
;;   0 bytes consed
```

### 感想
簡単なC-APIを書けるだけのRust/Go力があればそのコード資産に簡単にアクセスできるというのは非常にうれしい。
Common Lispはそれ自体がかなり速いためにスピードのために外部ライブラリを呼ぶ必要性はあまりないのだが、ユーザ人口が少ないのでライブラリの物量不足になっており、アクセス可能なコード資産が増えるのはいいことである。
反面、外部ライブラリをCommon Lispのプロジェクトに組込むときはビルド時のエラーや依存関係など余計に考えなければならないことが増えるのでそういったところでの苦労が予想される。
