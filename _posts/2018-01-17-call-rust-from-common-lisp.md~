---
layout: post
title: "RoswellのスクリプトでREST APIのクライアントを書いて実行ファイルにするまで"
description: ""
category: 
tags: [lisp]
---
{% include JB/setup %}

* TOC
{:toc}

### rosスクリプト
[前に書いたsituated-program-challengeの問題](https://masatoi.github.io/2017/12/23/cl-situated-program-challenge){:target="_blank"}で、REST APIのクライアントはコマンドラインにするそうなのでrosスクリプトでやってみた。
rosスクリプトはRoswellでインストールされた処理系およびライブラリ環境を使ってスクリプトを書くもので、メモリイメージをダンプすることで実行ファイルにもできる。
rosスクリプトにしておくことでコマンドライン引数の取り扱いなどの処理系ごとの違いを吸収できる。

まずは以下のようにしてひな形を作る。

```
ros init client
```

そうするとカレントディレクトリにclient.rosというファイルができる。その中身は

```common_lisp
#!/bin/sh
#|-*- mode:lisp -*-|#
#|
exec ros -Q -- $0 "$@"
|#
(progn ;;init forms
  (ros:ensure-asdf)
  ;;#+quicklisp (ql:quickload '() :silent t)
  )

(defpackage :ros.script.client.3723612865
  (:use :cl))
(in-package :ros.script.client.3723612865)

(defun main (&rest argv)
  (declare (ignorable argv)))
;;; vim: set ft=lisp lisp:
```

ライブラリを使う場合はquickloadの行を編集する。
rosスクリプトの実行時には、ここで最後に定義したmain関数が呼ばれる。main関数のargvにコマンドライン引数が文字列のリストとして入っている。

### situated-program-challengeで指定されているコマンド

第1引数がURLで、第2引数がHTTPメソッド、それ以降がキーワードオプションになる。例えば、

```
./client http://localhost:5000/groups get
./client http://localhost:5000/groups post group-name=group4 admin-member-ids=1,2,3
./client http://localhost:5000/members/1/groups/1 post admin=true
./client http://localhost:5000/groups/1/venues get
./client http://localhost:5000/members/1 get
```

POSTメソッドではキーワードオプションの部分をJSONに直してHTTPクライアントから送信する必要があるので、キーワードオプションを連想リストにする関数`argstr->assoc`を定義してみた。

```
CLIENT.3723612865> (argstr->assoc "keyword=hoge")
("keyword" . "hoge")
CLIENT.3723612865> (argstr->assoc "keyword=123")
("keyword" . 123)
CLIENT.3723612865> (argstr->assoc "keyword=1,2,3")
("keyword" 1 2 3)
```

まとめると、

```common_lisp
#!/bin/sh
#|-*- mode:lisp -*-|#
#|
exec ros -Q -- $0 "$@"
|#
(progn ;;init forms
  (ros:ensure-asdf)
  #+quicklisp (ql:quickload '(:dexador :cl-json :cl-ppcre) :silent t)
  )

(defpackage :ros.script.client.3723612865
  (:use :cl))
(in-package :ros.script.client.3723612865)

(defun argstr->assoc (str)
  (let* ((pair (ppcre:split "=" str))
         (key (car pair))
         (val (cadr pair))
         (val-list
           (mapcar (lambda (str)
                     (cond ((ppcre:scan "^[0-9]*$" str) (parse-integer str))
                           ((string= str "true") t)
                           ((string= str "false") nil)
                           (t str)))
                   (ppcre:split "," val))))
    (if (= (length val-list) 1)
        (cons key (car val-list))
        (cons key val-list))))

(defun main (&rest argv)
  (assert (>= (length argv) 2))
  (let* ((url (car argv))
         (method (intern (string-upcase (cadr argv))))
         (keyargs (mapcar #'argstr->assoc (cddr argv)))
         (json (if keyargs (cl-json:encode-json-alist-to-string keyargs))))
    (multiple-value-bind (res status)
        (ecase method
          (get (dex:get url))
          (post (dex:post url :content json
                              :headers '(("content-type" . "application/json")))))
      (format *standard-output* "~A~%" res)
      (format *error-output* "~A~%" status))))
```

### 実行ファイル出力

`ros init`した時点で実行可能権限が付いているのでそのまま実行できるが、処理系を起動したりライブラリをロードする時間が若干かかる。

```
$ time ./client.ros http://localhost:5000/members/1 get
{"member-id":1,"first-name":"Satoshi","last-name":"Imai","email":"satoshi.imai@gmail.com"}
200
0.881 secs
```

`ros build`することでロード済みのメモリイメージをダンプでき、clientという実行ファイルができる。

```
ros build client.ros
```

実行ファイルのサイズは16MBくらいになった。これを実行してみると4倍くらい速くなっている。

```
$ time ./client http://localhost:5000/members/1 get
{"member-id":1,"first-name":"Satoshi","last-name":"Imai","email":"satoshi.imai@gmail.com"}
200
0.210 secs
```

[ros buildにはいろいろオプションがあって](https://github.com/roswell/roswell/wiki/Building-images-and-executables){:target="_blank"}、例えばイメージの圧縮を切ったりもできる。

```
ros build client.ros --disable-compression
```

実行ファイルのサイズは65Mくらいになった。これを実行してみるとさらに速くなる。(curlよりは遅いが・・・)

```
$ time ./client http://localhost:5000/members/1 get
{"member-id":1,"first-name":"Satoshi","last-name":"Imai","email":"satoshi.imai@gmail.com"}
200
0.045 secs

$ time curl http://localhost:5000/members/1
{"member-id":1,"first-name":"Satoshi","last-name":"Imai","email":"satoshi.imai@gmail.com"}
0.020 secs
```
rosスクリプト自体はただのテキストファイルなので数KBしかなく、それほどスピードを必要としないならビルドは必要ないと思うが、ビルドするとそのファイル単体で実行できるのでLisp処理系やライブラリをインストールする必要がなく、配布するにはいいと思う。使い分けていこう。
