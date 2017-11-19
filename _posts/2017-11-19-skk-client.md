---
layout: post
title: "メモ: Common LispでSKKサーバのクライアントを書く"
description: ""
category: 
tags: [lisp]
---
{% include JB/setup %}

lemでskkを使いたいという密かな野望があるので、そのうちにマイナーモードを作るかもしれない。
SKKサーバとの通信プロトコルはとても簡単なので、ソケットプログラミングの練習にはよさそうに見える。

### SKKサーバのプロトコル

- [これは今使ってるSKKサーバの一つdbskkd-cdbのプロトコル](https://github.com/jj1bdx/dbskkd-cdb/blob/master/skk-server-protocol.md){:target="_blank"}
- [これはPySocialSKKServのページにあったプロトコル](https://ja.osdn.net/projects/pysocialskkserv/wiki/SKKServ){:target="_blank"}

telnetからSKKサーバにつないで実験することもできる。ただしSKKサーバの文字コードはEUC-JPなので、ターミナルを設定しておかないと文字化けする。
```
% telnet localhost skkserv
1a  # aの後にスペース
1/α/エー/エイ/アー/а/ア/
```

### Common Lispからソケットを叩くには
Common Lispからポータブルにソケットを叩くライブラリとしてusocketがある。加えて文字コードの変換にbabel、タイムアウトのためにportable-threadsをロードしておく。([逆引きCommon Lisp: 操作をタイムアウトにする](http://tips.cddddr.org/common-lisp/index.cgi?%e6%93%8d%e4%bd%9c%e3%82%92%e3%82%bf%e3%82%a4%e3%83%a0%e3%82%a2%e3%82%a6%e3%83%88%e3%81%ab%e3%81%99%e3%82%8b){:target="_blank"})

```common_lisp
(ql:quickload :usocket)
(ql:quickload :babel)
(ql:quickload :portable-threads)

(defvar *skk-server-portnum* 1178)
(defvar *skk-server-host* "localhost")

;; ソケットを開く
(defparameter sock
  (usocket:socket-connect *skk-server-host* *skk-server-portnum*
                          :element-type '(unsigned-byte 8)))
                          
;; ソケットを閉じる
;; (usocket:socket-close sock)
```

SKKサーバの文字コードがEUC-JPなので、ソケットのストリームからそのままreadなどで読み込むことはできない。バイト列として読み書きするために、`:element-type`キーワードでオクテットを指定して`socket-connect`を呼び出している。


読み書きにも注意が必要で、書き出しのときは`babel:string-to-octets`でEUC-JPエンコーディングした文字列のバイト列を1文字ずつ`write-byte`で書き出す。
読み出し時は改行コードが来るまで`read-byte`を繰り返し、得られたEUC-JPのバイト列を`babel:octets-to-string`で処理系で扱える文字列に変換する。SKKサーバはバージョン問合せのときなど改行コードを付けないものを返してくることもあるので、readが待ち状態になってしまって処理が止まってしまったときのためにタイムアウト処理を用意しておく。

```common_lisp
(defun write-skkserv (socket string)
  (loop for byte across (babel:string-to-octets string :encoding :eucjp) do
    (write-byte byte (usocket:socket-stream socket)))
  (force-output (usocket:socket-stream socket)))

(defun read-skkserv (socket &key (timeout-seconds 1))
  (let ((len 0)
        (product nil))
    (flet ((make-res-string (len product)
             (babel:octets-to-string
              (make-array len :element-type '(unsigned-byte 8) :initial-contents (nreverse product))
              :encoding :eucjp)))
      (portable-threads:with-timeout
          (timeout-seconds
            (error "Timeout in read-skkserv. Read data: ~A~%" (make-res-string len product)))
        (let ((byte (read-byte (usocket:socket-stream socket) nil nil)))
          (loop until (or (null byte) (= byte 10)) do
            (push byte product)
            (incf len)
            (setf byte (read-byte (usocket:socket-stream socket) nil nil)))
          (make-res-string len product))))))
```

### REPLでSKKサーバと会話してみる。

```
CL-USER> (write-skkserv sock "1きm ")
NIL
CL-USER> (read-skkserv sock)
"1/決/来/極;NB:「きわm」と同形/着/気/黄/决;「決」の異体字/來;「来」の旧字(人名用漢字)/徠;「来」の異体字/氣;「気」の旧字(人名用漢字)/著;<rare> ≒着る/"
CL-USER> (write-skkserv sock "1きn ")
NIL
CL-USER> (read-skkserv sock)
"1/気/来/着/機/氣;「気」の旧字(人名用漢字)/著;<rare> ≒着る/來;「来」の旧字(人名用漢字)/徠;「来」の異体字/切/斬;人を斬る/伐;木を伐る/剪;盆栽を剪る/截;布地を截る/鑽;<rare> 火を鑽る(=火打ち石で火を起こす)/"
CL-USER> (write-skkserv sock "1きき ")
NIL
CL-USER> (read-skkserv sock)
"1/機器/危機/鬼気/嬉々/器機;=機器/記紀;古事記・日本書紀/毀棄/忌諱/嬉嬉/喜々/気気/輝輝/効き/窺基/奇奇/帰期/飢鬼/喜気/汽機/起期/騏驥/奇気/記旗/諱忌/奇鰭/暉暉/"
CL-USER> (write-skkserv sock "1ほげ ")
NIL
CL-USER> (read-skkserv sock)
"4ほげ"
```
