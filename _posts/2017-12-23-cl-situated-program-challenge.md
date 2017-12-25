---
layout: post
title: "Common LispでREST APIを作ってみる"
description: ""
category: 
tags: [lisp]
---
{% include JB/setup %}

* TOC
{:toc}

([Lisp Advent Calendar 2017](https://qiita.com/advent-calendar/2017/lisp){:target="_blank"}参加記事)

## situated-program-challenge

最近、中野を拠点としたClojureのミートアップイベント[clj-nakano](https://clj-nakano.connpass.com/){:target="_blank"}が誕生し、これまでに二回開催されている。

先日自分も参加してきたのだが、その中で主催者の中村さんから[Rich Hickeyの講演内容を紹介する発表](https://gitpitch.com/clj-nakano/effective-programs-ja#/){:target="_blank"}があり、そこでの彼の主張は「継続的に動き続けるプログラムで、現実世界の変化に対応して変化していかなければならないプログラムを書くにあたって、静的型付けだと問題が多い」というものだった。

そこで本当にそう言えるのかを実験するために、[situated-program-challenge](https://github.com/clj-nakano/situated-program-challenge){:target="_blank"}と題してREST APIを実装する課題が提案された。
これはミートアップイベントの管理をするシステムで、実装後にAPIやDBのテーブル構造などに変更が加えられることを想定してバージョン1と2を作り、その変更に要する労力がいかほどかを言語ごとに比較するのが目的だ。

前回のclj-nakanoでは、

- [Clojureによるバージョン1の実装 (iku000888さん)](https://github.com/iku000888/situated-program-challenge/tree/clj-solution){:target="_blank"}
- [Scalaによるバージョン1の実装 (shinichyさん)](https://github.com/shinichy/situated-program-challenge/tree/version1){:target="_blank"}

が紹介された。他の言語でも[situated-program-challengeのレポジトリ](https://github.com/clj-nakano/situated-program-challenge){:target="_blank"}からフォークすることで参加できる。

今回はこれをCommon Lispでやってみる。とりあえずバージョン1のRESTサーバを作るところまでやってみた。

- [Common Lispによるバージョン1の実装 (masatoi)](https://github.com/masatoi/situated-program-challenge/tree/cl-version1){:target="_blank"}

### 使ったライブラリ

- HTTPサーバ: [woo](https://github.com/fukamachi/woo){:target="_blank"}
- Webアプリケーションフレームワーク: [clack](https://github.com/fukamachi/clack){:target="_blank"} + [ningle](https://github.com/fukamachi/ningle){:target="_blank"}
- ORマッパー: [mito](https://github.com/fukamachi/mito){:target="_blank"}
- HTTPクライアント: [dexador](https://github.com/fukamachi/dexador){:target="_blank"}
- JSONライブラリ: [jonathan](https://github.com/Rudolph-Miller/jonathan){:target="_blank"}

### NingleでJSONの受け渡しをするAPIのエントリポイントを作る

NingleはごくシンプルなWebアプリケーションフレームワークで、URLとLisp関数を結び付ける役割を果たす。
まず準備として、ningleアプリケーションのインスタンスを生成し、サーバの起動/停止を行なう関数や、URLとLisp関数の対応付けを行うマクロ`defroute`を定義しておく。

```common_lisp
(defparameter *app* (make-instance 'ningle:<app>))
(defparameter *handler* nil)

;; サーバの起動
(defun start (&key (port 5000))
  (setf *handler*
	(clack:clackup *app*
                       :server :woo
                       :use-default-middlewares nil
                       :port port)))

;; サーバの停止
(defun stop () (clack:stop *handler*))

;; routeへ登録するためのマクロ
(defmacro defroute (name (params &rest route-args) &body body)
  `(setf (ningle:route *app* ,name ,@route-args)
         (lambda (,params)
           (declare (ignorable ,params))
           ,@body)))
```
次に、APIのエントリポイントを定義する。defrouteの第一引数はエントリポイントのURLである。paramsには、URL内の`:member-id`や`:event-id`に対応する値と、HTTPクライアントから渡されるJSONデータをパースしたものが連想リストとして入る。さらに、実際にHTTPクライアントであるdexadorを使ってJSONデータをPOSTメソッドで送信してみる。
```common_lisp
;; エントリポイントの定義
(defroute "/members/:member-id/meetups/:event-id" (params :method :POST)
  (print params)
  '(200 (:content-type "application/json")
    ("{\"HELLO\":10}")))

;; HTTPクライアントでJSONデータをPOST
(dex:post "http://localhost:5000/members/123/meetups/321"
          :content "{\"FIRST_NAME\": \"Satoshi\", \"LAST_NAME\": \"Imai\"}"
          :headers '(("content-type" . "application/json")))

;; paramsの中身を表示
;; (("LAST_NAME" . "Imai") ("FIRST_NAME" . "Satoshi") (:MEMBER-ID . "123") (:EVENT-ID . "321"))

;; "{\"HELLO\":10}" ← 返り値1: HTTPクライアントが受け取るJSONデータ
;; 200              ← 返り値2: HTTPステータス
;; (それ以降の返り値は省略)
```
以下のようなマクロ`with-protect-to-json`を定義しておけば、本体部分で属性リストを返すとJSONに変換してステータス番号と一緒にクライアントに送ってくれる。何かエラーが発生したときにはそのエラーメッセージを送る。こういう毎回似たようなパターンが現われるようなものはマクロとしてくくり出しておくと便利だ。
```common_lisp
(defmacro with-protect-to-json (&body body)
  `(handler-case
       `(200 (:content-type "application/json")
             (,(jojo:to-json (progn ,@body))))
     (error (e)
       `(500 (:content-type "application/json")
             (,(jojo:to-json (list :|error| (format nil "~A" e))))))))

(defun asc (key alist)
  (cdr (assoc key alist :test #'string=)))

(defroute "/numerator/:numer/denominator/:denom" (params :method :GET)
  (with-protect-to-json
    (list :answer (/ (parse-integer (asc :numer params))
                     (parse-integer (asc :denom params))))))

;; 9を3で割った結果を返す
(dex:get "http://localhost:5000/numerator/9/denominator/3")
;; "{\"ANSWER\":3}"
;; 200

;; ゼロ除算
(dex:get "http://localhost:5000/numerator/1/denominator/0")
;; debugger invoked on a DEXADOR.ERROR:HTTP-REQUEST-INTERNAL-SERVER-ERROR in thread
;; #<THREAD "main thread" RUNNING {1001928083}>:
;;   An HTTP request to "http://localhost:5000/numerator/1/denominator/0" returned 500 internal server error.

;; {"error":"arithmetic error DIVISION-BY-ZERO signalled\nOperation was (/ 1 0)."}
```

### ORマッパーMitoでDBへのアクセス
situated-program-challengeではPostgreSQLを使う。Common Lispには昔からPostgreSQL向けのPostmodernというORマッパーがあるが、[Shibuya.lispで以前深町さんがMitoという新しいORマッパーの発表をされていた](https://www.slideshare.net/fukamachi/mito-a-successor-of-integral){:target="_blank"}のを思い出したので使ってみることにした。

MitoはMySQL、PostgreSQL、SQLite3に対応しているのでこれだけでも使う理由になる。

#### DBへの接続設定
```common_lisp
(defun connect-db ()
  (mito:connect-toplevel :postgres :database-name "meetup" :username "meetup" :password "password123"))
```
#### テーブル定義
テーブルはメタオブジェクトプロトコル(MOP)によって拡張されたクラスによって定義する。
けっこう書く量が多かったので、カラムと型の対応を並べるだけでテーブルを定義できるように`deftable`というマクロを定義した。

カラムの型には、`:text`や`:integer`といったデータ型の他に、deftableで定義した他のクラスも指定することができる。あと特に何も指定していなくても`:id`と`:created-at`と`:updated-at`の3つのカラムは自動的に追加され、:idが主キーになる。主キーは指定することもできるが、複合主キーは指定できないようだ。この時点で指定のテーブル構造とは微妙に異なるようになるが気にしないことにする。
```common_lisp
(defmacro deftable (table-name superclass-list &body column-type-pairs)
  `(defclass ,table-name (,@superclass-list)
     ,(mapcar (lambda (col)
		(let* ((col-symbol (if (listp col) (car col) col))
		       (col-name (symbol-name col-symbol))
                       (col-type (if (listp col) (cadr col)))
                       (col-primary (if (find :primary-key col) t nil)))
		  (list col-symbol
			:accessor (intern (concatenate 'string (symbol-name table-name) "-" col-name))
			:initarg (intern col-name :keyword)
                        :col-type col-type
                        :primary-key col-primary)))
       column-type-pairs)
     (:metaclass mito:dao-table-class)))

(deftable groups ()
  (name :text))

(deftable members ()
  (first-name :text)
  (last-name  :text)
  (email      :text))

(deftable groups-members ()
  (group-ref  groups)
  (member-ref members)
  (admin  :boolean))

(deftable meetups ()
  (title    :text)
  (start-at :timestamp)
  (end-at   :timestamp)
  (venue-id :integer)
  (group-id :integer))

(deftable meetups-members ()
  (meetup-ref meetups)
  (member-ref members))

(deftable venues ()
  (name        :text)
  (postal-code :text)
  (prefecture  :text)
  (city        :text)
  (street1     :text)
  (street2     :text)
  (group-id    :integer))
```

#### テーブル生成
```common_lisp
(defparameter *table-list*
  '(groups groups-members meetups meetups-members members venues))

(defun create-all-table ()
  (dolist (table *table-list*)
    (mito:execute-sql (car (mito:table-definition table)))))
```

#### データの取得/登録
テーブルに対応するクラスから生成したインスタンスがデータアクセスオブジェクト(DAO)になる。`mito:find-dao`や`mito:select-dao`でDBからDAOを取得できる。
```common_lisp
;; IDで取得
(mito:find-dao 'members :id 1)
;; #<MEMBERS {100CDE8C53}>

;; 全部を取得
(mito:select-dao 'members)
;; (#<MEMBERS {100C5D7363}> #<MEMBERS {100C5D89D3}> #<MEMBERS {100C5DA043}>)

;; 条件で取得
(mito:select-dao 'members (mito.dao::where (:= :last-name "Yamada")))
;; (#<MEMBERS {100CCBE3F3}> #<MEMBERS {100CCBFA63}>)
```

データを登録するときは、テーブルのクラスのインスタンスを作り、`mito:insert-dao`で登録する。

```common_lisp
(defparameter *new-member*
  (make-instance 'members :first-name "Satoshi"
                          :last-name "Imai"
                          :email "satoshi.imai@gmail.com"))

(mito:insert-dao *new-member*)
```

##### 関係テーブルのDAOから参照しているテーブルのDAOをまとめて取得する
SQLでやるときはJOINでテーブルを結合してからSELECTするのだと思うのだが、Mitoでは関係テーブルのクラスでSELECTするときに参照するクラスを指定することで、参照先のDAOもまとめて取得することができる。

例えば、上で定義したgroups-membersクラスはカラムの型としてgroupsクラスとmembersクラスを指定した。groups-membersクラスに対してselect-daoし、その要素をdescribeしてみるとgroup-refとmember-refスロットは空である。

```common_lisp
(describe (car (mito:select-dao 'groups-members)))
;; #<GROUPS-MEMBERS {100D633873}>
;;   [standard-object]

;; Slots with :INSTANCE allocation:
;;   CREATED-AT                     = @2017-12-25T16:31:23.000000+09:00
;;   UPDATED-AT                     = @2017-12-25T16:31:23.000000+09:00
;;   SYNCED                         = T
;;   ID                             = 1
;;   GROUP-REF                      = #<unbound slot>
;;   GROUP-REF-ID                   = 2
;;   MEMBER-REF                     = #<unbound slot>
;;   MEMBER-REF-ID                  = 1
;;   ADMIN                          = T
```

ここでselect-daoのincludes節で参照するテーブルのクラスを指定してやると、group-refとmember-refスロットにそれぞれのDAOが入っていることが分かる。

```common_lisp
(describe (car (mito:select-dao 'groups-members (includes 'members 'groups))))
;; #<GROUPS-MEMBERS {100DA8D5F3}>
;;   [standard-object]

;; Slots with :INSTANCE allocation:
;;   CREATED-AT                     = @2017-12-25T16:31:23.000000+09:00
;;   UPDATED-AT                     = @2017-12-25T16:31:23.000000+09:00
;;   SYNCED                         = T
;;   ID                             = 1
;;   GROUP-REF                      = #<GROUPS {100DAAB823}>
;;   GROUP-REF-ID                   = 2
;;   MEMBER-REF                     = #<MEMBERS {100DAA1963}>
;;   MEMBER-REF-ID                  = 1
;;   ADMIN                          = T
```

### 例: membersの参照/登録

ここまでの内容から、実際にメンバーを参照/登録するエントリを作ってみるとこうなる。

```common_lisp
(defun members-dao->plist (dao)
  (list :|member-id|  (object-id dao)
        :|first-name| (members-first-name dao)
        :|last-name|  (members-last-name dao)
        :|email|      (members-email dao)))

(defroute "/members" (params :method :get)
  (with-protect-to-json
    (mapcar #'members-dao->plist (select-dao 'members))))

(defroute "/members" (params :method :post)
  (with-protect-to-json
    (let ((dao (make-instance 'members
                              :first-name (asc "first-name" params)
                              :last-name  (asc "last-name" params)
                              :email      (asc "email" params))))
      (insert-dao dao)
      (members-dao->plist dao))))

(cl-json:decode-json-from-string (dex:get "http://localhost:5000/members"))

;; (((:MEMBER-ID . 1) (:FIRST-NAME . "Satoshi") (:LAST-NAME . "Imai")
;;   (:EMAIL . "satoshi.imai@gmail.com"))
;;  ((:MEMBER-ID . 2) (:FIRST-NAME . "Taro") (:LAST-NAME . "Yamada")
;;   (:EMAIL . "taro.yamada@hoge.com"))
;;  ((:MEMBER-ID . 3) (:FIRST-NAME . "Hanako") (:LAST-NAME . "Yamada")
;;   (:EMAIL . "hanako.yamada@hoge.com")))

(defparameter *members4*
  (jojo:to-json '(:|first-name| "Ichiro"
                  :|last-name|  "Suzuki"
                  :|email|      "ichiro.suzuki@fuga.com")))

(cl-json:decode-json-from-string
 (dex:post "http://localhost:5000/members"
           :content *members4*
           :headers '(("content-type" . "application/json"))))

;; ((:MEMBER-ID . 4) (:FIRST-NAME . "Ichiro") (:LAST-NAME . "Suzuki")
;;  (:EMAIL . "ichiro.suzuki@fuga.com"))

(cl-json:decode-json-from-string (dex:get "http://localhost:5000/members"))

;; (((:MEMBER-ID . 1) (:FIRST-NAME . "Satoshi") (:LAST-NAME . "Imai")
;;   (:EMAIL . "satoshi.imai@gmail.com"))
;;  ((:MEMBER-ID . 2) (:FIRST-NAME . "Taro") (:LAST-NAME . "Yamada")
;;   (:EMAIL . "taro.yamada@hoge.com"))
;;  ((:MEMBER-ID . 3) (:FIRST-NAME . "Hanako") (:LAST-NAME . "Yamada")
;;   (:EMAIL . "hanako.yamada@hoge.com"))
;;  ((:MEMBER-ID . 4) (:FIRST-NAME . "Ichiro") (:LAST-NAME . "Suzuki")
;;   (:EMAIL . "ichiro.suzuki@fuga.com")))
```

## まとめ

- Ningleなどを使ってREST APIを作ってみた
- DB操作にはORマッパーMitoを使ってみた
- エントリごとに似たパターンが多いのでマクロでコードサイズをかなり圧縮できる
- クライアントはコマンドラインで使えるようにとのことなので、Rosスクリプトでやろうと思う
