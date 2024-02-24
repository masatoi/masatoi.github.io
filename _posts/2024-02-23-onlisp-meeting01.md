---
layout: post
title: "On Lisp読書会(1) 参加メモ"
description: "On Lisp読書会(1) 参加メモ"
category: 
tags: [lisp]
---
{% include JB/setup %}

* TOC
{:toc}

# On Lisp読書会@Shibuya.lisp

Shiubya.lispで、最近RustでSci-Lispという独自処理系を作られているchaploudさんからご提案があり、週一でOn Lispの読書会に参加することになった。
参加者は予定範囲を読んでいることを前提としており、モデレータが本の流れをなぞりながら都度質問や議論をするという感じの進め方だった。
以下は第1回の個人的なメモである。

## 1章 拡張可能なプログラミング言語
Lispが拡張可能な言語であり、ボトムアップスタイルのプログラミングに向いているということが主張されている。
ボトムアッププログラミングの対義語はトップダウンなデザインであり、ダムの建設など事前に全てパーツを計画し、それらを順序立てて組み立てていくやり方のことを指している。
ボトムアッププログラミングでは細かく汎用的なユーティリティを作っていき、その層を重ねていくということのようだ。より探索的なアプローチといえる。
プログラムはソフトウェアなのでダムの建設と違って柔らかく、一度作ってしまえば後戻りできないというものでもないため、このようなアプローチが向いている場面もある。
Lispは言語自体が柔軟なので汎用的なユーティリティを作りやすい(例として高階関数とマクロが挙げられている) → 言語自体を成長させていくことが容易、ということだと思った。

この辺はUNIX的な考え方−すなわち直交したシンプルなコマンドラインツールをシェル上で組み合わせることによって複雑なタスクを実現する−と近いという意見も出た。

問題に合わせてDSLを作るとより問題を短く記述できるため、メンテナンス性が増し、独自に導入した流儀を揃えることができる環境では威力を発揮すると主張している。
そのため小規模グループでの開発に向いているとされている。

感想:
最近では、高階関数やパターンマッチベースのマクロ機能はLispの専売特許というわけでもなくなってきているので、他の言語でもボトムアップアプローチはできるし、普通にやられていると思う。
とはいえCommon Lispのいい意味での枯れ具合とインタラクティブ性が高性能な処理系と同居しているというのは、いまだに特異的だと思っている。

## 2章 関数
Lispはほとんど関数の集合体であり、例外が少ない。
普通の言語では組込みオペレータであるような`+`も単なる関数として実装されている。

2章冒頭では関数定義の方法、lambdaによる無名関数、関数と変数の名前空間が異なりそれぞれにアクセスする方法が異なることが説明される。
関数もLispのデータ(操作対象)であるという例として、シンボルの属性リストに関数を格納して呼び出す例が出てきた。
Common Lispのシンボルが実はリッチなデータ構造で、属性リストを持てるというのが説明なしに出てくるので解説した。

```lisp
(defun behave (animal)
  (funcall (get animal 'behavior)))

(setf (get 'dog 'behavior)
      #'(lambda ()
          (print "bark")))

(behave 'dog)
; "bark"
```

### レキシカルスコープとダイナミックスコープ
最近の言語ではレキシカルスコープが当たり前になっているので、むしろダイナミックスコープが何かという話になった。
`defvar`などでスペシャル変数宣言するとダイナミックスコープになる(関数内部で自由な変数を外側のletで束縛するなどして実行時に変更できる)という説明をした(たぶん合ってる)

```
;; ダイナミックスコープの例
(defvar *y*)

(defun scope-test2 (x)
  (list x *y*))

(let ((*y* 5))
  (scope-test 3))
; => (3 5)
```

### レキシカルクロージャ
関数が作られた環境への参照を関数内に閉じ込めることができ、そのような関数をクロージャと呼ぶ、ということだと理解している。

次の例では `db` が連想リストで、`make-dbms`内で作られている3つのアクセサ関数経由でしか参照も変更もできないというカプセル化がなされている。
```lisp
(defun make-dbms (db)
  (list
   ;; referrer
   #'(lambda (key)
       (cdr (assoc key db)))
   ;; setter
   #'(lambda (key val)
       (push (cons key val) db)
       key)
   ;; cleaner
   #'(lambda (key)
       (setf db (delete key db :key #'car))
       key)))

(defparameter cities (make-dbms '((boston . us) (paris . france))))
;; referrer
(funcall (first cities) 'boston) ; => US
(funcall (first cities) 'paris) ; => FRANCE

;; setter
(funcall (second cities) 'london 'england)

(funcall (first cities) 'london) ; => ENGLAND

;; cleaner
(funcall (third cities) 'london)
(funcall (first cities) 'london) ; => NIL
```

#### ローカル関数
`labels`を導入して関数内の補助関数をローカル関数として定義する例が出てくる。
このinstances-inが外側のobjを閉じ込めているのでこれもクロージャの例になっている。


```
(defun count-instances (obj lsts)
  (labels ((instances-in (lst)
             (if (consp lst)
                 (+ (if (eq (car lst) obj) 1 0)
                    (instances-in (cdr lst)))
                 0)))
    (mapcar #'instances-in lsts)))

;; リストの各要素リスト内のaの数を数える
(count-instances 'a '((a b c) (d a r p a) (d a r) (a a)))
```

余談として、labels以外にも `flet` や `flet*` などもあって、これらを使うと再帰を使っていないことが明確になるという話をした。
とはいえlabelsがあればこれらは基本いらないはず。とにかく言語要素を少なくするという考え方と、用途が微妙に異なる構文を多数用意しておくことでコンテキスト情報を与えるとう考え方があり、Common Lispは後者であるということだと思う。

Schemeのように内部関数定義でもいいのでは？と思うが、defunを使うとトップレベルに定義される。
```
(defun count-instances (obj lsts)
  (defun instances-in (lst)
    (if (consp lst)
        (+ (if (eq (car lst) obj) 1 0)
           (instances-in (cdr lst)))
        0))
  (mapcar #'instances-in lsts))

;; count-instancesを呼ぶ度にinstances-inが上書かれる
(count-instances 'b '((a b c) (d a r p a) (d a r) (a a)))
(instances-in '(d a r p a)) ; => 0
```

### 末尾再帰
ここから次回ということになった。
次回は自分がモデレータなので予習しておかねば…
