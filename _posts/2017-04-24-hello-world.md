---
layout: post
title: "Hello World"
description: ""
category: 
tags: [misc]
---
{% include JB/setup %}

## はてなダイアリーからの移行
はてなははてなダイアリーからはてなブログへ移行することを推奨しているのだが、これまでは移行するメリットを見出せず放りっぱなしにしていた。
しかし最近Chromeではてなダイアリーにアクセスすると異常に遅いことに気付いた。どうもメンテナンスされていないJavascriptが原因っぽい。それでいい加減に他のブログシステムに移行した方がいいと思うようになった。

日々のメモはEmacsのorg-modeで取っているので、できればブログもorg-modeで編集できた方がいいのだが、pandocなどでorg-modeからmarkdownに変換する方法もあるということなので、メジャーな方法、Jekyll bootstrap+Github pagesでいくことにした。これだとレスポンスも速いし情報源もたくさんある。

まずやったことはrbenvのインストール、最新のrubyをインストール (ruby2以降でないとJekyllがインストールできないので)。それからgithubでブログサイトのレポジトリをつくった。基本的には[ここ(Jekyll QuickStart)](http://jekyllbootstrap.com/usage/jekyll-quick-start.html)のサイト名の部分だけを差し替えて実行すればいい。

それから_config.ymlをいじってサイト情報を書き込んだりシンタックスハイライトを有効化したりする。

### テーマの変更
色々見たが、最初から入っているTwitterテーマを使うことにした。
`~/masatoi.github.io/assets/themes/twitter/css/style.css`をいじると反映されるので、[pygentsのテーマ](http://jwarby.github.io/jekyll-pygments-themes/languages/javascript.html)から適当なものを選んで叩き台にする。

なお、highlighterはrougeでないとコミットしたときにgithubからお叱りのメールが来る。

### markdown-modeのインストール
Emacsのpackageからmarkdown-modeを入れる。これでmarkdownのシンタックスハイライトとC-c C-c pでEmacs規定のブラウザでプレビューができる。もっとも、Jekyll serveでファイルの変更を監視していればmdファイルを保存した段階で`localhost:4000`でプレビューが見られる。

### コードのテスト
```common_lisp
(defun split-arr3 (pred arr true-array false-array)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array fixnum) arr true-array false-array))
  (let ((true-index 0)
        (false-index 0))
    (declare (type fixnum true-index false-index))
    (loop for elem fixnum across arr do
      (cond ((funcall pred elem)
             (setf (aref true-array true-index) elem)
             (incf true-index))
            (t
             (setf (aref false-array false-index) elem)
             (incf false-index))))
    (values true-array true-index false-array false-index)))
    
(defun main ()
  (print "common lisp start.")
  (loop for count from 1 to *n*
        collect (sumup3 count))
  (print "common lisp end."))
```

```ruby
def foo
  puts 'foo'
end
```
