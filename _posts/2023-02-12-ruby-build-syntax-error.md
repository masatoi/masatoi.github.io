---
layout: post
title: "メモ: ruby-buildがシンタックスエラーで失敗する"
description: ""
category: 
tags: [misc]
---
{% include JB/setup %}

# メモ: ruby-buildがシンタックスエラーで失敗する

Ubuntu 22.04上でrbenvでRuby3系をインストールしようとしたらruby-buildがシンタックスエラーで失敗してハマった。

ビルドログの末尾

```
rbconfig.rb updated
generating encdb.h
creating verconf.h
./template/fake.rb.in:19:in `eval': (eval):1: syntax error, unexpected backslash (SyntaxError)
\\# 71 "./version.c" 3         ...
^
	from ./template/fake.rb.in:19:in `value'
	from ./template/fake.rb.in:24:in `block (3 levels) in <main>'
	from ./template/fake.rb.in:23:in `scan'
	from ./template/fake.rb.in:23:in `block (2 levels) in <main>'
	from /usr/lib/ruby/3.0.0/erb.rb:905:in `eval'
	from /usr/lib/ruby/3.0.0/erb.rb:905:in `result'
	from ./tool/generic_erb.rb:36:in `block (2 levels) in <main>'
	from ./tool/generic_erb.rb:36:in `block in <main>'
	from ./tool/generic_erb.rb:29:in `map'
	from ./tool/generic_erb.rb:29:in `<main>'
make: *** [uncommon.mk:791: x86_64-linux-fake.rb] Error 1
make: *** Waiting for unfinished jobs....
encdb.h updated
verconf.h updated
```

Rubyのソースに手を入れる必要があるように見える。

よく分からんがエラーになっている `template/fake.rb.in` の `\\` を消す。

```
-  eval(val.gsub(/#/, '\\#').gsub(/((?:\G|[^\\])(?:\\\\)*)\n/, '\1'))
+  eval(val.gsub(/#/, '#').gsub(/((?:\G|[^\\])(?:\\\\)*)\n/, '\1'))
```

rbenvとruby-buildが別リポジトリになってて何故？と思ったが、rbenvはruby環境の切り替えがメインの仕事で、ビルドは手動でやって `~/.rbenv/versions` に置くというのでもいいらしい。

ソースをダウンロードしてきて `template/fake.rb.in` を直してビルドする。

```
sudo apt install autoconf bison patch build-essential rustc libssl-dev libyaml-dev libreadline6-dev zlib1g-dev libgmp-dev libncurses5-dev libffi-dev libgdbm6 libgdbm-dev libdb-dev uuid-dev

mkdir $HOME/.rbenv/versions/3.2.1
./configure --prefix=$HOME/.rbenv/versions/3.2.1/
make install
```
