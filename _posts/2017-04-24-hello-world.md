---
layout: post
title: "Hello World"
description: ""
category: 
tags: [misc]
---
{% include JB/setup %}

### はてなダイアリーからの移行
はてなははてなダイアリーからはてなブログへ移行することを推奨しているが、自分ははてなダイアリーで十分だと思って放りっぱなしにしていた。
だが最近Chromeではてなダイアリーにアクセスすると異常に遅いことに気付き、どうもメンテナンスされていないJavascriptが原因っぽいので、そろそろ他のブログシステムに移行した方がいいと思うようになった。

日々のメモはEmacsのorg-modeで取っているので、できればorg-modeで編集できた方がいいのだが、pandocなどでorg-modeからmarkdownに変換する方法もあるので、メジャーな方法、つまりJekyll+Github pagesでいくことにした。

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
