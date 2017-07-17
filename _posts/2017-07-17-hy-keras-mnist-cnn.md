---
layout: post
title: "HyからKerasを利用してMNISTを分類する(畳み込みニューラルネット版)"
description: ""
category: 
tags: [lisp,hy,keras,machine-learning]
---
{% include JB/setup %}

## 参考

- [https://github.com/fchollet/keras/blob/master/examples/mnist_cnn.py](https://github.com/fchollet/keras/blob/master/examples/mnist_cnn.py){:target="_blank"}
- [KerasでCIFAR-10の一般物体認識(人工知能に関する断創録)](http://aidiary.hatenablog.com/entry/20161127/1480240182){:target="_blank"}

## コード

```clojure
(import [keras [*]]
        [keras.datasets [mnist]]
        [keras.models [Sequential]]
        [keras.layers [Dense Dropout Flatten]]
        [keras.layers [Conv2D MaxPooling2D]]
        [keras [backend :as K]])

(import keras)

(setv batch-size  128
      num-classes 10
      epochs      12)

;; input image dimensions
(setv img-rows 28
      img-cols 28)

;; the data, shuffled and split between train and test sets
(setv mnist-dataset (mnist.load-data)
      x-train (first  (first  mnist-dataset))
      y-train (second (first  mnist-dataset))
      x-test  (first  (second mnist-dataset))
      y-test  (second (second mnist-dataset)))

(if (= (K.image-data-format) "channels_first")
  (setv x-train (.reshape x-train (nth x-train.shape 0) 1 img-rows img-cols)
        x-test  (.reshape x-test  (nth x-test.shape 0)  1 img-rows img-cols)
        input-shape [1 img-rows img-cols])
  (setv x-train (.reshape x-train (nth x-train.shape 0) img-rows img-cols 1)
        x-test  (.reshape x-test  (nth x-test.shape 0)  img-rows img-cols 1)
        input-shape [img-rows img-cols 1]))

;; normalize [0.0 1.0]
(setv x-train (.astype x-train "float32")
      x-test  (.astype x-test  "float32"))
(/= x-train 255)
(/= x-test  255)

(print x-train.shape "train samples")
(print x-test.shape "test samples")

;; one-hot-encoding
(setv y-train (keras.utils.to-categorical y-train num-classes)
      y-test  (keras.utils.to-categorical y-test  num-classes))

;; define model
(defmacro define-sequential [model layers-def compile-options-dict]
  (setv layers
        (list (map (fn (elem) `(.add ~model ~elem)) layers-def)))
  `(do (def ~model (Sequential))
       ~@layers
       (.compile ~model ~@compile-options-dict)
       ~model))

(define-sequential model
  [(Conv2D 32 :kernel-size [3 3] :input-shape input-shape :activation "relu")
   (Conv2D 64 :kernel-size [3 3] :activation "relu")
   (MaxPooling2D :pool-size [2 2])
   (Dropout 0.25)
   (Conv2D 32 :kernel-size [3 3] :activation "relu")
   (Conv2D 64 :kernel-size [3 3] :activation "relu")
   (MaxPooling2D :pool-size [2 2])
   (Dropout 0.25)
   (Flatten)
   (Dense :units 128 :activation "relu")
   (Dropout 0.5)
   (Dense :units num-classes :activation "softmax")]
  {:loss keras.losses.categorical-crossentropy
   :optimizer (keras.optimizers.Adadelta)
   :metrics ["accuracy"]})

(.summary model)

;; run
(def history
  (.fit model x-train y-train
        :batch-size batch-size :epochs epochs :verbose 1 :validation-data [x-test y-test]))

;; test
(def loss-metrics (.evaluate model x-test y-test :batch-size batch-size))
(print "loss-metrics: " loss-metrics)

;; predict
(def classes (.predict-classes model x-test :batch-size batch-size))
(print "predicted classes: " classes)

(def proba (.predict-proba model x-test :batch_size batch-size))
(print "predicted proba: " proba)
```

50エポックくらい回すと99.55%くらいまでいく。
