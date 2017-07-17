---
layout: post
title: "HyからKerasを利用してMNISTを分類する"
description: ""
category: 
tags: [lisp,hy,keras,machine-learning]
---
{% include JB/setup %}

以前HyからChainerを利用してMNISTを分類する例を上げたので、今度はKerasでやってみる。

# 参考

- [30秒でkerasに入門しましょう](https://keras.io/ja/#30keras){:target="_blank"}
- [KerasでMNIST (人工知能に関する断創録)](http://aidiary.hatenablog.com/entry/20161109/1478696865){:target="_blank"}

# インストール

```
pip install tensorflow-gpu
pip install keras
```

# データの用意とモデル定義

```clojure
(import [keras.models [Sequential]]
        [keras.layers [Dense Dropout Activation]]
        [keras.optimizers [Adam]]
        [keras.datasets [mnist]]
        [keras.utils [np-utils]])

;;; データセット(MNIST)の取得と前処理
(def mnist-dataset (mnist.load-data))
(def X-train (first (first mnist-dataset)))
(def Y-train (second (first mnist-dataset)))
(def X-test (first (second mnist-dataset)))
(def Y-test (second (second mnist-dataset)))

;; 画像を1次元配列化
(setv X-train (.reshape X-train 60000 784)
      X-test  (.reshape X-test  10000 784))

;; 画素を0.0-1.0の範囲に変換
(setv X-train (.astype X-train "float32")
      X-test  (.astype X-test  "float32"))
(/= X-train 255)
(/= X-test  255)

(print X-train.shape "train samples")
(print X-test.shape "test samples")

;; one-hot-encoding
(setv Y-train (np-utils.to-categorical Y-train 10)
      Y-test  (np-utils.to-categorical Y-test  10))

;;; モデル定義
(def model (Sequential))
(.add model (Dense :input-dim 784 :units 512))
(.add model (Activation "relu"))
(.add model (Dropout 0.2))
(.add model (Dense :units 512))
(.add model (Activation "relu"))
(.add model (Dropout 0.2))
(.add model (Dense :units 10))
(.add model (Activation "softmax"))
(.compile model
          :loss "categorical_crossentropy"
          :optimizer (Adam)
          :metrics ["accuracy"])

(.summary model)
```

```
_________________________________________________________________
Layer (type)                 Output Shape              Param #   
=================================================================
dense_1 (Dense)              (None, 512)               401920    
_________________________________________________________________
activation_1 (Activation)    (None, 512)               0         
_________________________________________________________________
dropout_1 (Dropout)          (None, 512)               0         
_________________________________________________________________
dense_2 (Dense)              (None, 512)               262656    
_________________________________________________________________
activation_2 (Activation)    (None, 512)               0         
_________________________________________________________________
dropout_2 (Dropout)          (None, 512)               0         
_________________________________________________________________
dense_3 (Dense)              (None, 10)                5130      
_________________________________________________________________
activation_3 (Activation)    (None, 10)                0         
=================================================================
Total params: 669,706
Trainable params: 669,706
Non-trainable params: 0
_________________________________________________________________
```

マクロを使うとモデル定義を多少すっきりさせられる。

```clojure
(defmacro define-sequential [model layers-def compile-options-dict]
  (setv layers
        (list (map (fn (elem)
                     `(.add ~model ~elem))
                   layers-def)))
  `(do (def ~model (Sequential))
       ~@layers
       (.compile ~model ~@compile-options-dict)
       ~model))

(define-sequential model
  ;; layer
  [(Dense :input-dim 784 :units 512)
   (Activation "relu")
   (Dropout 0.2)
   (Dense :units 512)
   (Activation "relu")
   (Dropout 0.2)
   (Dense :units 10)
   (Activation "softmax")]
  ;; options
  {:loss "categorical_crossentropy"
   :optimizer (Adam)
   :metrics ["accuracy"]})
```

# 訓練、テスト、予測

```clojure
;; 訓練を実行
(def history (.fit model X-train Y-train :nb-epoch 5 :batch-size 100))

;; テスト
(def loss-metrics (.evaluate model X-test Y-test :batch-size 100))

(for [i (range 4)]
  (.fit model X-train Y-train :nb-epoch 5 :batch-size 100)
  (print "\n" (.evaluate model X-test Y-test :batch-size 100)))

;; 予測(分類結果)
(def classes (.predict-classes model X-test :batch-size 100))
;; 予測(分布)
(def proba (.predict-proba model X-test :batch_size 100))
```

```
Epoch 1/10
2017-06-20 13:31:26.460964: W tensorflow/core/platform/cpu_feature_guard.cc:45] The TensorFlow library wasn't compiled to use SSE4.1 instructions, but these are available on your machine and could speed up CPU computations.
2017-06-20 13:31:26.460990: W tensorflow/core/platform/cpu_feature_guard.cc:45] The TensorFlow library wasn't compiled to use SSE4.2 instructions, but these are available on your machine and could speed up CPU computations.
2017-06-20 13:31:26.460997: W tensorflow/core/platform/cpu_feature_guard.cc:45] The TensorFlow library wasn't compiled to use AVX instructions, but these are available on your machine and could speed up CPU computations.
2017-06-20 13:31:26.461001: W tensorflow/core/platform/cpu_feature_guard.cc:45] The TensorFlow library wasn't compiled to use AVX2 instructions, but these are available on your machine and could speed up CPU computations.
2017-06-20 13:31:26.461007: W tensorflow/core/platform/cpu_feature_guard.cc:45] The TensorFlow library wasn't compiled to use FMA instructions, but these are available on your machine and could speed up CPU computations.
2017-06-20 13:31:26.683495: I tensorflow/stream_executor/cuda/cuda_gpu_executor.cc:893] successful NUMA node read from SysFS had negative value (-1), but there must be at least one NUMA node, so returning NUMA node zero
2017-06-20 13:31:26.683701: I tensorflow/core/common_runtime/gpu/gpu_device.cc:940] Found device 0 with properties: 
name: GeForce GTX 750 Ti
major: 5 minor: 0 memoryClockRate (GHz) 1.0845
pciBusID 0000:01:00.0
Total memory: 1.95GiB
Free memory: 1.60GiB
2017-06-20 13:31:26.683719: I tensorflow/core/common_runtime/gpu/gpu_device.cc:961] DMA: 0 
2017-06-20 13:31:26.683724: I tensorflow/core/common_runtime/gpu/gpu_device.cc:971] 0:   Y 
2017-06-20 13:31:26.683733: I tensorflow/core/common_runtime/gpu/gpu_device.cc:1030] Creating TensorFlow device (/gpu:0) -> (device: 0, name: GeForce GTX 750 Ti, pci bus id: 0000:01:00.0)
60000/60000 [==============================] - 76s - loss: 0.2144 - acc: 0.9345    
Epoch 2/10
60000/60000 [==============================] - 6s - loss: 0.1040 - acc: 0.9682     
Epoch 3/10
60000/60000 [==============================] - 6s - loss: 0.0821 - acc: 0.9744     
Epoch 4/10
60000/60000 [==============================] - 6s - loss: 0.0664 - acc: 0.9789     
Epoch 5/10
60000/60000 [==============================] - 6s - loss: 0.0574 - acc: 0.9820     
Epoch 6/10
60000/60000 [==============================] - 6s - loss: 0.0523 - acc: 0.9840     
Epoch 7/10
60000/60000 [==============================] - 6s - loss: 0.0454 - acc: 0.9858     
Epoch 8/10
60000/60000 [==============================] - 6s - loss: 0.0440 - acc: 0.9867     
Epoch 9/10
60000/60000 [==============================] - 6s - loss: 0.0388 - acc: 0.9879     
Epoch 10/10
60000/60000 [==============================] - 6s - loss: 0.0351 - acc: 0.9890     
<keras.callbacks.History object at 0x7f7882ace6d8>
=> (.fit model X-train Y-train :nb-epoch 10 :batch_size 32)
Epoch 1/10
60000/60000 [==============================] - 6s - loss: 0.0341 - acc: 0.9896     
Epoch 2/10
60000/60000 [==============================] - 6s - loss: 0.0350 - acc: 0.9897     
Epoch 3/10
60000/60000 [==============================] - 6s - loss: 0.0332 - acc: 0.9900     
Epoch 4/10
60000/60000 [==============================] - 6s - loss: 0.0326 - acc: 0.9912     
Epoch 5/10
60000/60000 [==============================] - 6s - loss: 0.0325 - acc: 0.9907     
Epoch 6/10
60000/60000 [==============================] - 6s - loss: 0.0293 - acc: 0.9917     
Epoch 7/10
60000/60000 [==============================] - 6s - loss: 0.0277 - acc: 0.9921     
Epoch 8/10
60000/60000 [==============================] - 6s - loss: 0.0294 - acc: 0.9919     
Epoch 9/10
60000/60000 [==============================] - 6s - loss: 0.0257 - acc: 0.9928     
Epoch 10/10
60000/60000 [==============================] - 6s - loss: 0.0291 - acc: 0.9919     
<keras.callbacks.History object at 0x7f7893573a20>
```
