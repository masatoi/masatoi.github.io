---
layout: post
title: "ChainerのMNISTの例をHyに翻訳してみる"
description: ""
category: 
tags: [lisp,hylang,python]
---
{% include JB/setup %}

前回Hyのチュートリアルをやってみたので、今度は[Chainerのサンプルコード](https://github.com/pfnet/chainer/blob/master/examples/mnist/train_mnist.py){:target="_blank"}をHyに翻訳してみる。

ほとんど逐語訳で済むが、注意点としては、

- アンダーバーはハイフンに直す
- オブジェクトの属性はそのまま`obj.attr`でアクセスできる
- メソッド呼び出しは`(obj.method arg1 arg2)`でも`(.method obj arg1 arg2)`でもよい
- Hyでの多重代入(分配束縛)のやりかたはよく分からなかった。マクロを書けば作れる

# コード
``` clojure
;; -*- coding:utf-8; mode:hy -*-

(try
 (import matplotlib)
 (except [ImportError]))

(import [argparse]
        [chainer]
        [chainer.functions :as F]
        [chainer.links :as L]
        [chainer [training]]
        [chainer.training [extensions]])

(defclass MLP [chainer.Chain]
  (defn --init-- [self n-units n-out]
    (.--init-- (super MLP self)
               :l1 (L.Linear None n-units)
               :l2 (L.Linear None n-units)
               :l3 (L.Linear None n-out)))
  (defn --call-- [self x]
    (setv h1 (F.relu (self.l1 x))
          h2 (F.relu (self.l2 h1)))
    (self.l3 h2)))

(defn main []
  (setv parser (argparse.ArgumentParser :description "Chainer example: MNIST"))
  (.add-argument parser "--batchsize" "-b"
                 :type int :default 100 :help "Number of images in each mini-batch")
  (.add-argument parser "--epoch" "-e"
                 :type int :default 20 :help "Number of sweeps over the dataset to train")
  (.add-argument parser "--frequency" "-f"
                 :type int :default 1 :help "Frequency of taking a snapshot")
  (.add-argument parser "--gpu" "-g"
                 :type int :default -1 :help "GPU ID (negative value indicates CPU)")
  (.add-argument parser "--out" "-o"
                 :default "result" :help "Directory to output the result")
  (.add-argument parser "--resume" "-r"
                 :default "" :help "Resume the training from snapshot")
  (.add-argument parser "--unit" "-u"
                 :type int :default 1000 :help "Number of units")
  (setv args (.parse-args parser))

  (print (.format "GPU: {}" args.gpu))
  (print (.format "# unit: {}" args.unit))
  (print (.format "# Minibatch-size: {}" args.batchsize))
  (print (.format "# epoch: {}" args.epoch))
  (print "")

  ;; Set up a neural network to train
  ;; Classifier reports softmax cross entropy loss and accuracy at every
  ;; iteration, which will be used by the PrintReport extension below.

  (setv model (L.Classifier (MLP args.unit 10)))

  (when (>= args.gpu 0)
    (.use (chainer.cuda.get-device args.gpu)) ; Make a specified GPU current
    (.to-gpu model))                          ; Copy the model to the GPU
  
  ;; Setup an optimizer
  (setv optimizer (chainer.optimizers.Adam))
  (.setup optimizer model)

  ;; Load the MNIST dataset
  (setv mnist-dataset (chainer.datasets.get-mnist)
        train (first mnist-dataset)
        test  (second mnist-dataset)
        train-iter (chainer.iterators.SerialIterator train args.batchsize)
        test-iter  (chainer.iterators.SerialIterator test  args.batchsize
                                                     :repeat False :shuffle False))
  
  ;; Set up a trainer
  (setv updater (training.StandardUpdater train-iter optimizer :device args.gpu)
        trainer (training.Trainer updater [args.epoch "epoch"] :out args.out))

  ;; Evaluate the model with the test dataset for each epoch
  (trainer.extend (extensions.Evaluator test-iter model :device args.gpu))

  ;; Dump a computational graph from 'loss' variable at the first iteration
  ;; The "main" refers to the target link of the "main" optimizer.
  (trainer.extend (extensions.dump-graph "main/loss"))

  ;; Take a snapshot for each specified epoch
  (setv frequency (if (= args.frequency -1) args.epoch (max 1 args.frequency)))
  (trainer.extend (extensions.snapshot) :trigger [frequency "epoch"])
  
  ;; Write a log of evaluation statistics for each epoch
  (trainer.extend (extensions.LogReport))

  ;; Save two plot images to the result dir
  (when (extensions.PlotReport.available)
    (trainer.extend (extensions.PlotReport ["main/loss" "validation/main/loss"]
                                           "epoch" :file_name "loss.png"))
    (trainer.extend (extensions.PlotReport ["main/accuracy" "validation/main/accuracy'"]
                                           "epoch" :file_name "accuracy.png")))

  ;; Print selected entries of the log to stdout
  ;; Here "main" refers to the target link of the "main" optimizer again, and
  ;; "validation" refers to the default name of the Evaluator extension.
  ;; Entries other than 'epoch' are reported by the Classifier link, called by
  ;; either the updater or the evaluator.
  (trainer.extend (extensions.PrintReport
                   ["epoch" "main/loss" "validation/main/loss" "main/accuracy"
                            "validation/main/accuracy" "elapsed_time"]))

  ;; Print a progress bar to stdout
  (trainer.extend (extensions.ProgressBar))

  (if args.resume
    ;; Resume from a snapshot
    (chainer.serializers.load_npz args.resume trainer))
  
  ;; Run the training
  (trainer.run)
  )

(if (= --name-- "__main__") (main))
```
