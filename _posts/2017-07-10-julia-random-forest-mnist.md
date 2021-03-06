---
layout: post
title: "JuliaのランダムフォレストライブラリDecisionTree.jlでMNIST"
description: ""
category: 
tags: [julia,machine-learning]
---
{% include JB/setup %}

Juliaが速くて機械学習分野で人気と聞いたので試してみた。ランダムフォレストの実装があったのでこれでMNISTデータの分類を試してみる。

- [DecisionTree.jl (github)](https://github.com/bensadeghi/DecisionTree.jl){:target="_blank"}

# インストール

まず必要パッケージをインストールする。

```julia
Pkg.add("DecisionTree")
Pkg.add("MNIST")
```

# データの読み込みと前処理

MNISTパッケージにデータが含まれているが、DecisionTreeのデータとして使うにはちょっとした前処理が必要。

```julia
# MNISTパッケージの機能でMNISTのデータを読み込む
trainX, trainY = traindata()
testX, testY = testdata()

# 入力行列を転置
trainXt = transpose(trainX)
testXt  = transpose(testX)

# ターゲットが数値だと回帰になってしまうので文字列に直す
function setStringArr(arr,arrstr)
    dataSize = size(arr)[1]
    for i in 1:dataSize
        x=arr[i]
        arrstr[i]="$x"
    end
end

trainYstr = Array{String}(60000)
setStringArr(trainY,trainYstr)

testYstr = Array{String}(10000)
setStringArr(testY,testYstr)
```

# ランダムフォレストのモデル構築

データからランダムフォレストのモデルを作るには`build_frest()`を使う。引数は、サンプリングする特徴数、木の数、バギング比率、決定木の最大深さ。サンプリングする特徴数は推奨値であるところの元の特徴数784の平方根28を使う。

処理の時間を測るには`@time`を頭に付けておけばいいらしい。118秒かかっている。

```julia
# using 28 random features, 10 trees, 1.0 portion of samples per tree (optional), and a maximum tree depth of 15 (optional)
@time model = build_forest(trainYstr, trainXt, 28, 10, 1.0, 15)

# 118.467527 seconds (90.35 M allocations: 57.266 GiB, 18.03% gc time)
# Ensemble of Decision Trees
# Trees:      10
# Avg Leaves: 4097.6
# Avg Depth:  15.0
```

# テスト

```julia
predictions = apply_forest(model, testXt)
cm = confusion_matrix(testYstr, predictions)

# Classes:  String["0.0", "1.0", "2.0", "3.0", "4.0", "5.0", "6.0", "7.0", "8.0", "9.0"]
# Matrix:   10×10 Array{Int64,2}:
#  963     1    0    0    1    3    8    1    3    0
#    0  1118    3    4    0    1    4    2    3    0
#    7     0  979    5    4    2   11   14    8    2
#    1     0   21  938    1   23    2    8    8    8
#    1     0    5    1  905    1    9    4    8   48
#    5     1    2   37    1  813   13    3    9    8
#    9     3    2    0    4    2  936    0    2    0
#    0     2   19    7    2    0    0  970    5   23
#    5     0   10   18    5   10   11    2  889   24
#    5     4    4   11   11    3    3    7    6  955

# Accuracy: 0.9466
# Kappa:    0.94064143993818
```

で94.6%くらいの正答率になる。木の数が10にしては時間がかかりすぎている。topを見るかぎり並列化もしていないっぽい。
