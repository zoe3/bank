#使用ライブラリ
library(rpart)
library(dplyr)
library(pROC)
library(partykit)

#partykitのインストール
#install.packages("partykit")

##データ読込
train<-read.csv("../motodata/train.csv", header=TRUE)
test<-read.csv("../motodata/test.csv", header=TRUE)

##決定木モデル作成
#可視化のため目的変数の型をファクターに変換
train$y<-as.factor(train$y)

#決定木モデル作成
tree<-rpart(y~., #目的変数~説明変数, 「.」はy以外の全変数を説明変数とするという意味
            data=train, #学習データ
            maxdepth=2, #階層数
            minbucket=500, #最小ノードサイズ
            cp=0.001, #枝刈りの強さ
            method="class", #分類
            parms=list(split="gini") #分割基準, list(split="information")とするとEntropy基準となる
            )

tree<-rpart(y~., #目的変数~説明変数, 「.」はy以外の全変数を説明変数とするという意味
            data=train, #学習データ
            maxdepth=8, #階層数
            minbucket=100, #最小ノードサイズ
            cp=0.0001, #枝刈りの強さ
            method="class", #分類
            parms=list(split="gini") #分割基準, list(split="information")とするとEntropy基準となる
            )

tree<-rpart(y~., #目的変数~説明変数, 「.」はy以外の全変数を説明変数とするという意味
            data=train, #学習データ
            maxdepth=8, #階層数
            minbucket=100, #最小ノードサイズ
            cp=0.0001, #枝刈りの強さ
            method="class", #分類
            parms=list(split="information")
            )


#構築データでのAUC
#モデルの当てはめ
pred_train<-predict(tree, train)[,2]
#AUC
auc<-roc(train$y, pred_train)
print(auc)
##Area under the curve: 0.7487

## maxdepth=8, #階層数
## minbucket=100, #最小ノードサイズ
## Area under the curve: 0.8493

## maxdepth=8, #階層数
## minbucket=100, #最小ノードサイズ
## cp=0.001, #枝刈りの強さ
## method="class", #分類
## parms=list(split="information")
## ->
## Area under the curve: 0.859

##可視化
plot(as.party(tree))

#モデルの当てはめ
tree2<-rpart(y~., #目的変数~説明変数, 「.」はy以外の全変数を説明変数とするという意味
            data=train, #学習データ
            maxdepth=2, #階層数
            minbucket=500, #最小ノードサイズ
            cp=0.001, #枝刈りの強さ
            method="class", #分類
            parms=list(split="information") #Entropy基準
            )

pred_train2<-predict(tree2, train)[,2]
#AUC
auc2<-roc(train$y, pred_train2)
print(auc2)
## Area under the curve: 0.7832

plot(as.party(tree2))


##予測データに決定木モデルを適用
#モデルの当てはめ
pred_test<-predict(tree, test)[,2]

#submitの形式で出力(CSV)
#データ加工
out<-data.frame(test$id, pred_test)

#出力
write.table(out, #出力データ
            "C:/study/bank/submit/submit_20170927_1_rpart.csv", #出力先
            quote=FALSE, #文字列を「"」で囲む有無
            col.names=FALSE, #変数名(列名)の有無
            row.names=FALSE, #行番号の有無
            sep="," #区切り文字の指定
)

## 0.862397745897
