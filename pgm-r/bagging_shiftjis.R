##バギングの実装例

##使用ライブラリ
library(dplyr)
library(rpart)
library(pROC)
library(ggplot2)

###データ分割
##精度を見たいのでホールドアウト法により構築データと検証データに分ける
##データ読込
train<-read.csv("../motodata/train.csv", header=TRUE)

##構築データの割合
rate<-0.7

##構築データ数(小数の切捨て)
num<-as.integer(nrow(train)*rate)

#########ランダムに構築データを取得########
##再現性のため乱数シードを固定
set.seed(17)

##sample(ベクトル, ランダムに取得する個数, 復元抽出の有無)
row<-sample(1:nrow(train), num, replace=FALSE)

##構築データ
train_train<-train[row,]

##検証データ
train_test<- train[-row,]

###比較用に普通に構築
##決定木作成
tree_tmp<-rpart(y~., data=dplyr::select(train_train, -id),
                maxdepth=10, minbucket=12, cp=0.000008,
                method="class", parms=list(split="gini"))

##検証データへ当てはめ
pred_test<-predict(tree_tmp, dplyr::select(train_test, -id, -y))[,2]

##AUCの計算
##roc(目的変数(1 or 0), 予測結果)
auc<-roc(train_test$y, pred_test)
print(auc)


###バギングの実装
##今回は全て決定木(rpart)を使用

##モデル構築回数
L<-10

##rate_M*(train_trainの行数)の数だけデータを復元抽出
rate_M<-0.1
M<-as.integer(nrow(train_train)*rate_M)

##再現性のため乱数シードを固定
set.seed(17)

##バギング(モデルは全て決定木)
auc<-NULL
tree_tmp<-as.list(NULL)
for(i in 1:L){
  
  #sample(ベクトル, ランダムに取得する個数, 復元抽出の有無)
  row<-sample(1:nrow(train_train), M, replace=TRUE)
  
  #復元抽出による構築データ
  train_tmp<-train_train[row,]
  
  #決定木作成
  tree_tmp<-rpart(y~., data=dplyr::select(train_tmp, -id),
                  maxdepth=10, minbucket=12, cp=0.000008,
                  method="class", parms=list(split="gini"))
  
  #検証データへ当てはめ
  pred_test<-predict(tree_tmp, dplyr::select(train_test, -id, -y))[,2]
  
  #AUCの計算
  #roc(目的変数(1 or 0), 予測結果)
  auc_tmp<-roc(train_test$y, pred_test)
  auc <- c(auc, as.numeric(auc_tmp$auc))
  
  #予測結果をとっておく
{
    if(i==1){score_tmp<-pred_test}
    else{score_tmp<-data.frame(score_tmp, pred_test)}
  }

}

##予測結果の平均
##apply(データ, 1, 関数)でデータを行ごとに横ベクトルとして関数に適用
score<-apply(score_tmp, 1, mean)

##AUCの計算
##roc(目的変数(1 or 0), 予測結果)
auc_tmp<-roc(train_test$y, score)
auc <- c(auc, as.numeric(auc_tmp$auc))

##結果の確認
model<-c(as.character(1:L),"ALL_mean")
dat<-data.frame(model, AUC=auc)

print(dat)

g<-ggplot(dat, aes(x=model, y=AUC)) + geom_bar(stat="identity")
plot(g)
