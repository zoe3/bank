#使用ライブラリ
library(rpart)
library(pROC)
library(dplyr)
library(ggplot2)

#データ読込
train<-read.csv("../motodata/train.csv", header=T)

#データをK分割
K<-5


##まずはmaxdepth=5でクロスバリデーション
#再現性のため乱数シードを固定
set.seed(17)

#学習データをK個にグループ分け
#sample(ベクトル, ランダムに取得する個数, 復元抽出の有無, ベクトルの各要素が抽出される確率)
train$cv_group<-sample(1:K, nrow(train), replace=TRUE, prob=rep(1/K, K))

#構築, 検証データのスコアの初期化
score_train_tmp<-0
score_test_tmp<-0

#クロスバリデーション
for(j in 1:K){
  #構築, 検証データに分ける
  train_tmp<-train %>%
    dplyr::filter(cv_group!=j) %>%
    dplyr::select(-cv_group)
  test_tmp<-train %>%
    dplyr::filter(cv_group==j) %>%
    dplyr::select(-cv_group)
  #構築データでモデル構築(今回はrpartによる決定木)
  tree_tmp<-rpart(y~., data=train_tmp,
                  maxdepth=5, minbucket=1, cp=0.00001,
                  method="class", parms=list(split="gini"))
  
  #構築データの予測値
  pred_train<-predict(tree_tmp, train_tmp)[,2]
  
  #構築データのAUC
  auc<-roc(train_tmp$y, pred_train)$auc
  
  #構築データのAUCを足していく
  score_train_tmp<-score_train_tmp + auc
  
  #検証データの予測値
  pred_test<-predict(tree_tmp, test_tmp)[,2]
  
  #検証データのAUC
  auc<-roc(test_tmp$y, pred_test)$auc
  
  #検証データのAUCを足していく
  score_test_tmp<-score_test_tmp + auc
}

#構築データのAUC
print(score_train_tmp/K)
#検証データのAUC
print(score_test_tmp/K)

## 構築と検証のAUCが近い場合は過学習が行なわれていないと想定

##maxdepthを動かしてチューニング
#再現性のため乱数シードを固定
set.seed(17)

#1～15までチューニングする
#search_vec<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
search_vec<-c(1,5,10,25,50,75,100,125)

#構築, 検証データのスコアの初期化
score<-data.frame(train=NA, test=NA, search=search_vec)
l<-1
for(i in search_vec){
  
  #学習データをK個にグループ分け
  #sample(ベクトル, ランダムに取得する個数, 復元抽出の有無, ベクトルの各要素が抽出される確率)
  train$cv_group<-sample(1:K, nrow(train), replace=TRUE, prob=rep(1/K, K))
  
  #構築, 検証データのスコアの初期化
  score_train_tmp<-0
  score_test_tmp<-0
  
  #クロスバリデーション
  for(j in 1:K){
    
    #構築, 検証データに分ける
    train_tmp<-train %>%
      dplyr::filter(cv_group!=j) %>%
      dplyr::select(-cv_group)
    test_tmp<-train %>%
      dplyr::filter(cv_group==j) %>%
      dplyr::select(-cv_group)
    #構築データでモデル構築(今回はrpartによる決定木)
    #maxdepth以外のパラメータは適当に固定
    tree_tmp<-rpart(y~., data=train_tmp,
                    maxdepth=7, #iにはsearch_vecの値が順に入る
                    #maxdepth=i, #iにはsearch_vecの値が順に入る                    
                    minbucket=i, cp=0.00001,
                    method="class",
                    #parms=list(split="gini"))
                    parms=list(split="information"))
    
    #構築データの予測値
    pred_train<-predict(tree_tmp, train_tmp)[,2]
    
    #構築データのAUC
    auc<-roc(train_tmp$y, pred_train)$auc
    
    #構築データのAUCをとっておく
    score_train_tmp<-score_train_tmp + auc
    
    
    #検証データの予測値
    pred_test<-predict(tree_tmp, test_tmp)[,2]
    
    #検証データのAUC
    auc<-roc(test_tmp$y, pred_test)$auc
    
    #検証データのAUCをとっておく
    score_test_tmp<-score_test_tmp + auc
  }
  #構築データ(全体)の予測値
  score$train[l]<-score_train_tmp/K
  #検証データ(全体)の予測値
  score$test[l]<-score_test_tmp/K
  #scoreに代入する行数の更新
  l<-l+1
}

#過学習の確認(可視化)
g<-ggplot(score, aes(search))+
  geom_line(aes(y=train, color="train"))+geom_line(aes(y=test, color="test"))+
  xlab("Search")+ylab("AUC")+ggtitle("Cross validation")+
  scale_colour_discrete(name="Data Set")
plot(g)
