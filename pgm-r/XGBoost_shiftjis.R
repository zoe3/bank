##########使用ライブラリ##########
library(dplyr)
library(xgboost)
library(caret)
library(pROC)

#データ読み込み
train<-read.csv("../motodata/train.csv", header=T)

#カテゴリー変数を数値変数に変換
train_v<-as.data.frame(predict(dummyVars(~.,data=train), train))

#目的変数作成
y_train <- train$y

#行列に変換
x_train <-as.matrix(dplyr::select(train_v,-id,-y))

###Hold Out
#構築データの割合
rate<-0.7

#構築データ数(小数の切捨て)
num<-as.integer(nrow(x_train)*rate)

#再現性のため乱数シードを固定
set.seed(17)

#sample(ベクトル, ランダムに取得する個数, 復元抽出の有無)
row<-sample(1:nrow(x_train), num, replace=FALSE)

#構築データ
x_train_train<-x_train[row,]

#検証データ
x_train_test<-x_train[-row,]

#目的変数作成
y_train_train<- y_train[row]
y_train_test<- y_train[-row]

#パラメータの設定
set.seed(17)
param <- list(objective = "binary:logistic", #ロジスティック回帰で確率出力
              eval_metric = "auc", #評価指標
              eta=0.07, #学習率
              max_depth=3, #決定木の階層
              min_child_weight=10, #最小ノード数
              colsample_bytree=0.4, #使用する変数割合
              gamma=0.9, #損失還元最小値
              subsample=1 #使用する学習データ割合
)

#CVによる学習数探索
xgbcv <- xgb.cv(param=param, data=x_train_train, label=y_train_train,
                nrounds=200, #学習回数
                nfold=5, #CV数
                nthread=1 #使用するCPU数
)

#モデル構築
set.seed(17)
model_xgb <- xgboost(param=param, data = x_train_train, label=y_train_train,
                     nrounds=which.max(xgbcv$test.auc.mean), nthread=1, imprtance=TRUE)

#train_testのAUC
pred<-predict(model_xgb, x_train_test)
auc<-roc(y_train_test, pred)
print(auc)


#変数重要度
imp<- xgb.importance(names(dplyr::select(train_v,-id,-y)), model=model_xgb)
print(imp)

