##スタッキングの実装例(投稿用)

#使用ライブラリ
library(dplyr)
library(rpart)
library(pROC)
library(ggplot2)
library(partykit)

#データ読込
train<-read.csv("../motodata/train.csv", header=TRUE)
test<-read.csv("../motodata/test.csv", header=TRUE)

#### データ加工
## Job
test$y <- 9
combi <- rbind(train,test)

combi <- combi %>%
    dplyr::mutate(job2 = if_else(job %in% c('retired','students'), 'notworker', 'worker')) %>%
    dplyr::mutate(job2 = if_else(job == 'unknown' , 'unknown', job2)) %>%
    dplyr::mutate(job2 = as.factor(job2)) %>%
    glimpse

combi <- combi %>%
    dplyr::mutate(job3 = if_else(job %in% c('admin.','bule-collar','management','services','technician'), 'major', 'minor')) %>%
    dplyr::mutate(job3 = as.factor(job3)) %>%
    glimpse

train <- combi %>%
    dplyr::filter(y < 9)

str(train)

test <- combi %>%
    dplyr::filter(y == 9) %>%
    dplyr::select(-y)

str(test)

## 最終接触時間(duration)は外れ値を0.995%tileを寄せる。線形にするため、ルートを取る。
## 年齢(age)は、50で折り返し。
## 年間平均残高(balance)は、95%tileを取る。
train <- train %>%
    dplyr::mutate(duration2=ifelse(duration >= quantile(duration,probs=.995),
                                   quantile(duration,probs=.995),
                                   duration)) %>%
    dplyr::mutate(duration3 = sqrt(duration2)) %>%
    dplyr::mutate(age2=abs(50-age)) %>%
    dplyr::mutate(balance2=ifelse(balance >= quantile(balance,probs=.95),
                                  quantile(balance,probs=.95),
                                  balance))

test <- test %>%
    dplyr::mutate(duration2=ifelse(duration >= quantile(duration,probs=.995),
                                   quantile(duration,probs=.995),
                                   duration)) %>%
    dplyr::mutate(duration3 = sqrt(duration2)) %>%
    dplyr::mutate(age2=abs(50-age)) %>%
    dplyr::mutate(balance2=ifelse(balance >= quantile(balance,probs=.95),
                                  quantile(balance,probs=.95),
                                  balance))

## Customerを分割. 前回キャンペーン有
train_old <- train %>%
    dplyr::filter(pdays > -1)
test_old <- test %>%
    dplyr::filter(pdays > -1)

train_new <- train %>%
    dplyr::filter(pdays==-1) %>%
    dplyr::select(-c(pdays,previous,poutcome))
test_new <- test %>%
    dplyr::filter(pdays==-1) %>%
    dplyr::select(-c(pdays,previous,poutcome))


## 前キャンペーンの日付求める
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
train_old <- train_old %>%
    mutate(lastdate = as.POSIXct(as.Date(paste(day,month,"2017",sep=""),"%d%b%Y")))
test_old <- test_old %>%
    mutate(lastdate = as.POSIXct(as.Date(paste(day,month,"2017",sep=""),"%d%b%Y")))
train_old <- train_old %>%
    mutate(pdate = as.POSIXct(as.Date(paste(day,month,"2017",sep=""),"%d%b%Y") - pdays))
test_old <- test_old %>%
    mutate(pdate = as.POSIXct(as.Date(paste(day,month,"2017",sep=""),"%d%b%Y") - pdays))
Sys.setlocale("LC_TIME", lct)


##スタッキングの実装
#今回は決定木(rpart)とロジスティック回帰(glm)をロジスティック回帰(glm)でアンサンブル

### Old
#再現性のため乱数シードを固定
set.seed(17)
#学習データをK個にグループ分け
K<-5
#sample(ベクトル, ランダムに取得する個数, 復元抽出の有無, ベクトルの各要素が抽出される確率)
train_old$cv_group<-sample(1:K, nrow(train_old), replace=TRUE, prob=rep(1/K, K))

#構築, 検証データ予測スコアの初期化
score_train_tree<-NULL
score_train_logi<-NULL
score_test_tree<-NULL
score_test_logi<-NULL
y<-NULL

#クロスバリデーション
for(j in 1:K){
  #構築, 検証データに分ける
  train_tmp<-train_old %>%
    dplyr::filter(cv_group!=j) %>%
    dplyr::select(-cv_group)
  test_tmp<-train_old %>%
    dplyr::filter(cv_group==j) %>%
    dplyr::select(-cv_group)

  ## 構築データでモデル構築(決定木)
  tree_tmp<-rpart(y~., data=train_tmp,
                  maxdepth=10, minbucket=12, cp=0.000008,
                  method="class", parms=list(split="gini"))
  
  ## 構築データでモデル構築(ロジスティック回帰)
  logi_tmp<-glm(y~., data=train_tmp, family=binomial(link="logit"))

  #モデル構築に使用していないデータの予測値と目的変数
  pred_train_tree<-predict(tree_tmp, test_tmp)[,2]
  pred_train_logi<-predict(logi_tmp, test_tmp, type="response")
  y<-c(y, test_tmp$y)
  
  score_train_tree<-c(score_train_tree, pred_train_tree)
  score_train_logi<-c(score_train_logi, pred_train_logi)
  
  
  #検証データの予測値
  pred_test_tree<-predict(tree_tmp, test_old)[,2]
  pred_test_logi<-predict(logi_tmp, test_old, type="response")
  
  score_test_tree<-cbind(score_test_tree, pred_test_tree)
  score_test_logi<-cbind(score_test_logi, pred_test_logi)
  
}

#余計な変数削除
train_old <- train_old %>%
  dplyr::select(-cv_group)

#検証データの予測値の平均
#apply(データ, 1, 関数)で行ごとに関数を適用する
score_test_tree<-apply(score_test_tree, 1, mean)
score_test_logi<-apply(score_test_logi, 1, mean)
m_dat_test1<-data.frame(tree=score_test_tree, logi=score_test_logi)


#メタモデル用変数作成
m_dat_train<-data.frame(tree=score_train_tree, logi=score_train_logi, y=y)

#メタモデル構築(今回はロジスティック回帰)
m_logi<-glm(y~., data=m_dat_train, family=binomial(link="logit"))


##検証データ適用1
#メタモデル適用
pred_test_m_logi1<-predict(m_logi, m_dat_test1, type="response")

#CSV出力
submit1_old <- data.frame(id=test_old$id, score=pred_test_m_logi1)

### New
#再現性のため乱数シードを固定
set.seed(17)
#学習データをK個にグループ分け
K<-5
#sample(ベクトル, ランダムに取得する個数, 復元抽出の有無, ベクトルの各要素が抽出される確率)
train_new$cv_group<-sample(1:K, nrow(train_new), replace=TRUE, prob=rep(1/K, K))

#構築, 検証データ予測スコアの初期化
score_train_tree<-NULL
score_train_logi<-NULL
score_test_tree<-NULL
score_test_logi<-NULL
y<-NULL

#クロスバリデーション
for(j in 1:K){
  #構築, 検証データに分ける
  train_tmp<-train_new %>%
    dplyr::filter(cv_group!=j) %>%
    dplyr::select(-cv_group)
  test_tmp<-train_new %>%
    dplyr::filter(cv_group==j) %>%
    dplyr::select(-cv_group)
  
  ## 構築データでモデル構築(決定木)
  tree_tmp<-rpart(y~., data=train_tmp,
                  maxdepth=10, minbucket=12, cp=0.000008,
                  method="class", parms=list(split="gini"))
  
  ## 構築データでモデル構築(ロジスティック回帰)
  logi_tmp<-glm(y~., data=train_tmp, family=binomial(link="logit"))
  
  #モデル構築に使用していないデータの予測値と目的変数
  pred_train_tree<-predict(tree_tmp, test_tmp)[,2]
  pred_train_logi<-predict(logi_tmp, test_tmp, type="response")
  y<-c(y, test_tmp$y)
  
  score_train_tree<-c(score_train_tree, pred_train_tree)
  score_train_logi<-c(score_train_logi, pred_train_logi)
  
  
  #検証データの予測値
  pred_test_tree<-predict(tree_tmp, test_new)[,2]
  pred_test_logi<-predict(logi_tmp, test_new, type="response")
  
  score_test_tree<-cbind(score_test_tree, pred_test_tree)
  score_test_logi<-cbind(score_test_logi, pred_test_logi)
  
}

#余計な変数削除
train_new <- train_new %>%
  dplyr::select(-cv_group)

#検証データの予測値の平均
#apply(データ, 1, 関数)で行ごとに関数を適用する
score_test_tree<-apply(score_test_tree, 1, mean)
score_test_logi<-apply(score_test_logi, 1, mean)
m_dat_test1<-data.frame(tree=score_test_tree, logi=score_test_logi)


#メタモデル用変数作成
m_dat_train<-data.frame(tree=score_train_tree, logi=score_train_logi, y=y)

#メタモデル構築(今回はロジスティック回帰)
m_logi<-glm(y~., data=m_dat_train, family=binomial(link="logit"))


##検証データ適用1
#メタモデル適用
pred_test_m_logi1<-predict(m_logi, m_dat_test1, type="response")

#CSV出力
submit1_new <- data.frame(id=test_new$id, score=pred_test_m_logi1)

## Marge
submit1 <- rbind(submit1_old, submit1_new)

write.table(submit1,
            file="../submit/submit_20171108_ens_tree_logi_1.csv",
            quote=F, sep=",", row.names=F, col.names=F)




