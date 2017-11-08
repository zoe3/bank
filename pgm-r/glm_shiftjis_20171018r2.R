##使用ライブラリ
library(pROC)
library(dplyr)
library(ggplot2)

##データ読込
train<-read.csv("../motodata/train.csv", header=T)
test<-read.csv("../motodata/test.csv", header=T)

## Customerを分割. New Old
train_new <- train %>%
    dplyr::filter(pdays==-1) %>%
    dplyr::select(-c(pdays,previous,poutcome))
test_new <- test %>%
    dplyr::filter(pdays==-1) %>%
    dplyr::select(-c(pdays,previous,poutcome))

train_old <- train %>%
    dplyr::filter(pdays > -1)
test_old <- test %>%
    dplyr::filter(pdays > -1)

#### New
##精度確認のためHold Out
rate<-0.7
num<-as.integer(nrow(train_new)*rate)
set.seed(17)
row<-sample(1:nrow(train_new), num, replace=FALSE)

train_train<-train_new[row,]
train_test<-train_new[-row,]

######ロジスティック回帰モデル構築
logi_model <- glm(
    y ~ .,
    data=train_train,             ##学習データ
    family=binomial(link="logit") ##ロジスティック回帰を指定.二項分布に従う。
)

##モデルの中身を見る

summary(logi_model)
##モデルの精度確認
##モデルの当てはめ
pred_train_test<- predict(logi_model, newdata=train_test, type="response") ####決定木と違い返す値が決っている。

##AUC確認
auc<-roc(train_test$y, pred_train_test)$auc
auc

######変数加工
##線形性の確認
summary(train_new$duration)

hist(train_new$duration)

##パーセンタイル点の確認
quantile(train_new$duration,probs=c(.05,.95))
quantile(train_new$duration,probs=c(.05,.995))

##丸め(ifelse(条件式, 真のとき返す値, 偽のとき返す値))上のみ
train_new <- train_new%>%
  dplyr::mutate(duration2=ifelse(duration >= quantile(duration,probs=.995),
                                quantile(duration,probs=.995),
                                duration))

##外れ値の確認
hist(train_new$duration2)

check <-train_new%>%
    ## duration2ごとの対数オッズを計算
    dplyr::mutate(duration_c = floor(duration2/50)*50) %>%
    dplyr::group_by(duration_c) %>%
    dplyr::summarise(p=mean(y)) %>%  ####p反応率
    dplyr::ungroup(.) %>%
    dplyr::mutate(log_odds=log(p/(1-p)))

##グラフの出力
g<-ggplot(check, aes(x=duration_c, y=log_odds)) + geom_line()
plot(g)

##変数加工(duration2のsqrtで変換)
train_new2 <- train_new %>%
    dplyr::mutate(duration3 = sqrt(duration2)) %>%
    dplyr::select(-c(duration,duration2))

## duration3の分割する幅の検討
hist(train_new2$duration3)

##再び線形性の確認
check <-train_new2 %>%
    ## duration2ごとの対数オッズを計算
    dplyr::mutate(duration_c = floor(duration3/5)*5) %>%
    dplyr::group_by(duration_c) %>%
    dplyr::summarise(p=mean(y)) %>%  ####p反応率
    dplyr::ungroup(.) %>%
    dplyr::mutate(log_odds=log(p/(1-p)))

##グラフの出力
g<-ggplot(check, aes(x=duration_c, y=log_odds)) + geom_line()
plot(g)

## age, balanceの加工を入れる。
train_new2 <- train_new2 %>%
    dplyr::mutate(age2=abs(50-age)) %>%
    dplyr::mutate(balance2=ifelse(balance >= quantile(balance,probs=.95),
                                  quantile(balance,probs=.95),
                                  balance)) %>%
    dplyr::select(-c(age,balance))


## 多重共線性の確認
##相関係数の確認
## 目安
## |r| = 0.7 ~ 1 かなり強い相関がある
## |r| = 0.4 ~ 0.7 やや相関がある
## |r| = 0.2 ~ 0.4 弱い相関がある
## |r| = 0 ~ 0.2 ほとんど相関なし

cor(train2[,c('duration3','pdays')])

cor(train2[,c('duration3','id')])

cor(train2[,c("age2", "duration3")])

cor(train2[,c("age2", "balance2")])

cor(train2[,c('pdays','previous')])

cor(train2[,c('campaign','previous')])

#### 小さな負の相関がある。小さいので同時に入れても問題なし


hist(temp$pday)

toPoint <- function(factors) {
    mapping <- c("failure"=1, "other"=3, "success"=5, "unknown"=0)
    mapping[as.character(factors)]
}
train2$poutcome <- toPoint(train2$poutcome)
            
######新変数でロジスティック回帰モデル構築
##Hold Out(先ほどと同じ分け方)
train_new2 <- train_new2 %>%
    dplyr::select(-id)

train_train_new2<-train_new2[row,]
train_test_new2<-train_new2[-row,]

logi_model2 <- glm(
    y ~ .,    ##目的変数と説明変数の指定(全て使う場合はy~.)
    data=train_train_new2,             ##学習データ
    family=binomial(link="logit") ##ロジスティック回帰を指定
)

##モデルの中身を見る
summary(logi_model2)

##モデルの精度確認
##モデルの当てはめ
pred_train_test<- predict(logi_model2, newdata=train_test_new2, type="response")

o##AUC確認
auc<-roc(train_test$y, pred_train_test)$auc
auc


check <- data.frame(train_test, pred_train_test)

check %>%
    dplyr::arrange(desc(pred_train_test)) %>%
    head(100) %>%
    dplyr::summarise(sum(y))

######Submit
##testにもtrainと同様の加工をしてtrain全体でモデル構築してtestに適用
test_new2 <- test_new%>%
  dplyr::mutate(age2=abs(50-age),
                balance2=ifelse(balance >= quantile(balance,probs=.95),
                                quantile(balance,probs=.95),
                                balance)) %>%
  dplyr::mutate(duration3=sqrt(ifelse(duration >= quantile(duration,probs=.995),
                                      quantile(duration,probs=.995),
                                      duration)))

#### trainの95%tailでまとめる方が良い

logi_model3_new <- glm(
    y ~ .,
    data=train_new2,             ##学習データ 全部使う!!
    family=binomial(link="logit") ##ロジスティック回帰を指定
)

summary(logi_model3_new)

pred_test_new <- predict(logi_model3_new, newdata=test_new2, type="response")

#### Old
##精度確認のためHold Out
rate<-0.7
num<-as.integer(nrow(train_old)*rate)
set.seed(17)
row<-sample(1:nrow(train_old), num, replace=FALSE)

train_train_old <- train_old[row,]
train_test_old <- train_old[-row,]

######ロジスティック回帰モデル構築
logi_model <- glm(
    y ~ .,
    data=train_train_old,             ##学習データ
    family=binomial(link="logit") ##ロジスティック回帰を指定.二項分布に従う。
)

##モデルの中身を見る
summary(logi_model)

##モデルの精度確認
##モデルの当てはめ
pred_train_test_old <- predict(logi_model, newdata=train_test_old, type="response") ####決定木と違い返す値が決っている。

##AUC確認
auc<-roc(train_test_old$y, pred_train_test_old)$auc
auc

######変数加工
##線形性の確認
summary(train_old$duration)

hist(train_old$duration)

##パーセンタイル点の確認
quantile(train_old$duration,probs=c(.05,.95))
quantile(train_old$duration,probs=c(.05,.995))

##丸め(ifelse(条件式, 真のとき返す値, 偽のとき返す値))上のみ
train_old <- train_old%>%
  dplyr::mutate(duration2=ifelse(duration >= quantile(duration,probs=.995),
                                quantile(duration,probs=.995),
                                duration))

##外れ値の確認
hist(train_old$duration2)

check <-train_old %>%
    ## duration2ごとの対数オッズを計算
    dplyr::mutate(duration_c = floor(duration2/50)*50) %>%
    dplyr::group_by(duration_c) %>%
    dplyr::summarise(p=mean(y)) %>%  ####p反応率
    dplyr::ungroup(.) %>%
    dplyr::mutate(log_odds=log(p/(1-p)))

##グラフの出力
g<-ggplot(check, aes(x=duration_c, y=log_odds)) + geom_line()
plot(g)

##変数加工(duration2のsqrtで変換)
train_old2 <- train_old %>%
    dplyr::mutate(duration3 = sqrt(duration2)) %>%
    dplyr::select(-c(duration,duration2))

## duration3の分割する幅の検討
hist(train_old2$duration3)

##再び線形性の確認
check <-train_old2 %>%
    ## duration2ごとの対数オッズを計算
    dplyr::mutate(duration_c = floor(duration3/5)*5) %>%
    dplyr::group_by(duration_c) %>%
    dplyr::summarise(p=mean(y)) %>%  ####p反応率
    dplyr::ungroup(.) %>%
    dplyr::mutate(log_odds=log(p/(1-p)))

##グラフの出力
g<-ggplot(check, aes(x=duration_c, y=log_odds)) + geom_line()
plot(g)

## age, balanceの加工を入れる。
train_old2 <- train_old2 %>%
    dplyr::mutate(age2=abs(50-age)) %>%
    dplyr::mutate(balance2=ifelse(balance >= quantile(balance,probs=.95),
                                  quantile(balance,probs=.95),
                                  balance)) %>%
    dplyr::select(-c(age,balance))


## 多重共線性の確認
##相関係数の確認
## 目安
## |r| = 0.7 ~ 1 かなり強い相関がある
## |r| = 0.4 ~ 0.7 やや相関がある
## |r| = 0.2 ~ 0.4 弱い相関がある
## |r| = 0 ~ 0.2 ほとんど相関なし

cor(train_old2[,c('duration3','pdays')])

cor(train_old2[,c('duration3','id')])

cor(train_old2[,c("age2", "duration3")])

cor(train_old2[,c("age2", "balance2")])

cor(train_old2[,c('pdays','previous')])

cor(train_old2[,c('campaign','previous')])

cor(train_old2[,c("previous", "balance2")])

cor(train_old2[,c("campaign", "balance2")])

cor(train_old2[,c("pdays", "balance2")])

cor(train_old2[,c("duration3", "balance2")])

#### 小さな負の相関がある。小さいので同時に入れても問題なし

hist(temp$pday)

toPoint <- function(factors) {
    mapping <- c("failure"=1, "other"=3, "success"=5, "unknown"=0)
    mapping[as.character(factors)]
}
train2$poutcome <- toPoint(train2$poutcome)
            
######新変数でロジスティック回帰モデル構築
##Hold Out(先ほどと同じ分け方)
train_old2 <- train_old2 %>%
    dplyr::select(-id)

train_train_old2<-train_old2[row,]
train_test_old2<-train_old2[-row,]

logi_model2 <- glm(
    y ~ .,    ##目的変数と説明変数の指定(全て使う場合はy~.)
    data=train_train_old2,             ##学習データ
    family=binomial(link="logit") ##ロジスティック回帰を指定
)

##モデルの中身を見る
summary(logi_model2)

##モデルの精度確認
##モデルの当てはめ
pred_train_test_old <- predict(logi_model2, newdata=train_test_old2, type="response")

##AUC確認
auc<-roc(train_test_old$y, pred_train_test_old)$auc
auc

check <- data.frame(train_test_old, pred_train_test_old)

check %>%
    dplyr::arrange(desc(pred_train_test_old)) %>%
    head(100) %>%
    dplyr::summarise(sum(y))

######Submit
##testにもtrainと同様の加工をしてtrain全体でモデル構築してtestに適用
test_old2 <- test_old %>%
  dplyr::mutate(age2=abs(50-age),
                balance2=ifelse(balance >= quantile(balance,probs=.95),
                                quantile(balance,probs=.95),
                                balance)) %>%
  dplyr::mutate(duration3=sqrt(ifelse(duration >= quantile(duration,probs=.995),
                                      quantile(duration,probs=.995),
                                      duration)))

#### trainの95%tailでまとめる方が良い

logi_model3 <- glm(
    y ~ .,
    data=train_old2,             ##学習データ 全部使う!!
    family=binomial(link="logit") ##ロジスティック回帰を指定
)

summary(logi_model3)

pred_test_old <- predict(logi_model3, newdata=test_old2, type="response")



##submitの形式で出力(CSV)
##データ加工
out_new <- data.frame(id=test_new2$id, pred_test=pred_test_new)

out_old <- data.frame(id=test_old2$id, pred_test=pred_test_old)

out <- rbind(out_new, out_old)

##出力
write.table(out, ##出力データ
            "../submit/submit_20171018r2_1_logi.csv", ##出力先
            quote=FALSE, ##文字列を「"」で囲む有無
            col.names=FALSE, ##変数名(列名)の有無
            row.names=FALSE, ##行番号の有無
            sep="," ##区切り文字の指定
)


######参考：ステップワイズ法
logi_model_all <- glm(
  y ~ .,    ##目的変数と説明変数の指定(全て使う場合はy~.)
  data=train2,             ##学習データ
  family=binomial(link="logit") ##ロジスティック回帰を指定
)

##ステップワイズ
step.model_all <- step(logi_model_all)

##選択された変数の確認
summary(step.model_all)
## Call:
## glm(formula = y ~ job + marital + education + housing + loan + 
##     contact + day + month + campaign + poutcome + duration3 + 
##     age2 + balance2, family = binomial(link = "logit"), data = train2)
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -3.2268  -0.3629  -0.2131  -0.1175   3.5523  

logi_model_min <- glm(
    y ~ 1, 
    data=train2,             ##学習データ 全部使う!!
    family=binomial(link="logit") ##ロジスティック回帰を指定
)

step.model_min <- step(logi_model_min, direction='both',
                       scope=(~ age2 + job + marital + education + default + balance2 + housing + loan + contact + day + month + duration3 + campaign + pdays + previous + poutcome))

## 警告メッセージ: 
## glm.fit: 数値的に 0 か 1 である確率が生じました  

logi_model_min <- glm(
    y ~ duration3 + poutcome + month + contact + housing + age2 + 
    job + campaign + loan + balance2 , 
    data=train2,             ##学習データ 全部使う!!
    family=binomial(link="logit") ##ロジスティック回帰を指定
)

step.model_min <- step(logi_model_min, direction='both',
                       scope=(~ age2 + job + marital + default + balance2 + housing + loan + contact + month + duration3 + campaign + pdays + previous + poutcome))


##選択された変数の確認
summary(step.model_min)
## Call:
## glm(formula = y ~ duration3 + poutcome + month + contact + housing + 
##     age2 + job + campaign + loan + education + day + marital + 
##     balance2, family = binomial(link = "logit"), data = train2)
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -3.2268  -0.3629  -0.2131  -0.1175   3.5523  

logi_model_4 <- glm(formula = y ~ duration3 + poutcome + month + contact + housing + 
    age2 + job + campaign + loan + education + day + marital + 
    balance2, family = binomial(link = "logit"), data = train2)

pred_test <- predict(logi_model_4, newdata=test2, type="response")

##submitの形式で出力(CSV)
##データ加工
out<-data.frame(test2$id, pred_test)

##出力
write.table(out, ##出力データ
            "../submit/submit_20171018_2_logi.csv", ##出力先
            quote=FALSE, ##文字列を「"」で囲む有無
            col.names=FALSE, ##変数名(列名)の有無
            row.names=FALSE, ##行番号の有無
            sep="," ##区切り文字の指定
)



######参考：caretによるダミー変数化
library(caret)

dummy <- dummyVars(~., data=train)
train_dummy<- as.data.frame(predict(dummy, train))


## memo
train$category <- 'unknown'
train$category[train$job %in% c('admin.','self-employed','entrepreneur')] <- 'capitalist'
train$category[train$job %in% c('management','blue-collar','services','technician','housemaid')] <- 'worker'
train$category[train$job %in% c('retired','student','unemployed')] <- 'not-worker'
train$category <- as.factor(train$category)
summary(train$category)
check<-train %>%
  ## jobごとの対数オッズを計算
  dplyr::group_by(category) %>%
  dplyr::summarise(p=mean(y)) %>%  ####p反応率
  dplyr::ungroup(.) %>%
  dplyr::mutate(log_odds=log(p/(1-p)))

##外れ値を丸める
##外れ値の確認
hist(train2$balance)

##パーセンタイル点の確認
quantile(train2$balance,probs=c(.05,.95))

##丸め(ifelse(条件式, 真のとき返す値, 偽のとき返す値))上のみ
train2<-train2 %>%
  dplyr::mutate(balance2=ifelse(balance >= quantile(balance,probs=.95),
                                quantile(balance,probs=.95),
                                balance))
##外れ値の確認
hist(train2$balance2)

##相関係数の確認
cor(train2[,c("age2", "duration3")])
#### 小さな負の相関がある。小さいので同時に入れても問題なし
