##使用ライブラリ
library(pROC)
library(dplyr)
library(ggplot2)

##データ読込
train<-read.csv("../motodata/train.csv", header=T)
test<-read.csv("../motodata/test.csv", header=T)

## Customerを分割. Old
train_old <- train %>%
    dplyr::filter(pdays > -1)
test_old <- test %>%
    dplyr::filter(pdays > -1)

## 前キャンペーンの日付求める
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")

train_old <- train_old %>%
    mutate(pdate = as.integer(as.Date(paste(day,month,"2017",sep=""),"%d%b%Y")))

## 関係ありそう
table(train_old$pdate, train_old$y)

test_old <- test_old %>%
    mutate(pdate = as.integer(as.Date(paste(day,month,"2017",sep=""),"%d%b%Y")))

Sys.setlocale("LC_TIME", lct)

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
    y ~ pdate,
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
train_old <- train_old%>%
  dplyr::mutate(duration2=ifelse(duration >= quantile(duration,probs=.995),
                                quantile(duration,probs=.995),
                                duration))
##変数加工(duration2のsqrtで変換)
train_old2 <- train_old %>%
    dplyr::mutate(duration3 = sqrt(duration2)) %>%
    dplyr::select(-c(duration,duration2))

## age, balanceの加工を入れる。
train_old2 <- train_old2 %>%
    dplyr::mutate(age2=abs(50-age)) %>%
    dplyr::mutate(balance2=ifelse(balance >= quantile(balance,probs=.95),
                                  quantile(balance,probs=.95),
                                  balance)) %>%
    dplyr::select(-c(age,balance))
            
######新変数でロジスティック回帰モデル構築
##Hold Out(先ほどと同じ分け方)

## 不要カラムの排除
train_old2 <- train_old2 %>%
    dplyr::select('-id')

train_train_old2<-train_old2[row,]
train_test_old2<-train_old2[-row,]

logi_model_min <- glm(
    y ~ 1,
    data=train_train_old2,             ##学習データ 全部使う!!
    family=binomial(link="logit") ##ロジスティック回帰を指定
)

step.logi_model_min <- step(logi_model_min, direction='both',
                       scope=(~ pdate + age2 + job + marital + default + balance2 + housing + loan + contact + month + duration3 + campaign + pdays + previous + poutcome))

summary(step.logi_model_min)

logi_model2 <- glm(
    y ~ .,    ##目的変数と説明変数の指定(全て使う場合はy~.)
    data=train_train_old2,             ##学習データ
    family=binomial(link="logit") ##ロジスティック回帰を指定
)

logi_model2 <- glm(formula = y ~ poutcome + duration3 + month + housing + campaign + 
    pdate + loan + marital + contact, family = binomial(link = "logit"), 
    data = train_train_old2)

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
    y ~ poutcome + duration3 + month + housing + campaign + pdate + loan + marital + contact,
    data=train_old2,             ##学習データ 全部使う!!
    family=binomial(link="logit") ##ロジスティック回帰を指定
)

summary(logi_model3)
pred_test_old <- predict(logi_model3, newdata=test_old2, type="response")

## dataframeの保存
test_old$pred <- pred_test_old
save(test_old, file="test_old.dat")



