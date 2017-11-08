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

## > table(train$previous, train$poutcome)
##       failure other success unknown
##   0         0     0       0   22146
##   1      1041   363     279       1
##   2       816   243     213       1
##   3       407   139     143       0
##   4       251   110      84       0
##   5       161    58      44       1
##   6        85    45      36       0
##   7        59    31      26       1
  

## カラム追加
train$data <- "train"
test$data <- "test"
test$y <- NA

combi <- rbind(train,test)

combi$age2 <- '53+'
combi$age2[combi$age < 31] <- '<31'
combi$age2[combi$age < 35 & combi$age >= 31] <- '31-34'
combi$age2[combi$age < 40 & combi$age >= 35] <- '35-39'
combi$age2[combi$age < 46 & combi$age >= 40] <- '40-45'
combi$age2[combi$age < 53 & combi$age >= 46] <- '46-52'

combi$age2 <- as.factor(combi$age2)

aggregate(duration ~ job, data=combi, FUN=function(x) {sum(x)/length(x)})
aggregate(duration ~ job, data=combi, FUN=function(x) {var(x)})

combi$category <- 'unknown'
combi$category[combi$job %in% c('admin.','self-employed','entrepreneur')] <- 'capitalist'
combi$category[combi$job %in% c('management','blue-collar','services','technician','housemaid')] <- 'worker'
combi$category[combi$job %in% c('retired','student','unemployed')] <- 'not-worker'
combi$category <- as.factor(combi$category)

train <- combi %>%
    dplyr::filter(data=='train') %>%
    dplyr::select(-data)

test <- combi %>%
    dplyr::filter(data=='test') %>%
dplyr::select(-data)

##決定木モデル作成
#可視化のため目的変数の型をファクターに変換
train$y<-as.factor(train$y)

#決定木モデル作成
tree<-rpart(y ~ . - id, #目的変数~説明変数, 「.」はy以外の全変数を説明変数とするという意味
            data=train, #学習データ
            maxdepth=15, #階層数
            minbucket=10, #最小ノードサイズ
            cp=0.0001, #枝刈りの強さ
            method="class", #分類
            #parms=list(split="gini") #分割基準, list(split="information")とするとEntropy基準となる
            parms=list(split="information")
            )

#構築データでのAUC
#モデルの当てはめ
pred_train<-predict(tree, train)[,2]
#AUC
auc<-roc(train$y, pred_train)
print(auc)

##可視化
plot(as.party(tree))

## 可視化その2
plot(tree)
text(tree, use.n=TRUE, cex=0.5)
path.rpart(tree)

## cp統計量を確認
printcp(tree)
plotcp(tree)

##予測データに決定木モデルを適用
#モデルの当てはめ
pred_test<-predict(tree, test)[,2]

#submitの形式で出力(CSV)
#データ加工
out<-data.frame(test$id, pred_test)

#出力
write.table(out, #出力データ
            "../submit/submit_20171011_1_rpart.csv", #出力先
            quote=FALSE, #文字列を「"」で囲む有無
            col.names=FALSE, #変数名(列名)の有無
            row.names=FALSE, #行番号の有無
            sep="," #区切り文字の指定
)

