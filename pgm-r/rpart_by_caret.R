library(dplyr)
library(caret)
set.seed(12345)

train <- read.csv("../motodata/train.csv")
train$y <- factor(train$y, label=c("neg","pos"))

cvControl <- trainControl(
    method = "cv",                          # cross validation
    number = 10,                            # 10分割して検証
    classProbs = TRUE,                      # 予測値として確率を得たい場合にTRUE
    summaryFunction = twoClassSummary       # ２値分類の場合にこのように指定
)

grid <- expand.grid(
    cp=c(0, 0.0001, 0.0002, 0.0003, 0.0004, 0.0005)
)

model <- train(
    data = train,
    y ~ .,
    method='rpart',         # アルゴリズムを指定：今回はrpart
    trControl=cvControl, 
    tuneGrid=grid,
    metric = "ROC"         # 確認したい精度指標:今回はROC     
)

