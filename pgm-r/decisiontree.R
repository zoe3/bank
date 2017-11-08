# R_kiso_20170920.html
train <- read.csv('../motodata/train.csv')

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

## duration model
fit <- rpart(y ~ duration, data=train, method="class")
fancyRpartPlot(fit)



