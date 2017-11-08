##使用ライブラリ
library(pROC)
library(dplyr)
library(ggplot2)

##データ読込
train<-read.csv("../motodata/train.csv", header=T)
test<-read.csv("../motodata/test.csv", header=T)

fit <- glm(y ~ duration,
    family = binomial(link = "logit"),
    data = train)

mean(train$y)

Call:  glm(formula = y ~ duration, family = binomial(link = "logit"), 
    data = train)

Coefficients:
(Intercept)     duration  
  -3.217936     0.003562  

Degrees of Freedom: 27127 Total (i.e. Null);  27126 Residual
Null Deviance:	    19580 
Residual Deviance: 16400 	AIC: 16400
> 
