##使用ライブラリ
library(pROC)
library(dplyr)
library(ggplot2)

library(gridExtra)

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
    mutate(pdate = as.Date(paste(day,month,"2017",sep=""),"%d%b%Y"))
##    mutate(pdate = as.integer(as.Date(paste(day,month,"2017",sep=""),"%d%b%Y")))

test_old <- test_old %>%
    mutate(pdate = as.Date(paste(day,month,"2017",sep=""),"%d%b%Y"))
##    mutate(pdate = as.integer(as.Date(paste(day,month,"2017",sep=""),"%d%b%Y")))

Sys.setlocale("LC_TIME", lct)

str(train_old$pdate)

table(train_old$pdate, train_old$y)

table(train_old$pdate, train_old$poutcome)

table(train_old$pdate, train_old$job)

train_old %>%
    dplyr::select(c('pdate','job')) %>%
    dplyr::group_by(pdate,job) %>%
    dplyr::summarise(count=n()) %>%
    dplyr::ungroup(.) %>%
    ggplot(., aes(x=pdate, y=count, colour=job)) + geom_line()

## 前キャンペーンにアプローチした数
g <- train_old %>%
    dplyr::select(c('pdate','job','poutcome')) %>%
    dplyr::group_by(pdate,job) %>%
    dplyr::summarise(count=n()) %>%
    dplyr::ungroup(.) %>%
    glimpse

## 前キャンペーンでsuccessだった数
g <- train_old %>%
    dplyr::filter(poutcome == 'success') %>%
    dplyr::select(c('pdate','job','poutcome')) %>%
    dplyr::group_by(pdate,job) %>%
    dplyr::summarise(count=n()) %>%
    dplyr::ungroup(.) %>%
    glimpse


ggplot(data=g, aes(x=pdate, y=count, colour=job)) +
    geom_line()

ggplot(data=g, aes(x=pdate, y=count, colour=job)) +
    xlim(as.Date("2017-01-15"),as.Date("2017-02-20")) +
    geom_line()

ggplot(data=g, aes(x=pdate, y=count, colour=job)) +
    xlim(as.Date("2017-04-10"),as.Date("2017-05-25")) +
    geom_line()

ggplot(data=g, aes(x=pdate, y=count, colour=job)) +
    xlim(as.Date("2017-11-10"),as.Date("2017-12-01")) +
    geom_line()


chart1 <- ggplot(data=g, aes(x=pdate, y=count, colour=job)) +
    geom_line() +
    xlim(as.Date("2017-01-15"),as.Date("2017-02-20")) +
    facet_grid(~job)
chart2 <- ggplot(data=g, aes(x=pdate, y=count, colour=job)) +
    geom_line() +
    xlim(as.Date("2017-04-10"),as.Date("2017-05-25")) +
    facet_grid(~job) 
chart3 <- ggplot(data=g, aes(x=pdate, y=count, colour=job)) +
    geom_line() +
    xlim(as.Date("2017-11-10"),as.Date("2017-12-01")) +
    facet_grid(~job)

## まとめて1枚に出力
grid.arrange(chart1, chart2, chart3, ncol = 1)



## 顧客情報の観察
p <- ggplot(train, aes(x=age, y=balance))
p + geom_point(aes(colour=y))

g <- train %>%
    dplyr::group_by(job, contact, housing) %>%
    dplyr::summarise(m=mean(y), count=n()) %>%
    dplyr::ungroup(.) 

    dplyr::arrange(desc(m)) %>%


df = as.data.frame(g)

    
