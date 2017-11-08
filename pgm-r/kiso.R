# R_kiso_20170920.html
train <- read.csv('../motodata/train.csv')

head(train, n=10)

summary(train)

table(train$y)
prop.table(table(train$y))

hist(train$age)

table(train$job)

table(train$job, train$y)

age_c <- cut(train$age, breaks=c(0,20,30,40,50,60,100))
print(age_c)

table(age_c, train$y)

plot(train$marital)


演習
## Q1. trainデータのレコード数
str(train)
## 'data.frame':	27128 obs. of  18 variables:

## Q2. データ内の変数が量的変数/質的変数かを調べる
str(train)

## int 質的変数
## Factor 量的変数

## 'data.frame':	27128 obs. of  18 variables:
##  $ id       : int  1 2 3 4 5 6 7 8 9 10 ...
##  $ age      : int  39 51 36 63 31 29 37 32 31 32 ...
##  $ job      : Factor w/ 12 levels "admin.","blue-collar",..: 2 3 5 6 5 2 8 10 1 7 ...
##  $ marital  : Factor w/ 3 levels "divorced","married",..: 2 2 3 2 3 3 2 3 3 3 ...
##  $ education: Factor w/ 4 levels "primary","secondary",..: 2 1 3 2 3 2 2 2 2 3 ...
##  $ default  : Factor w/ 2 levels "no","yes": 1 1 1 1 1 1 1 1 1 1 ...
##  $ balance  : int  1756 1443 436 474 354 260 52 230 0 1815 ...
##  $ housing  : Factor w/ 2 levels "no","yes": 2 1 1 1 1 2 2 2 2 1 ...
##  $ loan     : Factor w/ 2 levels "no","yes": 1 1 1 1 1 1 1 1 1 1 ...
##  $ contact  : Factor w/ 3 levels "cellular","telephone",..: 1 1 1 1 1 3 1 1 1 2 ...
##  $ day      : int  3 18 13 25 30 2 6 18 7 10 ...
##  $ month    : Factor w/ 12 levels "apr","aug","dec",..: 1 4 1 5 1 7 12 9 9 2 ...
##  $ duration : int  939 172 567 423 502 707 908 442 895 235 ...
##  $ campaign : int  1 10 1 1 1 14 1 1 2 1 ...
##  $ pdays    : int  -1 -1 595 -1 9 -1 185 266 295 102 ...
##  $ previous : int  0 0 2 0 2 0 9 8 2 2 ...
##  $ poutcome : Factor w/ 4 levels "failure","other",..: 4 4 1 4 3 4 3 1 1 1 ...
##  $ y        : int  1 1 1 1 1 1 1 1 1 1 ...


## Q3.欠損はあるか？
summary(train)

カラム	ヘッダ名称	説明	変数種別
1	id	行の通し番号	
2	age	年齢	数値
3	job	職種	カテゴリ
4	martial	未婚/既婚	カテゴリ

##  $ id       : なし
##  $ age      : なし
##  $ job      : ある。unknown
##  $ marital  : なし

unique(train$job)
 [1] blue-collar   entrepreneur  management    retired       services     
 [6] technician    admin.        self-employed housemaid     unemployed   
[11] unknown       student      
12 Levels: admin. blue-collar entrepreneur housemaid management ... unknown

 ##       id             age                 job           marital     
 ## Min.   :    1   Min.   :18.00   blue-collar:5886   divorced: 3055  
 ## 1st Qu.: 6783   1st Qu.:33.00   management :5620   married :16411  
 ## Median :13564   Median :39.00   technician :4491   single  : 7662  
 ## Mean   :13564   Mean   :40.95   admin.     :3085                   
 ## 3rd Qu.:20346   3rd Qu.:48.00   services   :2506                   
 ## Max.   :27128   Max.   :95.00   retired    :1391                   
 ##                                 (Other)    :4149                   


5	education	教育水準	カテゴリ
6	default	債務不履行があるか	バイナリ
7	balance	年間平均残高	数値(€)
8	housing	住宅ローン	バイナリ
9	loan	個人ローン	バイナリ

##  $ education: ある。unknown
##  $ default  : なし
##  $ balance  : ある。年間平均残高がマイナスってあるのか？
##  $ housing  : なし
##  $ loan     : なし

hist(train$balance)

##     education     default        balance       housing      loan      
 ## primary  : 4150   no :26644   Min.   : -6847   no :12003   no :22788  
 ## secondary:13882   yes:  484   1st Qu.:    72   yes:15125   yes: 4340  
 ## tertiary : 7959               Median :   449                          
 ## unknown  : 1137               Mean   :  1356                          
 ##                               3rd Qu.:  1428                          
 ##                               Max.   :102127                          

10	contact	連絡方法	カテゴリ
11	day	最終接触日	数値
12	month	最終接触月	カテゴリ
13	duration	最終接触時間	数値(秒）

##  $ contact  : ある。unknown
##  $ day      : なし
##  $ month    : なし
##  $ duration : なし

unique(train$month)
> unique(train$month)
 [1] apr feb jan jun sep may aug mar jul nov oct dec
Levels: apr aug dec feb jan jul jun mar may nov oct sep

hist(train$duration)

 ##      contact           day            month         duration     
 ## cellular :17580   Min.   : 1.00   may    :8317   Min.   :   0.0  
 ## telephone: 1687   1st Qu.: 8.00   jul    :4136   1st Qu.: 104.0  
 ## unknown  : 7861   Median :16.00   aug    :3718   Median : 182.0  
 ##                   Mean   :15.81   jun    :3204   Mean   : 260.7  
 ##                   3rd Qu.:21.00   nov    :2342   3rd Qu.: 323.0  
 ##                   Max.   :31.00   apr    :1755   Max.   :4918.0  
 ##                                   (Other):3656                   

14	compaign	現キャンペーンにおける接触回数	数値
15	pdays	経過日数：前キャンペーン接触後の日数	数値
16	previous	接触実績：現キャンペーン以前までに顧客に接触した回数	数値
17	poutcome	前回のキャンペーンの成果	カテゴリ

##  $ campaign : なし
##  $ pdays    : ある。-1
##  $ previous : なし
##  $ poutcome : ある unknown
                                                                       
 ##    campaign          pdays           previous           poutcome    
 ## Min.   : 1.000   Min.   : -1.00   Min.   :  0.0000   failure: 2969  
 ## 1st Qu.: 1.000   1st Qu.: -1.00   1st Qu.:  0.0000   other  : 1123  
 ## Median : 2.000   Median : -1.00   Median :  0.0000   success:  886  
 ## Mean   : 2.752   Mean   : 40.53   Mean   :  0.5797   unknown:22150  
 ## 3rd Qu.: 3.000   3rd Qu.: -1.00   3rd Qu.:  0.0000                  
 ## Max.   :63.000   Max.   :871.00   Max.   :275.0000                  


18	y	定額預金申し込み有無	バイナリ（1／０）
##  $ y        : なし

unique(train$y)

 ##       y        
 ## Min.   :0.000  
 ## 1st Qu.:0.000  
 ## Median :0.000  
 ## Mean   :0.117  
 ## 3rd Qu.:0.000  
 ## Max.   :1.000  

## Q4. balance > 10000の%?
nrow(subset(train, balance>10000))/nrow(train)

## balance_c <- cut(train$balance, breaks=c(0,10000,102128))
## table(balance_c, train$y)
## prop.table(table(balance_c, train$y))

## Q5. contact=cellularの%?
nrow(subset(train, contact=="cellular"))/nrow(train)

## Q6. Q4,Q5の人の申込率(y=1)の割合
## Anser 0.002285462 (yes & y=1)

train$flg <- 'no'
train$flg[train$balance > 10000 & train$contact=='cellular'] <- 'yes'
prop.table(table(train$flg, train$y), margin=NULL)

> train$flg <- 'no'
> train$flg[train$balance > 10000 & train$contact=='cellular'] <- 'yes'
> prop.table(table(train$flg, train$y), margin=NULL)
     
                0           1
  no  0.872640814 0.114715423
  yes 0.010358301 0.002285462

## Q7. Q6の集計表やグラフによる可視化
集計表
> prop.table(table(train$flg, train$y), margin=NULL)
     
                0           1
  no  0.872640814 0.114715423
  yes 0.010358301 0.002285462

table(train$flg, train$y)

          0     1
  no  23673  3112
  yes   281    62


## グラフ
## X,Yともに離散値
library('ggplot2')

str(factor(train$y))
train$y <- factor(train$y)
str(train$y)

## g+geom_count()
ggplot(train, aes(x=flg, y=y)) +
    geom_count()
