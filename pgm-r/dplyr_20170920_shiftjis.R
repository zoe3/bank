#使用ライブラリ
library(dplyr)

####関数の連結
#サンプルデータ
temp1<-data.frame(x=1:100, y=201:300, z=101:200)
print(temp1)

#1
sub1<-subset(temp1, x > 40)

#2 chain構文。第一引数に前の構文を入れてくれる。
sub2<-temp1 %>% subset(x>40)

#3 chain構文は改行することが多い
sub3<-temp1 %>%
  subset(x>40)

#4 dplyr::は明示している。
sub4<-temp1 %>%
  dplyr::filter(x>40)

###x>50のyの平均(y_mean)とzの中央値(z_median)のデータを作りたい
#未使用の例
sub<-subset(temp1, x>50)
y_mean<-mean(sub[,"y"])
z_median<-median(sub[,"z"])
dat1<-data.frame(y_mean=y_mean, z_median=z_median)

#使用の例
statistics<-temp1 %>%
  dplyr::filter(x>50) %>%
  dplyr::summarise(y_mean=mean(y), z_median=median(z))

## dplyrを利用するとchain構文で次々と流す。
## 中間ファイルを作らない。
