#�g�p���C�u����
library(dplyr)

####�֐��̘A��
#�T���v���f�[�^
temp1<-data.frame(x=1:100, y=201:300, z=101:200)
print(temp1)

#1
sub1<-subset(temp1, x > 40)

#2 chain�\���B�������ɑO�̍\�������Ă����B
sub2<-temp1 %>% subset(x>40)

#3 chain�\���͉��s���邱�Ƃ�����
sub3<-temp1 %>%
  subset(x>40)

#4 dplyr::�͖������Ă���B
sub4<-temp1 %>%
  dplyr::filter(x>40)

###x>50��y�̕���(y_mean)��z�̒����l(z_median)�̃f�[�^����肽��
#���g�p�̗�
sub<-subset(temp1, x>50)
y_mean<-mean(sub[,"y"])
z_median<-median(sub[,"z"])
dat1<-data.frame(y_mean=y_mean, z_median=z_median)

#�g�p�̗�
statistics<-temp1 %>%
  dplyr::filter(x>50) %>%
  dplyr::summarise(y_mean=mean(y), z_median=median(z))

## dplyr�𗘗p�����chain�\���Ŏ��X�Ɨ����B
## ���ԃt�@�C�������Ȃ��B