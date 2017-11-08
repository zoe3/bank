#�g�p���C�u����
library(pROC)
library(dplyr)
library(ggplot2)

#�f�[�^�Ǎ�
train<-read.csv("../motodata/train.csv", header=T)
test<-read.csv("../motodata/test.csv", header=T)

#���x�m�F�̂���Hold Out
rate<-0.7
num<-as.integer(nrow(train)*rate)
set.seed(17)
row<-sample(1:nrow(train), num, replace=FALSE)

train_train<-train[row,]
train_test<-train[-row,]

###���W�X�e�B�b�N��A���f���\�z
logi_model <- glm(
  y ~ age+balance,    #�ړI�ϐ��Ɛ����ϐ��̎w��(�S�Ďg���ꍇ��y~.)
  data=train_train,             #�w�K�f�[�^
  family=binomial(link="logit") #���W�X�e�B�b�N��A���w��.�񍀕��z�ɏ]���B
)

#���f���̒��g������
summary(logi_model)

#���f���̐��x�m�F
#���f���̓��Ă͂�
pred_train_test<- predict(logi_model, newdata=train_test, type="response") ##����؂ƈႢ�Ԃ��l�������Ă���B

#AUC�m�F
auc<-roc(train_test$y, pred_train_test)$auc
auc


###�ϐ����H
#���`���̊m�F
check<-train %>%
  #�N�ゲ�Ƃ̑ΐ��I�b�Y���v�Z
  dplyr::mutate(age_c = floor(age/10)*10) %>%
  dplyr::group_by(age_c) %>%
  dplyr::summarise(p=mean(y)) %>%  ##p������
  dplyr::ungroup(.) %>%
  dplyr::mutate(log_odds=log(p/(1-p)))

#�O���t�̏o��
g<-ggplot(check, aes(x=age_c, y=log_odds)) + geom_line()
plot(g)

#�ϐ����H(age��50�Ő܂�Ԃ��Ă݂�)
train2<-train %>%
  dplyr::mutate(age2=abs(50-age))

#�Ăѐ��`���̊m�F
check<-train2 %>%
  #age2�ł̔N�ゲ�Ƃ̑ΐ��I�b�Y���v�Z
  dplyr::mutate(age_c = floor(age2/10)*10) %>%
  dplyr::group_by(age_c) %>%
  dplyr::summarise(p=mean(y)) %>%
  dplyr::ungroup(.) %>%
  dplyr::mutate(log_odds=log(p/(1-p)))

#�O���t�̏o��
g<-ggplot(check, aes(x=age_c, y=log_odds)) + geom_line()
plot(g)

#�O��l���ۂ߂�
#�O��l�̊m�F
hist(train2$balance)

#�p�[�Z���^�C���_�̊m�F
quantile(train2$balance,probs=c(.05,.95))

#�ۂ�(ifelse(������, �^�̂Ƃ��Ԃ��l, �U�̂Ƃ��Ԃ��l))��̂�
train2<-train2 %>%
  dplyr::mutate(balance2=ifelse(balance >= quantile(balance,probs=.95),
                                quantile(balance,probs=.95),
                                balance))
#�O��l�̊m�F
hist(train2$balance2)

#���֌W���̊m�F
cor(train2[,c("age2", "balance2")])
## �����ȕ��̑��ւ�����B�������̂œ����ɓ���Ă����Ȃ�

###�V�ϐ��Ń��W�X�e�B�b�N��A���f���\�z
#Hold Out(��قǂƓ���������)
train_train2<-train2[row,]
train_test2<-train2[-row,]

logi_model2 <- glm(
  y ~ age2+balance2,    #�ړI�ϐ��Ɛ����ϐ��̎w��(�S�Ďg���ꍇ��y~.)
  data=train_train2,             #�w�K�f�[�^
  family=binomial(link="logit") #���W�X�e�B�b�N��A���w��
)

#���f���̒��g������
summary(logi_model2)

#���f���̐��x�m�F
#���f���̓��Ă͂�
pred_train_test<- predict(logi_model2, newdata=train_test2, type="response")

#AUC�m�F
auc<-roc(train_test$y, pred_train_test)$auc
auc


###Submit
#test�ɂ�train�Ɠ��l�̉��H������train�S�̂Ń��f���\�z����test�ɓK�p
test2<-test %>%
  dplyr::mutate(age2=abs(50-age),
                balance2=ifelse(balance >= quantile(balance,probs=.95),
                                quantile(balance,probs=.95),
                                balance))
## train��95%tail�ł܂Ƃ߂�����ǂ�

logi_model3 <- glm(
  y ~ age2+balance2,    #�ړI�ϐ��Ɛ����ϐ��̎w��(�S�Ďg���ꍇ��y~.)
  data=train2,             #�w�K�f�[�^ �S���g��!!
  family=binomial(link="logit") #���W�X�e�B�b�N��A���w��
)


pred_test <- predict(logi_model3, newdata=test2, type="response")

#submit�̌`���ŏo��(CSV)
#�f�[�^���H
out<-data.frame(test2$id, pred_test)

#�o��
write.table(out, #�o�̓f�[�^
            "C:/bank/submit/submit_0530_1_logi.csv", #�o�͐�
            quote=FALSE, #��������u"�v�ň͂ޗL��
            col.names=FALSE, #�ϐ���(��)�̗L��
            row.names=FALSE, #�s�ԍ��̗L��
            sep="," #��؂蕶���̎w��
)


###�Q�l�F�X�e�b�v���C�Y�@
logi_model_all <- glm(
  y ~ .,    #�ړI�ϐ��Ɛ����ϐ��̎w��(�S�Ďg���ꍇ��y~.)
  data=train,             #�w�K�f�[�^
  family=binomial(link="logit") #���W�X�e�B�b�N��A���w��
)

#�X�e�b�v���C�Y
step.model_all <- step(logi_model_all)

#�I�����ꂽ�ϐ��̊m�F
summary(step.model_all)


###�Q�l�Fcaret�ɂ��_�~�[�ϐ���
library(caret)

dummy <- dummyVars(~., data=train)
train_dummy<- as.data.frame(predict(dummy, train))
