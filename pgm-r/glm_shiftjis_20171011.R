##�g�p���C�u����
library(pROC)
library(dplyr)
library(ggplot2)

##�f�[�^�Ǎ�
train<-read.csv("../motodata/train.csv", header=T)
test<-read.csv("../motodata/test.csv", header=T)

##���x�m�F�̂���Hold Out
rate<-0.7
num<-as.integer(nrow(train)*rate)
set.seed(17)
row<-sample(1:nrow(train), num, replace=FALSE)

train_train<-train[row,]
train_test<-train[-row,]

######���W�X�e�B�b�N��A���f���\�z
logi_model <- glm(
    y ~ duration + housing + loan,    ##�ړI�ϐ��Ɛ����ϐ��̎w��(�S�Ďg���ꍇ��y~.)
    data=train_train,             ##�w�K�f�[�^
    family=binomial(link="logit") ##���W�X�e�B�b�N��A���w��.�񍀕��z�ɏ]���B
)

##���f���̒��g������
summary(logi_model)

##���f���̐��x�m�F
##���f���̓��Ă͂�
pred_train_test<- predict(logi_model, newdata=train_test, type="response") ####����؂ƈႢ�Ԃ��l�������Ă���B

##AUC�m�F
auc<-roc(train_test$y, pred_train_test)$auc
auc


######�ϐ����H
##���`���̊m�F
summary(train$duration)

##�O��l�̊m�F
hist(train$duration)

##�p�[�Z���^�C���_�̊m�F
quantile(train$duration,probs=c(.05,.95))

##�ۂ�(ifelse(������, �^�̂Ƃ��Ԃ��l, �U�̂Ƃ��Ԃ��l))��̂�
train<-train %>%
  dplyr::mutate(duration2=ifelse(duration >= quantile(duration,probs=.95),
                                quantile(duration,probs=.95),
                                duration))
##�O��l�̊m�F
hist(train$duration2)

check<-train %>%
    ## duration2���Ƃ̑ΐ��I�b�Y���v�Z
    dplyr::mutate(duration_c = floor(duration2/50)*50) %>%
    dplyr::group_by(duration_c) %>%
    dplyr::summarise(p=mean(y)) %>%  ####p������
    dplyr::ungroup(.) %>%
    dplyr::mutate(log_odds=log(p/(1-p)))

##�O���t�̏o��
g<-ggplot(check, aes(x=duration_c, y=log_odds)) + geom_line()
plot(g)

##�ϐ����H(duration2��sqrt�ŕϊ�)
train2 <- train %>%
    dplyr::mutate(duration3 = sqrt(duration2)) %>%
    dplyr::select(-c(duration,duration2))

## duration3�̕������镝�̌���
hist(train2$duration3)

##�Ăѐ��`���̊m�F
check<-train2 %>%
    ## duration2���Ƃ̑ΐ��I�b�Y���v�Z
    dplyr::mutate(duration_c = floor(duration3/5)*5) %>%
    dplyr::group_by(duration_c) %>%
    dplyr::summarise(p=mean(y)) %>%  ####p������
    dplyr::ungroup(.) %>%
    dplyr::mutate(log_odds=log(p/(1-p)))

##�O���t�̏o��
g<-ggplot(check, aes(x=duration_c, y=log_odds)) + geom_line()
plot(g)

## age, balance�̉��H������B
train2 <- train2 %>%
    dplyr::mutate(age2=abs(50-age)) %>%
    dplyr::mutate(balance2=ifelse(balance >= quantile(balance,probs=.95),
                                  quantile(balance,probs=.95),
                                  balance)) %>%
    dplyr::select(-c(age,balance))


######�V�ϐ��Ń��W�X�e�B�b�N��A���f���\�z
##Hold Out(��قǂƓ���������)
train_train2<-train2[row,]
train_test2<-train2[-row,]

logi_model2 <- glm(
    y ~ duration3 + age2 + balance2 + housing + loan,    ##�ړI�ϐ��Ɛ����ϐ��̎w��(�S�Ďg���ꍇ��y~.)
    data=train_train2,             ##�w�K�f�[�^
    family=binomial(link="logit") ##���W�X�e�B�b�N��A���w��
)

##���f���̒��g������
summary(logi_model2)

##���f���̐��x�m�F
##���f���̓��Ă͂�
pred_train_test<- predict(logi_model2, newdata=train_test2, type="response")

##AUC�m�F
auc<-roc(train_test$y, pred_train_test)$auc
auc

check <- data.frame(train_test, pred_train_test)

check %>%
    dplyr::arrange(desc(pred_train_test)) %>%
    head(100) %>%
    dplyr::summarise(sum(y))



######Submit
##test�ɂ�train�Ɠ��l�̉��H������train�S�̂Ń��f���\�z����test�ɓK�p
test2 <- test %>%
  dplyr::mutate(age2=abs(50-age),
                balance2=ifelse(balance >= quantile(balance,probs=.95),
                                quantile(balance,probs=.95),
                                balance)) %>%
  dplyr::mutate(duration3=sqrt(ifelse(duration >= quantile(duration,probs=.95),
                                      quantile(duration,probs=.95),
                                      duration)))

#### train��95%tail�ł܂Ƃ߂�����ǂ�

logi_model3 <- glm(
    y ~ duration3 + age2 + balance2 + housing + loan,    ##�ړI�ϐ��Ɛ����ϐ��̎w��(�S�Ďg���ꍇ��y~.)
    data=train2,             ##�w�K�f�[�^ �S���g��!!
    family=binomial(link="logit") ##���W�X�e�B�b�N��A���w��
)


pred_test <- predict(logi_model3, newdata=test2, type="response")

##submit�̌`���ŏo��(CSV)
##�f�[�^���H
out<-data.frame(test2$id, pred_test)

##�o��
write.table(out, ##�o�̓f�[�^
            "../submit/submit_20171011_1_logi.csv", ##�o�͐�
            quote=FALSE, ##��������u"�v�ň͂ޗL��
            col.names=FALSE, ##�ϐ���(��)�̗L��
            row.names=FALSE, ##�s�ԍ��̗L��
            sep="," ##��؂蕶���̎w��
)


######�Q�l�F�X�e�b�v���C�Y�@
logi_model_all <- glm(
  y ~ .,    ##�ړI�ϐ��Ɛ����ϐ��̎w��(�S�Ďg���ꍇ��y~.)
  data=train2,             ##�w�K�f�[�^
  family=binomial(link="logit") ##���W�X�e�B�b�N��A���w��
)

##�X�e�b�v���C�Y
step.model_all <- step(logi_model_all)

##�I�����ꂽ�ϐ��̊m�F
summary(step.model_all)


## Call:
## glm(formula = y ~ job + marital + education + housing + loan + 
##     contact + day + month + campaign + poutcome + duration3, 
##     family = binomial(link = "logit"), data = train2)
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -3.0411  -0.3624  -0.2006  -0.1057   3.5751  

logi_model4 <- glm(
    y ~ job + marital + education + housing + loan + contact + day + month + campaign + poutcome + duration3,
    data=train2,             ##�w�K�f�[�^ �S���g��!!
    family=binomial(link="logit") ##���W�X�e�B�b�N��A���w��
)

pred_test <- predict(logi_model4, newdata=test2, type="response")

##submit�̌`���ŏo��(CSV)
##�f�[�^���H
out<-data.frame(test2$id, pred_test)

##�o��
write.table(out, ##�o�̓f�[�^
            "../submit/submit_20171011_2_logi.csv", ##�o�͐�
            quote=FALSE, ##��������u"�v�ň͂ޗL��
            col.names=FALSE, ##�ϐ���(��)�̗L��
            row.names=FALSE, ##�s�ԍ��̗L��
            sep="," ##��؂蕶���̎w��
)


######�Q�l�Fcaret�ɂ��_�~�[�ϐ���
library(caret)

dummy <- dummyVars(~., data=train)
train_dummy<- as.data.frame(predict(dummy, train))


## memo
train$category <- 'unknown'
train$category[train$job %in% c('admin.','self-employed','entrepreneur')] <- 'capitalist'
train$category[train$job %in% c('management','blue-collar','services','technician','housemaid')] <- 'worker'
train$category[train$job %in% c('retired','student','unemployed')] <- 'not-worker'
train$category <- as.factor(train$category)
summary(train$category)
check<-train %>%
  ## job���Ƃ̑ΐ��I�b�Y���v�Z
  dplyr::group_by(category) %>%
  dplyr::summarise(p=mean(y)) %>%  ####p������
  dplyr::ungroup(.) %>%
  dplyr::mutate(log_odds=log(p/(1-p)))

##�O��l���ۂ߂�
##�O��l�̊m�F
hist(train2$balance)

##�p�[�Z���^�C���_�̊m�F
quantile(train2$balance,probs=c(.05,.95))

##�ۂ�(ifelse(������, �^�̂Ƃ��Ԃ��l, �U�̂Ƃ��Ԃ��l))��̂�
train2<-train2 %>%
  dplyr::mutate(balance2=ifelse(balance >= quantile(balance,probs=.95),
                                quantile(balance,probs=.95),
                                balance))
##�O��l�̊m�F
hist(train2$balance2)

##���֌W���̊m�F
cor(train2[,c("age2", "duration3")])
#### �����ȕ��̑��ւ�����B�������̂œ����ɓ���Ă����Ȃ�