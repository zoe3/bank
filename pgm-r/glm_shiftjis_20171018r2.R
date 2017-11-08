##�g�p���C�u����
library(pROC)
library(dplyr)
library(ggplot2)

##�f�[�^�Ǎ�
train<-read.csv("../motodata/train.csv", header=T)
test<-read.csv("../motodata/test.csv", header=T)

## Customer�𕪊�. New Old
train_new <- train %>%
    dplyr::filter(pdays==-1) %>%
    dplyr::select(-c(pdays,previous,poutcome))
test_new <- test %>%
    dplyr::filter(pdays==-1) %>%
    dplyr::select(-c(pdays,previous,poutcome))

train_old <- train %>%
    dplyr::filter(pdays > -1)
test_old <- test %>%
    dplyr::filter(pdays > -1)

#### New
##���x�m�F�̂���Hold Out
rate<-0.7
num<-as.integer(nrow(train_new)*rate)
set.seed(17)
row<-sample(1:nrow(train_new), num, replace=FALSE)

train_train<-train_new[row,]
train_test<-train_new[-row,]

######���W�X�e�B�b�N��A���f���\�z
logi_model <- glm(
    y ~ .,
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
summary(train_new$duration)

hist(train_new$duration)

##�p�[�Z���^�C���_�̊m�F
quantile(train_new$duration,probs=c(.05,.95))
quantile(train_new$duration,probs=c(.05,.995))

##�ۂ�(ifelse(������, �^�̂Ƃ��Ԃ��l, �U�̂Ƃ��Ԃ��l))��̂�
train_new <- train_new%>%
  dplyr::mutate(duration2=ifelse(duration >= quantile(duration,probs=.995),
                                quantile(duration,probs=.995),
                                duration))

##�O��l�̊m�F
hist(train_new$duration2)

check <-train_new%>%
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
train_new2 <- train_new %>%
    dplyr::mutate(duration3 = sqrt(duration2)) %>%
    dplyr::select(-c(duration,duration2))

## duration3�̕������镝�̌���
hist(train_new2$duration3)

##�Ăѐ��`���̊m�F
check <-train_new2 %>%
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
train_new2 <- train_new2 %>%
    dplyr::mutate(age2=abs(50-age)) %>%
    dplyr::mutate(balance2=ifelse(balance >= quantile(balance,probs=.95),
                                  quantile(balance,probs=.95),
                                  balance)) %>%
    dplyr::select(-c(age,balance))


## ���d�������̊m�F
##���֌W���̊m�F
## �ڈ�
## |r| = 0.7 ~ 1 ���Ȃ苭�����ւ�����
## |r| = 0.4 ~ 0.7 ��⑊�ւ�����
## |r| = 0.2 ~ 0.4 �ア���ւ�����
## |r| = 0 ~ 0.2 �قƂ�Ǒ��ւȂ�

cor(train2[,c('duration3','pdays')])

cor(train2[,c('duration3','id')])

cor(train2[,c("age2", "duration3")])

cor(train2[,c("age2", "balance2")])

cor(train2[,c('pdays','previous')])

cor(train2[,c('campaign','previous')])

#### �����ȕ��̑��ւ�����B�������̂œ����ɓ���Ă����Ȃ�


hist(temp$pday)

toPoint <- function(factors) {
    mapping <- c("failure"=1, "other"=3, "success"=5, "unknown"=0)
    mapping[as.character(factors)]
}
train2$poutcome <- toPoint(train2$poutcome)
            
######�V�ϐ��Ń��W�X�e�B�b�N��A���f���\�z
##Hold Out(��قǂƓ���������)
train_new2 <- train_new2 %>%
    dplyr::select(-id)

train_train_new2<-train_new2[row,]
train_test_new2<-train_new2[-row,]

logi_model2 <- glm(
    y ~ .,    ##�ړI�ϐ��Ɛ����ϐ��̎w��(�S�Ďg���ꍇ��y~.)
    data=train_train_new2,             ##�w�K�f�[�^
    family=binomial(link="logit") ##���W�X�e�B�b�N��A���w��
)

##���f���̒��g������
summary(logi_model2)

##���f���̐��x�m�F
##���f���̓��Ă͂�
pred_train_test<- predict(logi_model2, newdata=train_test_new2, type="response")

o##AUC�m�F
auc<-roc(train_test$y, pred_train_test)$auc
auc


check <- data.frame(train_test, pred_train_test)

check %>%
    dplyr::arrange(desc(pred_train_test)) %>%
    head(100) %>%
    dplyr::summarise(sum(y))

######Submit
##test�ɂ�train�Ɠ��l�̉��H������train�S�̂Ń��f���\�z����test�ɓK�p
test_new2 <- test_new%>%
  dplyr::mutate(age2=abs(50-age),
                balance2=ifelse(balance >= quantile(balance,probs=.95),
                                quantile(balance,probs=.95),
                                balance)) %>%
  dplyr::mutate(duration3=sqrt(ifelse(duration >= quantile(duration,probs=.995),
                                      quantile(duration,probs=.995),
                                      duration)))

#### train��95%tail�ł܂Ƃ߂�����ǂ�

logi_model3_new <- glm(
    y ~ .,
    data=train_new2,             ##�w�K�f�[�^ �S���g��!!
    family=binomial(link="logit") ##���W�X�e�B�b�N��A���w��
)

summary(logi_model3_new)

pred_test_new <- predict(logi_model3_new, newdata=test_new2, type="response")

#### Old
##���x�m�F�̂���Hold Out
rate<-0.7
num<-as.integer(nrow(train_old)*rate)
set.seed(17)
row<-sample(1:nrow(train_old), num, replace=FALSE)

train_train_old <- train_old[row,]
train_test_old <- train_old[-row,]

######���W�X�e�B�b�N��A���f���\�z
logi_model <- glm(
    y ~ .,
    data=train_train_old,             ##�w�K�f�[�^
    family=binomial(link="logit") ##���W�X�e�B�b�N��A���w��.�񍀕��z�ɏ]���B
)

##���f���̒��g������
summary(logi_model)

##���f���̐��x�m�F
##���f���̓��Ă͂�
pred_train_test_old <- predict(logi_model, newdata=train_test_old, type="response") ####����؂ƈႢ�Ԃ��l�������Ă���B

##AUC�m�F
auc<-roc(train_test_old$y, pred_train_test_old)$auc
auc

######�ϐ����H
##���`���̊m�F
summary(train_old$duration)

hist(train_old$duration)

##�p�[�Z���^�C���_�̊m�F
quantile(train_old$duration,probs=c(.05,.95))
quantile(train_old$duration,probs=c(.05,.995))

##�ۂ�(ifelse(������, �^�̂Ƃ��Ԃ��l, �U�̂Ƃ��Ԃ��l))��̂�
train_old <- train_old%>%
  dplyr::mutate(duration2=ifelse(duration >= quantile(duration,probs=.995),
                                quantile(duration,probs=.995),
                                duration))

##�O��l�̊m�F
hist(train_old$duration2)

check <-train_old %>%
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
train_old2 <- train_old %>%
    dplyr::mutate(duration3 = sqrt(duration2)) %>%
    dplyr::select(-c(duration,duration2))

## duration3�̕������镝�̌���
hist(train_old2$duration3)

##�Ăѐ��`���̊m�F
check <-train_old2 %>%
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
train_old2 <- train_old2 %>%
    dplyr::mutate(age2=abs(50-age)) %>%
    dplyr::mutate(balance2=ifelse(balance >= quantile(balance,probs=.95),
                                  quantile(balance,probs=.95),
                                  balance)) %>%
    dplyr::select(-c(age,balance))


## ���d�������̊m�F
##���֌W���̊m�F
## �ڈ�
## |r| = 0.7 ~ 1 ���Ȃ苭�����ւ�����
## |r| = 0.4 ~ 0.7 ��⑊�ւ�����
## |r| = 0.2 ~ 0.4 �ア���ւ�����
## |r| = 0 ~ 0.2 �قƂ�Ǒ��ւȂ�

cor(train_old2[,c('duration3','pdays')])

cor(train_old2[,c('duration3','id')])

cor(train_old2[,c("age2", "duration3")])

cor(train_old2[,c("age2", "balance2")])

cor(train_old2[,c('pdays','previous')])

cor(train_old2[,c('campaign','previous')])

cor(train_old2[,c("previous", "balance2")])

cor(train_old2[,c("campaign", "balance2")])

cor(train_old2[,c("pdays", "balance2")])

cor(train_old2[,c("duration3", "balance2")])

#### �����ȕ��̑��ւ�����B�������̂œ����ɓ���Ă����Ȃ�

hist(temp$pday)

toPoint <- function(factors) {
    mapping <- c("failure"=1, "other"=3, "success"=5, "unknown"=0)
    mapping[as.character(factors)]
}
train2$poutcome <- toPoint(train2$poutcome)
            
######�V�ϐ��Ń��W�X�e�B�b�N��A���f���\�z
##Hold Out(��قǂƓ���������)
train_old2 <- train_old2 %>%
    dplyr::select(-id)

train_train_old2<-train_old2[row,]
train_test_old2<-train_old2[-row,]

logi_model2 <- glm(
    y ~ .,    ##�ړI�ϐ��Ɛ����ϐ��̎w��(�S�Ďg���ꍇ��y~.)
    data=train_train_old2,             ##�w�K�f�[�^
    family=binomial(link="logit") ##���W�X�e�B�b�N��A���w��
)

##���f���̒��g������
summary(logi_model2)

##���f���̐��x�m�F
##���f���̓��Ă͂�
pred_train_test_old <- predict(logi_model2, newdata=train_test_old2, type="response")

##AUC�m�F
auc<-roc(train_test_old$y, pred_train_test_old)$auc
auc

check <- data.frame(train_test_old, pred_train_test_old)

check %>%
    dplyr::arrange(desc(pred_train_test_old)) %>%
    head(100) %>%
    dplyr::summarise(sum(y))

######Submit
##test�ɂ�train�Ɠ��l�̉��H������train�S�̂Ń��f���\�z����test�ɓK�p
test_old2 <- test_old %>%
  dplyr::mutate(age2=abs(50-age),
                balance2=ifelse(balance >= quantile(balance,probs=.95),
                                quantile(balance,probs=.95),
                                balance)) %>%
  dplyr::mutate(duration3=sqrt(ifelse(duration >= quantile(duration,probs=.995),
                                      quantile(duration,probs=.995),
                                      duration)))

#### train��95%tail�ł܂Ƃ߂�����ǂ�

logi_model3 <- glm(
    y ~ .,
    data=train_old2,             ##�w�K�f�[�^ �S���g��!!
    family=binomial(link="logit") ##���W�X�e�B�b�N��A���w��
)

summary(logi_model3)

pred_test_old <- predict(logi_model3, newdata=test_old2, type="response")



##submit�̌`���ŏo��(CSV)
##�f�[�^���H
out_new <- data.frame(id=test_new2$id, pred_test=pred_test_new)

out_old <- data.frame(id=test_old2$id, pred_test=pred_test_old)

out <- rbind(out_new, out_old)

##�o��
write.table(out, ##�o�̓f�[�^
            "../submit/submit_20171018r2_1_logi.csv", ##�o�͐�
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
##     contact + day + month + campaign + poutcome + duration3 + 
##     age2 + balance2, family = binomial(link = "logit"), data = train2)
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -3.2268  -0.3629  -0.2131  -0.1175   3.5523  

logi_model_min <- glm(
    y ~ 1, 
    data=train2,             ##�w�K�f�[�^ �S���g��!!
    family=binomial(link="logit") ##���W�X�e�B�b�N��A���w��
)

step.model_min <- step(logi_model_min, direction='both',
                       scope=(~ age2 + job + marital + education + default + balance2 + housing + loan + contact + day + month + duration3 + campaign + pdays + previous + poutcome))

## �x�����b�Z�[�W: 
## glm.fit: ���l�I�� 0 �� 1 �ł���m���������܂���  

logi_model_min <- glm(
    y ~ duration3 + poutcome + month + contact + housing + age2 + 
    job + campaign + loan + balance2 , 
    data=train2,             ##�w�K�f�[�^ �S���g��!!
    family=binomial(link="logit") ##���W�X�e�B�b�N��A���w��
)

step.model_min <- step(logi_model_min, direction='both',
                       scope=(~ age2 + job + marital + default + balance2 + housing + loan + contact + month + duration3 + campaign + pdays + previous + poutcome))


##�I�����ꂽ�ϐ��̊m�F
summary(step.model_min)
## Call:
## glm(formula = y ~ duration3 + poutcome + month + contact + housing + 
##     age2 + job + campaign + loan + education + day + marital + 
##     balance2, family = binomial(link = "logit"), data = train2)
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -3.2268  -0.3629  -0.2131  -0.1175   3.5523  

logi_model_4 <- glm(formula = y ~ duration3 + poutcome + month + contact + housing + 
    age2 + job + campaign + loan + education + day + marital + 
    balance2, family = binomial(link = "logit"), data = train2)

pred_test <- predict(logi_model_4, newdata=test2, type="response")

##submit�̌`���ŏo��(CSV)
##�f�[�^���H
out<-data.frame(test2$id, pred_test)

##�o��
write.table(out, ##�o�̓f�[�^
            "../submit/submit_20171018_2_logi.csv", ##�o�͐�
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