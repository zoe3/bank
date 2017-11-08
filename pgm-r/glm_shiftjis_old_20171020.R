##�g�p���C�u����
library(pROC)
library(dplyr)
library(ggplot2)

##�f�[�^�Ǎ�
train<-read.csv("../motodata/train.csv", header=T)
test<-read.csv("../motodata/test.csv", header=T)

## Customer�𕪊�. Old
train_old <- train %>%
    dplyr::filter(pdays > -1)
test_old <- test %>%
    dplyr::filter(pdays > -1)

## �O�L�����y�[���̓��t���߂�
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")

train_old <- train_old %>%
    mutate(pdate = as.integer(as.Date(paste(day,month,"2017",sep=""),"%d%b%Y")))

## �֌W���肻��
table(train_old$pdate, train_old$y)

test_old <- test_old %>%
    mutate(pdate = as.integer(as.Date(paste(day,month,"2017",sep=""),"%d%b%Y")))

Sys.setlocale("LC_TIME", lct)

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
    y ~ pdate,
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
train_old <- train_old%>%
  dplyr::mutate(duration2=ifelse(duration >= quantile(duration,probs=.995),
                                quantile(duration,probs=.995),
                                duration))
##�ϐ����H(duration2��sqrt�ŕϊ�)
train_old2 <- train_old %>%
    dplyr::mutate(duration3 = sqrt(duration2)) %>%
    dplyr::select(-c(duration,duration2))

## age, balance�̉��H������B
train_old2 <- train_old2 %>%
    dplyr::mutate(age2=abs(50-age)) %>%
    dplyr::mutate(balance2=ifelse(balance >= quantile(balance,probs=.95),
                                  quantile(balance,probs=.95),
                                  balance)) %>%
    dplyr::select(-c(age,balance))
            
######�V�ϐ��Ń��W�X�e�B�b�N��A���f���\�z
##Hold Out(��قǂƓ���������)

## �s�v�J�����̔r��
train_old2 <- train_old2 %>%
    dplyr::select('-id')

train_train_old2<-train_old2[row,]
train_test_old2<-train_old2[-row,]

logi_model_min <- glm(
    y ~ 1,
    data=train_train_old2,             ##�w�K�f�[�^ �S���g��!!
    family=binomial(link="logit") ##���W�X�e�B�b�N��A���w��
)

step.logi_model_min <- step(logi_model_min, direction='both',
                       scope=(~ pdate + age2 + job + marital + default + balance2 + housing + loan + contact + month + duration3 + campaign + pdays + previous + poutcome))

summary(step.logi_model_min)

logi_model2 <- glm(
    y ~ .,    ##�ړI�ϐ��Ɛ����ϐ��̎w��(�S�Ďg���ꍇ��y~.)
    data=train_train_old2,             ##�w�K�f�[�^
    family=binomial(link="logit") ##���W�X�e�B�b�N��A���w��
)

logi_model2 <- glm(formula = y ~ poutcome + duration3 + month + housing + campaign + 
    pdate + loan + marital + contact, family = binomial(link = "logit"), 
    data = train_train_old2)

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
    y ~ poutcome + duration3 + month + housing + campaign + pdate + loan + marital + contact,
    data=train_old2,             ##�w�K�f�[�^ �S���g��!!
    family=binomial(link="logit") ##���W�X�e�B�b�N��A���w��
)

summary(logi_model3)
pred_test_old <- predict(logi_model3, newdata=test_old2, type="response")

## dataframe�̕ۑ�
test_old$pred <- pred_test_old
save(test_old, file="test_old.dat")


