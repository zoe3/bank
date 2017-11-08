##�g�p���C�u����
library(rpart)
library(dplyr)
library(pROC)
library(partykit)

##partykit�̃C���X�g�[��
##install.packages("partykit")

###�f�[�^�Ǎ�
train<-read.csv("../motodata/train.csv", header=TRUE)
test<-read.csv("../motodata/test.csv", header=TRUE)

###����؃��f���쐬
##�����̂��ߖړI�ϐ��̌^���t�@�N�^�[�ɕϊ�
train$y<-as.factor(train$y)

str(train)
## duration���폜����B
train <- train %>%
    select(-duration)

str(train)

##����؃��f���쐬
tree<-rpart(y~., #�ړI�ϐ�~�����ϐ�, �u.�v��y�ȊO�̑S�ϐ�������ϐ��Ƃ���Ƃ����Ӗ�
            data=train, #�w�K�f�[�^
            maxdepth=8, #�K�w��
            minbucket=100, #�ŏ��m�[�h�T�C�Y
            cp=0.0001, #�}����̋���
            method="class", #����
            parms=list(split="information")
            #parms=list(split="gini") #�����, list(split="information")�Ƃ����Entropy��ƂȂ�
            )

##�\�z�f�[�^�ł�AUC
##���f���̓��Ă͂�
pred_train<-predict(tree, train)[,2]
##AUC
auc<-roc(train$y, pred_train)
print(auc)
###Area under the curve: 0.7487

#### duration�폜�O�̌���
### --
### maxdepth=8, #�K�w��
### minbucket=100, #�ŏ��m�[�h�T�C�Y
### Area under the curve: 0.8493
### --
### maxdepth=8, #�K�w��
### minbucket=100, #�ŏ��m�[�h�T�C�Y
### parms=list(split="information")
### Area under the curve: 0.859

#### duration���폜��̌���
### Area under the curve: 0.6359

###����
plot(as.party(tree))

###�\���f�[�^�Ɍ���؃��f����K�p
##���f���̓��Ă͂�
pred_test<-predict(tree, test)[,2]

##submit�̌`���ŏo��(CSV)
##�f�[�^���H
out<-data.frame(test$id, pred_test)

##�o��
write.table(out, #�o�̓f�[�^
            "C:/study/bank/submit/submit_20170927_1_rpart.csv", #�o�͐�
            quote=FALSE, #��������u"�v�ň͂ޗL��
            col.names=FALSE, #�ϐ���(��)�̗L��
            row.names=FALSE, #�s�ԍ��̗L��
            sep="," #��؂蕶���̎w��
)