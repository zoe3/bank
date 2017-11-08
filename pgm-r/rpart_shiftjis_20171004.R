#�g�p���C�u����
library(rpart)
library(dplyr)
library(pROC)
library(partykit)

#partykit�̃C���X�g�[��
#install.packages("partykit")

##�f�[�^�Ǎ�
train<-read.csv("../motodata/train.csv", header=TRUE)
test<-read.csv("../motodata/test.csv", header=TRUE)

##����؃��f���쐬
#�����̂��ߖړI�ϐ��̌^���t�@�N�^�[�ɕϊ�
train$y<-as.factor(train$y)

#����؃��f���쐬
tree<-rpart(y~., #�ړI�ϐ�~�����ϐ�, �u.�v��y�ȊO�̑S�ϐ�������ϐ��Ƃ���Ƃ����Ӗ�
            data=train, #�w�K�f�[�^
            maxdepth=8, #�K�w��
            minbucket=250, #�ŏ��m�[�h�T�C�Y
            cp=0.001, #�}����̋���
            method="class", #����
            parms=list(split="gini") #�����, list(split="information")�Ƃ����Entropy��ƂȂ�
)

#�\�z�f�[�^�ł�AUC
#���f���̓��Ă͂�
pred_train<-predict(tree, train)[,2]
#AUC
auc<-roc(train$y, pred_train)
print(auc)

#����
plot(as.party(tree))


##�\���f�[�^�Ɍ���؃��f����K�p
#���f���̓��Ă͂�
pred_test<-predict(tree, test)[,2]

#submit�̌`���ŏo��(CSV)
#�f�[�^���H
out<-data.frame(test$id, pred_test)

#�o��
write.table(out, #�o�̓f�[�^
            "C:/bank/submit/submit_1004_1_rpart.csv", #�o�͐�
            quote=FALSE, #��������u"�v�ň͂ޗL��
            col.names=FALSE, #�ϐ���(��)�̗L��
            row.names=FALSE, #�s�ԍ��̗L��
            sep="," #��؂蕶���̎w��
)