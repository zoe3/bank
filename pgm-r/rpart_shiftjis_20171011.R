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

## > table(train$previous, train$poutcome)
##       failure other success unknown
##   0         0     0       0   22146
##   1      1041   363     279       1
##   2       816   243     213       1
##   3       407   139     143       0
##   4       251   110      84       0
##   5       161    58      44       1
##   6        85    45      36       0
##   7        59    31      26       1
  

## �J�����ǉ�
train$data <- "train"
test$data <- "test"
test$y <- NA

combi <- rbind(train,test)

combi$age2 <- '53+'
combi$age2[combi$age < 31] <- '<31'
combi$age2[combi$age < 35 & combi$age >= 31] <- '31-34'
combi$age2[combi$age < 40 & combi$age >= 35] <- '35-39'
combi$age2[combi$age < 46 & combi$age >= 40] <- '40-45'
combi$age2[combi$age < 53 & combi$age >= 46] <- '46-52'

combi$age2 <- as.factor(combi$age2)

aggregate(duration ~ job, data=combi, FUN=function(x) {sum(x)/length(x)})
aggregate(duration ~ job, data=combi, FUN=function(x) {var(x)})

combi$category <- 'unknown'
combi$category[combi$job %in% c('admin.','self-employed','entrepreneur')] <- 'capitalist'
combi$category[combi$job %in% c('management','blue-collar','services','technician','housemaid')] <- 'worker'
combi$category[combi$job %in% c('retired','student','unemployed')] <- 'not-worker'
combi$category <- as.factor(combi$category)

train <- combi %>%
    dplyr::filter(data=='train') %>%
    dplyr::select(-data)

test <- combi %>%
    dplyr::filter(data=='test') %>%
dplyr::select(-data)

##����؃��f���쐬
#�����̂��ߖړI�ϐ��̌^���t�@�N�^�[�ɕϊ�
train$y<-as.factor(train$y)

#����؃��f���쐬
tree<-rpart(y ~ . - id, #�ړI�ϐ�~�����ϐ�, �u.�v��y�ȊO�̑S�ϐ�������ϐ��Ƃ���Ƃ����Ӗ�
            data=train, #�w�K�f�[�^
            maxdepth=15, #�K�w��
            minbucket=10, #�ŏ��m�[�h�T�C�Y
            cp=0.0001, #�}����̋���
            method="class", #����
            #parms=list(split="gini") #�����, list(split="information")�Ƃ����Entropy��ƂȂ�
            parms=list(split="information")
            )

#�\�z�f�[�^�ł�AUC
#���f���̓��Ă͂�
pred_train<-predict(tree, train)[,2]
#AUC
auc<-roc(train$y, pred_train)
print(auc)

##����
plot(as.party(tree))

## ��������2
plot(tree)
text(tree, use.n=TRUE, cex=0.5)
path.rpart(tree)

## cp���v�ʂ��m�F
printcp(tree)
plotcp(tree)

##�\���f�[�^�Ɍ���؃��f����K�p
#���f���̓��Ă͂�
pred_test<-predict(tree, test)[,2]

#submit�̌`���ŏo��(CSV)
#�f�[�^���H
out<-data.frame(test$id, pred_test)

#�o��
write.table(out, #�o�̓f�[�^
            "../submit/submit_20171011_1_rpart.csv", #�o�͐�
            quote=FALSE, #��������u"�v�ň͂ޗL��
            col.names=FALSE, #�ϐ���(��)�̗L��
            row.names=FALSE, #�s�ԍ��̗L��
            sep="," #��؂蕶���̎w��
)
