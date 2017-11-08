#�g�p���C�u����
library(rpart)
library(pROC)
library(dplyr)
library(ggplot2)

#�f�[�^�Ǎ�
train<-read.csv("../motodata/train.csv", header=T)

#�f�[�^��K����
K<-5

##�܂���maxdepth=5�ŃN���X�o���f�[�V����
#�Č����̂��ߗ����V�[�h���Œ�
set.seed(17)

#�w�K�f�[�^��K�ɃO���[�v����
#sample(�x�N�g��, �����_���Ɏ擾�����, �������o�̗L��, �x�N�g���̊e�v�f�����o�����m��)
train$cv_group<-sample(1:K, nrow(train), replace=TRUE, prob=rep(1/K, K))

#�\�z, ���؃f�[�^�̃X�R�A�̏�����
score_train_tmp<-0
score_test_tmp<-0

ptm <- proc.time()

#�N���X�o���f�[�V����
for(j in 1:K){
  #�\�z, ���؃f�[�^�ɕ�����
  train_tmp<-train %>%
    dplyr::filter(cv_group!=j) %>%
    dplyr::select(-cv_group)
  test_tmp<-train %>%
    dplyr::filter(cv_group==j) %>%
    dplyr::select(-cv_group)
  #�\�z�f�[�^�Ń��f���\�z(�����rpart�ɂ�錈���)
  tree_tmp<-rpart(y~., data=train_tmp,
                  maxdepth=5, minbucket=1, cp=0.00001,
                  method="class", parms=list(split="gini"))
  
  #�\�z�f�[�^�̗\���l
  pred_train<-predict(tree_tmp, train_tmp)[,2]
  
  #�\�z�f�[�^��AUC
  auc<-roc(train_tmp$y, pred_train)$auc
  
  #�\�z�f�[�^��AUC�𑫂��Ă���
  score_train_tmp<-score_train_tmp + auc
  
  #���؃f�[�^�̗\���l
  pred_test<-predict(tree_tmp, test_tmp)[,2]
  
  #���؃f�[�^��AUC
  auc<-roc(test_tmp$y, pred_test)$auc
  
  #���؃f�[�^��AUC�𑫂��Ă���
  score_test_tmp<-score_test_tmp + auc
}

proc.time() - ptm

#�\�z�f�[�^��AUC
print(score_train_tmp/K)
#���؃f�[�^��AUC
print(score_test_tmp/K)

## �\�z�ƌ��؂�AUC���߂��ꍇ�͉ߊw�K���s�Ȃ��Ă��Ȃ��Ƒz��

##maxdepth�𓮂����ă`���[�j���O
#�Č����̂��ߗ����V�[�h���Œ�
set.seed(17)

#1�`15�܂Ń`���[�j���O����
##search_vec<-c(1,2,3,4,5,6,7,8,9,10)
search_vec<-c(1,3,5,10,15,20,50,75,100)

ptm <- proc.time()
#�\�z, ���؃f�[�^�̃X�R�A�̏�����
score<-data.frame(train=NA, test=NA, search=search_vec)
l<-1
for(i in search_vec){
  
  #�w�K�f�[�^��K�ɃO���[�v����
  #sample(�x�N�g��, �����_���Ɏ擾�����, �������o�̗L��, �x�N�g���̊e�v�f�����o�����m��)
  train$cv_group<-sample(1:K, nrow(train), replace=TRUE, prob=rep(1/K, K))
  
  #�\�z, ���؃f�[�^�̃X�R�A�̏�����
  score_train_tmp<-0
  score_test_tmp<-0
  
  #�N���X�o���f�[�V����
  for(j in 1:K){
    
    #�\�z, ���؃f�[�^�ɕ�����
    train_tmp<-train %>%
      dplyr::filter(cv_group!=j) %>%
      dplyr::select(-cv_group)
    test_tmp<-train %>%
      dplyr::filter(cv_group==j) %>%
      dplyr::select(-cv_group)
    #�\�z�f�[�^�Ń��f���\�z(�����rpart�ɂ�錈���)
    #maxdepth�ȊO�̃p�����[�^�͓K���ɌŒ�
    tree_tmp<-rpart(y~., data=train_tmp,
                    #maxdepth=i, #i�ɂ�search_vec�̒l�����ɓ���
                    maxdepth=, #i�ɂ�search_vec�̒l�����ɓ���
                    minbucket=i, cp=0.00001,
                    method="class",
                    #parms=list(split="gini"))
                    parms=list(split="information"))
    
    #�\�z�f�[�^�̗\���l
    pred_train<-predict(tree_tmp, train_tmp)[,2]
    
    #�\�z�f�[�^��AUC
    auc<-roc(train_tmp$y, pred_train)$auc
    
    #�\�z�f�[�^��AUC���Ƃ��Ă���
    score_train_tmp<-score_train_tmp + auc
    
    #���؃f�[�^�̗\���l
    pred_test<-predict(tree_tmp, test_tmp)[,2]
    
    #���؃f�[�^��AUC
    auc<-roc(test_tmp$y, pred_test)$auc
    
    #���؃f�[�^��AUC���Ƃ��Ă���
    score_test_tmp<-score_test_tmp + auc
  }
  #�\�z�f�[�^(�S��)�̗\���l
  score$train[l]<-score_train_tmp/K
  #���؃f�[�^(�S��)�̗\���l
  score$test[l]<-score_test_tmp/K
  #score�ɑ������s���̍X�V
  l<-l+1
}
proc.time() - ptm

#�ߊw�K�̊m�F(����)
g<-ggplot(score, aes(search))+
  geom_line(aes(y=train, color="train"))+geom_line(aes(y=test, color="test"))+
  xlab("Search")+ylab("AUC")+ggtitle("Cross validation")+
  scale_colour_discrete(name="Data Set")
plot(g)