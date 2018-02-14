##Tree R 실습##

# 데이터 불러오기 #
data <- read.csv("titanic.csv",header = T,stringsAsFactors = F)
str(data)
head(data)

sum(is.na(data)) #264
data <- na.omit(data) #제거

# 데이터 범주화시켜주기(가변수화해줄 필요는 없다) #
data$Survived <- as.factor(data$Survived)
data$Pclass <- as.factor(data$Pclass)
data$Sex <- as.factor(data$Sex)
data$Embarked <- as.factor(data$Embarked)
str(data)

# train data/ test data 나누기 #
# install.packages("caret")
library(caret)
set.seed(1234)
idx <- createDataPartition(data$Survived, p=0.7, list=FALSE)
train <- data[idx,];str(train)
test <- data[-idx,];str(test)

table(train$Survived)/dim(train)[1] # 60:40 비율로 나뉘었다
table(test$Survived)/dim(test)[1] # 60:40 비율로 나뉘었다 good

########### 1. tree package ###########
#install.packages("tree")
library(tree)

# tree 모델 생성 #
# 사용법 : tree(target~.,data,..)

############################### 기본 tree ##############################
  treemodel1 <- tree(Survived~.,data=train) #default : split="deviance" 
  treemodel1 # *은 끝마디를 의미한다. 
  plot(treemodel1) # tree 형태 생성
  text(treemodel1) # tree 변수들 입력

  tree_pred1 <- predict(treemodel1,test[,-1],type="class")
  confusionMatrix(tree_pred1,test$Survived)
  #accuracy : 0.8562
#########################################################################

############################################## gini 계수 기준 tree ##############################################
  treemodel2 <- tree(Survived~.,data=train,split = "gini") #gini계수로 예측 
  treemodel2
  plot(treemodel2)
  text(treemodel2) #호우... parameter 조정해보거나 pruning을 해보자 

  tree_pred2 <- predict(treemodel2,test[,-1],type="class")
  confusionMatrix(tree_pred2,test$Survived)
  #accuracy : 0.8562 # tree가 복잡한 것에 비해 정확도가 단순한 tree(treemodel1)보다 나아지지 않았다. --> overfit?

  tree_pred2_train <- predict(treemodel2,train[,-1],type="class")
  confusionMatrix(tree_pred2_train,train$Survived)
  #train accuracy : 0.8934 # overfit이 발생했다는 뜻! 
################################################################################################################

# tree function option #
# tree.control(mincut=5,minsize=10,mindev=0.01) #숫자는 default
# mincut :  두 자식마디 양쪽 모두에 있어야 하는 최소 관측값 수 
# minsize : split하기 위한 최소 관측값 수  
# mindev : split 하기 위한 최소 deviance
# nmax : 성장할 수 있는 최대 노드 수의 추정치
#### tree 함수 참고: https://cran.r-project.org/web/packages/tree/tree.pdf ###

  
##################################### control 값 조정 #######################################
  # mincut = 100 , minsize = 200 으로 해보자 : 마디에 최소 200개는 있어야 split이 되며,
  #  각각의 자식 마디에는 최소 100개씩 있어야 split 된다. 
  my_control <- list(mincut=100,minsize=200,nmax=100) #nmax를 안 써주면 오류가 난다.
  treemodel3 <- tree(Survived~.,data=train,split="gini",control = my_control)
  treemodel3
  plot(treemodel3);text(treemodel3)

  # mincut = 100 , minsize = 400 으로 해보자 : 마디에 최소 400개가 있어야 split 된다.
  my_control <- list(mincut=100,minsize=400,nmax=100) #nmax를 안 써주면 오류가 난다.
  treemodel4 <- tree(Survived~.,data=train,split="gini",control = my_control)
  treemodel4
  plot(treemodel4);text(treemodel4)
#############################################################################################

# overfitting 이 일어난 treemodel2 에 대한 pruning을 진행해보자
# 사용법 : cv.tree(Model,FUN=prune.misclass) 으로 최소 가지수 찾고,
#          prune.missclass(Model,best= ) 로 모델 생성 
  
########################################## pruning ##############################################
  treemodel2
  treemodel2_prune <- cv.tree(treemodel2,FUN=prune.misclass)
  plot(treemodel2_prune) # 오분류율을 최저로 하는 가지수를 찾아준다. 여기서는 4~10 정도
  treemodel2_pruning_f = prune.misclass(treemodel2, best=6)
  plot(treemodel2_pruning_f);text(treemodel2_pruning_f)

  tree_prune_pred1 <- predict(treemodel2_pruning_f,test[,-1],type="class")
  confusionMatrix(tree_prune_pred1,test[,1])
  #accuracy : 0.8818 # tree 는 더 간단해졌는데 accuracy 는 더 올라갔다. 개이득 
  tree_prune_pred2 <- predict(treemodel2_pruning_f,train[,-1],type="class") 
  confusionMatrix(tree_prune_pred2,train[,1])
  #train accuracy : 0.8661 #test acc 가 train acc 보다 높으므로 overfitting이 없다고 볼 수 있다.
#################################################################################################

  
########### 2. rpart package ###########
#install.packages("rpart")
library(rpart)

# tree 모델 생성 #
# 사용법 : rpart(target~.,data,..)
# rpartplot 활용하자!
  
############################### 기본 rpart tree ########################
  rpartmodel1 <- rpart(Survived~.,data=train)
  rpartmodel1
  plot(rpartmodel1);text(rpartmodel1)
  #install.packages("rpart.plot")
  library(rpart.plot)
  prp(rpartmodel1, type=4, extra=2, digits=3) 
  #type은 1,2,3,4 원하는대로~ , extra , digits 도 바꿔가면서 해보세요~ 
  
  rpart_pred1 <- predict(rpartmodel1,test[,-1],type="class")
  confusionMatrix(rpart_pred1,test$Survived)
  #accuracy : 0.8818
########################################################################  
  
# rpart function option #
# minsplit : split하기 위한 최소 관측값 수 (defualt=20)
# minbucket : 자식마디에 있어야 하는 최소 관측값 수  (defualt= round(minsplit/3))
# cp : (complexity parameter) : 높으면 간단한 나무, 낮으면 복잡한 나무 (default=0.01))   
# xval : cross-validation fold 갯수(default=10)
# maxdepth : 최대 깊이 (default=30) 
#### rpart 함수 참고 : https://cran.r-project.org/web/packages/rpart/rpart.pdf ####
  
############################ parameter 조정하면서 예측해보기 ###########################
  #minsplit 조정 
  rpartmodel2 <- rpart(Survived~.,data=train,minsplit=50) 
  rpartmodel2
  par(mfrow=c(1,2))
  prp(rpartmodel1 , type=4,extra=2,digits=3);prp(rpartmodel2 , type=4,extra=2,digits=3) 
  #minsplit=20(defualt)일 때보다 tree가 간단해 졌다.
  
  #maxdepth 조정
  rpartmodel3 <- rpart(Survived~.,data=train,maxdepth=1)
  rpartmodel3
  par(mfrow=c(1,1))
  prp(rpartmodel3 , type=4,extra=2,digits=3)  
  #tree 깊이가 최대 1이여서 한번만 split된다.
  
  #minsplit,cp 조정
  set.seed(12345)
  rpartmodel4 <- rpart(Survived~.,data=train,minsplit=13,cp=0.003)
  rpartmodel4
  prp(rpartmodel4 , type=4,extra=2,digits=3) 
  #overfit의 느낌이 난다..
  
  rpart_pred4 <- predict(rpartmodel4,test[,-1],type="class")
  confusionMatrix(rpart_pred4,test$Survived)
  #accuracy : 0.8658
  rpart_pred4_train <- predict(rpartmodel4,train[,-1],type="class")
  confusionMatrix(rpart_pred4_train,train$Survived)
  #train accuracy : 0.8975
  # overfit이 생긴다!
########################################################################################

# overfitting 이 일어난 rpartmodle4 에 대한 pruning을 진행해보자
# 사용법 : printcp(Model); plotcp(Model)로 최저 가지수 찾고,
# prune(Model,cp=Model$cptable[which.min(Model$cptable[,"xerror"])],"CP") 로 모델 생성 
  
########################################## pruning ##################################################
  printcp(rpartmodel4) # == rpartmodel4$cptable
  plotcp(rpartmodel4)
  
  rpartmodel4_prune <- prune(rpartmodel4,
                             cp=rpartmodel4$cptable[which.min(rpartmodel4$cptable[,"xerror"])],"CP")
  prp(rpartmodel4_prune, type=4,extra=2,digits=3)
  rpart_pred4_prune <- predict(rpartmodel4_prune,test[,-1],type="class")
  confusionMatrix(rpart_pred4_prune,test[,1])
  #accuracy : 0.869 tree가 간단해졌는데 acc는 올랐다.
  rpart_pred4_prune_train <- predict(rpartmodel4_prune,train[,-1],type="class")
  confusionMatrix(rpart_pred4_prune_train,train[,1])
  #train acc : 0.8839 #아직 overfit이 조금 남아있긴 하지만 train acc를 조금이나마 낮췄다. 
#####################################################################################################
### rpart는 tree 생성할 때마다 조금씩 다른 tree를 생성한다. 왜 그런거지.. 공부가 좀 더 필요할 듯

  
########### 3. party package ###########
#install.packages("party")
library(party)
  
# party package는 p-test를 거친 significance를 사용....--> pruning 과정 따로 필요 없다!
# tree 모델 생성 #
# 사용법 : ctree(target~.,data,..)
  
#################################### 기본 ctree tree #####################################
  partymodel1 <- ctree(Survived~.,data=train)
  partymodel1
  plot(partymodel1)
  
  party_pred1 <- predict(partymodel1,test[,-1],type="response") # type="class" 가 아니다!
  confusionMatrix(party_pred1,test$Survived)
  #accuracy : 0.8754
##########################################################################################
  
# ctree function option #
# ctree.control(minsplit=20,minbucket=7,mtry=Inf,maxdepth=Inf....많다) 각각 defualt
# minsplit : split하기 위한 최소 관측값 수 
# minbucket : 자식마디에 있어야 하는 최소 관측값 수 
# mtry : 무작위로 추출된 설명 변수의 수 (= random forest와 비슷) / default인 Inf는 모든 변수 다 고려한다는 뜻
# maxdepth : 최대 깊이 / default인 Inf는 깊이 제한 없다는 뜻입니다.
### ctree 함수 참고 : https://cran.r-project.org/web/packages/partykit/partykit.pdf ###
  
############################## parameter 조정하면서 예측해보기 ################################
  set.seed(12345)
  partymodel2 <- ctree(Survived~.,data=train,controls = ctree_control(mtry=3))
  #Tree에서 가지를 뻗어나갈 때(성장할 때) 임의로 변수 3개를 선택해서 그 중 가장 적당한 것 선택
  #그래서 시행할 때마다 다른 결과값을 갖는다.
  #set.seed() 주면 우리 모두 동일한 값을 볼 수 있다.
  partymodel2
  plot(partymodel2)
  
  party_pred2 <- predict(partymodel2,test[,-1],type="response")
  confusionMatrix(party_pred2,test$Survived)
  #변수를 3개씩만 써서 split하는데도 성능이 크게 떨어지지 않는다.
  #시간적인 측면에서 유리하다.
###############################################################################################
  
  
##### 세 개의 package 중에서 지금 실습 중에 accuracy 가 제일 높은 함수라고 하더라도
##### 그 함수가 항상 최적이 되는 것은 아니다!
##### 데이터의 분포에 따라, 설명변수의 선택 방법에 따라 변할 수 있다.

################################################################################################  
############################ 그동안 제 강의 들어주셔서 감사합니다.  ############################
################################################################################################
    