# install.packages("caret")
# install.packages("glmnet")
# install.packages("car")
library(caret)
library(glmnet)
library(car)

### 1. logistic reg
rm(list = ls())
getwd()
setwd("D:\\tobigs2\\2weeks(reg,logistic)\\logistic")

# 1-1. 데이터 불러오기 
data <- read.csv("titanic.csv",header = T,stringsAsFactors = F)
dim(data)
names(data)
str(data)
head(data)

sum(is.na(data))
data <- na.omit(data) #na 가 포함된 행 제거 
sum(is.na(data))
dim(data)

# # 1-2.전처리
# data$Survived <- as.factor(data$Survived)
# data$Pclass <- as.factor(data$Pclass)
# data$Pclass_1 <- as.factor(ifelse(data$Pclass=='1',1,0)) #Pclass = 1 이면 1, 아니면 0
# data$Pclass_2 <- as.factor(ifelse(data$Pclass=='2',1,0)) #Pclass = 2 이면 1, 아니면 0
# data <- data[,-which(names(data)=='Pclass')] #Pclass 지우기
# data$Sex <- as.factor(data$Sex)
# data$Embarked <- as.factor(data$Embarked)
# data$Embarked_C <- as.factor(ifelse(data$Embarked=='C',1,0)) #Embarked = C 이면 1, 아니면 0
# data$Embarked_Q <- as.factor(ifelse(data$Embarked=='Q',1,0)) #Embarked = Q 이면 1, 아니면 0
# data$Embarked_S <- as.factor(ifelse(data$Embarked=='S',1,0)) #Embarked = S 이면 1, 아니면 0
# data <- data[,-which(names(data)=='Embarked')] #Embarked 지우기

# 1-3.train set/ test set 나누기 
## createDataPartition 함수도 써보기! 모를 땐, ?createDataPartition 시행 후 예시 따라해보면 이해 쏙쏙 

set.seed(123)
idx <- sample(1:length(data[,1]),length(data[,1])*0.8,replace = F)
train <- data[idx,]
test <- data[-idx,]

# 1-4. glm 적용해보기 glm(y~.,data,family=binomial(link='logit'))
Model1 <- glm(Survived~.,data=train,family=binomial(link = 'logit'))
summary(Model1)
coef(Model1)
vif(Model1)
pred_model1 <- predict(Model1,newdata=test,type='response')
pred_model1 <- round(pred_model1)
table(test$Survived,pred_model1)
sum(diag(table(test$Survived,pred_model1)))/length(test[,1])

# 1-5. forward/ backward/ stepwise (full/not)glm 생성 후 step
Model.full <- glm(Survived~.,data=train,family = binomial(link='logit'))
Model.con <- glm(Survived~1,data=train,family = binomial(link='logit'))
Model_for <- step(Model.con,list(lower=Model.con,upper=Model.full),direction = 'forward')
summary(Model_for)
Model_back <- step(Model.full,list(lower=Model.con,upper=Model.full),direction = 'backward')
summary(Model_back)
Model_step <- step(Model.con,list(lower=Model.con,upper=Model.full),direction = 'both')
pred_for <- predict(Model_for,newdata = test,type='response')
table(test$Survived,round(pred_for))
pred_back <- predict(Model_back,newdata = test,type='response')
table(test$Survived,round(pred_back))
pred_step <- predict(Model_step,newdata = test,type='response')
table(test$Survived,round(pred_step))

# 1-6. roc 커브 그려보기
# install.packages("pROC")
library(pROC)
curve<-roc(test$Survived, pred_step, direction="<")
curve$auc
plot(curve)


### 2. Multinomial logistic reg

# 2-1. 데이터 불러오기
# install.packages("nnet")
library(nnet)
data(iris)
str(iris)
# 2-2. train set/ test set 나누기
?createDataPartition
set.seed(123)
idx <- createDataPartition(iris$Species,p = 0.7,list=F) 
train <- iris[idx,]
test <- iris[-idx,]
# 2-3. multinom(y~.,data,link='logit')
Model_multi <- multinom(Species~.,data=train,link = 'logit')
summary(Model_multi)
pred <- predict(Model_multi,newdata = test,type='class')
table(test$Species,pred)


### 3. ridge / lasso reg
# install.packages("mlbench")

# 3-1. 데이터 불러오기(train,test 나누기 까지)
library(mlbench)
data(Sonar)
str(Sonar)
set.seed(123)
idx<-sample(1:length(Sonar[,1]),length(Sonar[,1])*.8,replace = F)
train <- Sonar[idx,]
test <- Sonar[-idx,]

# 3-2. glm(y~.,data,family="binomial") 적용 --> 안 된다.
ggg <- glm(Class~.,data=train,family = "binomial")
#Warning messages:
# 1: glm.fit: 알고리즘이 수렴하지 않았습니다 
# 2: glm.fit: 적합된 확률값들이 0 또는 1 입니다 
# 다중공선성 때문에 위의 말처럼 모든 값들이 0으로 예측하거나 1로 예측한다.
vif(ggg)

# 3-3. ridge 적용 cv.glmnet(x,y,family="binomail",alpha=0,nfolds=10) 하고 예측까지 
ridge_model <- cv.glmnet(x=data.matrix(train[,-61]),y=train[,61],family="binomial",alpha=0,nfolds =10)
coef(ridge_model)

ridge_model$lambda.min
ridge_model$lambda.1se
pred_ridge <- predict(ridge_model,data.matrix(test[,-61]),s= ridge_model$lambda.min,type="class")
table(pred_ridge,test[,61])

# 3-4. graph그리기 plot/ plot.glm
par(mfrow=c(1,2))
plot(ridge_model)
plot.glmnet(ridge_model$glmnet.fit,xvar="lambda")

# 3-5 .동일하게 lasso 하는데 lambda를 여러번 조정해봐야 한다. 
# 적절한 변수 선택을 하기 위해서!(너무 중요한 변수가 사라질 수도 있는 걸 막기 위해서) 
lasso_model <- cv.glmnet(x=data.matrix(train[,-61]),y=train[,61],family="binomial",alpha=1,nfolds =10)
pred_lasso1 <- predict(lasso_model,data.matrix(test[,-61]),s=lasso_model$lambda.min,type="class")
pred_lasso2 <- predict(lasso_model,data.matrix(test[,-61]),s=lasso_model$lambda.1se,type="class")

table(pred_lasso1,test[,61])
table(pred_lasso2,test[,61])

plot(lasso_model)
plot.glmnet(lasso_model$glmnet.fit,xvar="lambda")

# 3-6. 동시에
par(mfrow=c(2,2))