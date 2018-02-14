##### sample_data로 연습하기 #####

rm(list=ls())
# getwd()
# setwd("D:\\tobigs2\\3weeks(knn,clustering,association)\\association")
# install.packages("arules")
library(arules)

# 1.데이터 불러오기 (개인당 롤 플레이어 목록)
df <- read.csv("sample_data.csv")
str(df)
table(df$id)

# 2.데이터 변환 * 꼭 "transaction" 으로 바꾸기 * #transaction 만들기 파트가 맨 뒤에 또 있음!
play.list <- split(df$names,df$id) #split(분리할 벡터,분리할 기준)
play.transaction <- as(play.list,"transactions")
play.transaction

# 3.transaction 데이터 보기
inspect(play.transaction[1:10]) #head 로 볼 수 없다..
image(sample(play.transaction,5,replace=FALSE))
itemFrequency(play.transaction) #상대도수
itemFrequency(play.transaction,type="absolute") #도수
itemFrequency(play.transaction[,1:10]) #거래품목(item)별로 거래에서 차지하는 비율(support) - 앞의 10개만 확인
itemFrequencyPlot(play.transaction) #도수 막대그래프,#type="absolute"
itemFrequencyPlot(play.transaction,support=0.01,main="item frequency plot support") #지지도 1% 이상의 item에 대한 막대그래프
itemFrequencyPlot(play.transaction, topN=30, main="support top 30 item") #support 상위 30개의 막대그래프
play.transaction@itemInfo 
play.transaction@data  #sparse matrix 

# 4.apriori 함수 적용하기 
# apriori(data,parameter=list(support=0.1, confidence=0.8, minlen=1, maxlen=10, smax=1))
# support=최소지지도, confidence=최소신뢰도, minlen=최소물품수(lhs+rhs), maxlen=최대물품수(lhs+rhs), smax=최대지지도
# 별다른 지정을 안해주면 위의 default 값으로 저장된다.
rules <- apriori(play.transaction)
summary(rules)
inspect(rules)
inspect(sort(rules[1:20],by="lift"))
inspect(sort(rules[1:20],by="confidence"))

# 5.시각화
library(arulesViz)
plot(rules) 
plot(sort(rules, by = "lift"), method = "grouped")
plot(sort(rules, by = "confidence"), method = "grouped")
#빨갈수록 lift값이 크고, 원이 클수록 support값이 크다.
plot(rules, method = "graph", control = list(type="items"))
#{item}->{item} 연관규칙의 지지도는 원의 크기, 색깔은 향상도 
plot(rules,method="graph",interactive = T)   
plot(rules[1:5],method="graph",interactive = T)   
plot(rules,method="paracoord")

##### Groceries data로 연습하기 #####

# 1. 데이터 불러오기 
library(datasets)
data("Groceries")
Groceries
summary(Groceries)

# 2.transaction 데이터 보기
inspect(Groceries[1:10])
image(sample(Groceries,200,replace=FALSE))
itemFrequency(Groceries) #상대도수
itemFrequency(Groceries,type="absolute") #도수
itemFrequency(Groceries[,1:10]) #거래품목(item)별로 거래에서 차지하는 비율(support) - 앞의 10개만 확인
itemFrequencyPlot(Groceries) #도수 막대그래프,#type="absolute"
itemFrequencyPlot(Groceries,support=0.01,main="item frequency plot support") #지지도 1% 이상의 item에 대한 막대그래프
itemFrequencyPlot(Groceries, topN=30, main="support top 30 item") #support 상위 30개의 막대그래프
Groceries@itemInfo 
Groceries@data  #sparse matrix 

# 3. apriori 함수 적용하기 
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))
rules
inspect(rules[1:5])
inspect(sort(rules,by="lift")[1:10])

rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8, maxlen=3))
inspect(sort(rules,by="confidence")[1:10])
inspect(sort(rules,by="lift")[1:10])

# 3.2. 목표 제품 집중 분석
# whole milk 를 사는 고객들은 이전에 어떤 제품들을 구매했을까?
# whole milk 를 구매하는 고객들은 다음에 어떤 제품들을 구매할 것인가?

rules_before <-apriori(Groceries, parameter=list(supp=0.001,conf = 0.08),appearance = list(default="lhs",rhs="whole milk"))
inspect(sort(rules_before,by="confidence")[1:5])
#{rice,sugar}/{canned fish,hygiene articles}/{root vegetables,butter,rice}/{root vegetables,whipped/sour cream,flour}/{butter,soft cheese,domestic eggs}

rules_after <-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.15,minlen=2),appearance = list(lhs="whole milk", default="rhs"))
inspect(sort(rules_after,by="confidence")[1:5])
#{other vegetables}/{rolls/buns}/{yogurt}/{root vegetables}/{tropical fruit} 

# whole milk와 salt 를 구매한 고객은 어떤걸 구매할까?
rules_after <-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.15,minlen=3),appearance = list(lhs=c("whole milk","salt"), default="rhs"))
inspect(sort(rules_after,by="confidence")[1:5])

#rule_interest_lhs <- subset(rules, lhs %in% c("doc_72f", "doc_4ac"))

# 4. 시각화
plot(rules, method="grouped")
plot(sort(rules, by = "confidence"), method = "grouped")
plot(rules[1:5],method="graph",interactive = T)
plot(rules,method="paracoord")

####################################################################
# transactions class는 arules 패키지내에 연관성 분석을 위한 class
# 기존의 데이터를 transactions class로 변환
# 모든 요소들이 팩터형이여야 한다.
## 1. matrix -> transactions class
matrix <- matrix(c(1,1,0,0,0,
                   1,0,1,0,0,
                   0,1,1,1,0,
                   1,0,0,0,1,
                   0,0,1,1,1), ncol=5, byrow=T)

dimnames(matrix)<-list(paste0("trans",1:5),letters[1:5])
trans.matrix <-as(matrix,"transactions")
summary(trans.matrix)
inspect(trans.matrix)
## 2. data.frame -> transactions class
df <- as.data.frame(matrix)
str(df)
df <- as.data.frame(sapply(df,as.logical))
df.trans <-as(df,"transactions")
summary(df.trans)
inspect(df.trans)
## 3. list -> transactions class
list <- list(tr1=c("a","b","c"),
             tr2=c("a","d"),
             tr3=c("b","e"),
             tr4=c("a","d","e"),
             tr5=c("b","c","d"))
list
trans.list <-as(list,"transactions")
summary(trans.list)
inspect(trans.list)

#범주형 자료를 이진화시켜서 분석 
cust_id <- c(1, 2, 3, 4, 5, 6)
gender <- c("FEMALE", "MALE", "FEMALE", "FEMALE", "MALE", "FEMALE")
age <- c(23, 28, 42, 34, 45, 36)
child_prd_yn <- c("NO", "NO", "NO", "YES", "NO", "YES")
mobile_app_use <- c("YES", "YES", "NO", "YES", "NO", "YES")
re_order <- c("YES", "NO", "NO", "YES", "NO", "YES")

cust_mart <- cbind(cust_id, gender, age, child_prd_yn, mobile_app_use, re_order)
cust_mart <- as.data.frame(cust_mart)
str(cust_mart)

cust_mart <- transform(cust_mart, cust_id = as.character(cust_id),
                       age=as.numeric(as.character(age)))
sapply(cust_mart,class)
str(cust_mart)

# age : custinuous data -> discretization
cust_mart <- within(cust_mart, { #with랑 비슷한 역할을 하는 함수 
  age_cd = character(0)
  age_cd[ age <= 29 ] = "age_20"
  age_cd[ age > 29 & age <= 39 ] = "age_30"
  age_cd[ age > 39 ] = "age_40"
  age_cd = factor(age_cd, level = c("age_20", "age_30", "age_40"))
})
# cust_mart$age_cd <- cut(cust_mart$age,c(0,29,39,100),labels = c("age_20", "age_30", "age_40")) 위의 within과 똑같은 효과 
cust_mart_ar <- subset(cust_mart, select=-c(cust_id, age))
#only factors
cust_mart_trans <-as(cust_mart_ar, "transactions")
inspect(cust_mart_trans)
