#점심시간 문제  
library(caret)
library(C50)
library(irr)
credit <- read.csv("credit.csv", stringsAsFactors=TRUE)
str(credit)
library(caret)

set.seed(123)

in_train <- createDataPartition(credit$default, p = 0.75, list = FALSE)
credit_train <- credit[in_train, ] # 훈련 데이터 구성
credit_test <- credit[-in_train, ] # 테스트 데이터 구성

ctrl <-  trainControl( method="cv", number=40, selectionFunction="oneSE" )
m <- train( default~ . , data=credit, method="C5.0", metric="Kappa", trControl= ctrl )
#m<-train(default~., data=credit_train, method="C5.0")
m

p<-predict(m,credit_test)
p
table(p, credit_test$default)

#정확도 확인하는 코드
library(gmodels)
y <- CrossTable(credit_test$default, p )
sum(y$prop.tbl*diag(2))
