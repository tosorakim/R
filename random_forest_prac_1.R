library(caret)
library(C50)
library(irr)
data(iris)
head(iris)

set.seed(123)
in_train <- createDataPartition(iris$Species, p = 0.75, list = FALSE)
iris_train <- iris[in_train, ] # 훈련 데이터 구성
iris_test <- iris[-in_train, ] # 테스트 데이터 구성


m <- train( Species~ . , data=iris_train, method="rf") #random forest를 쓰겠다.

m # 튜닝한 결과를 확인할 수 있다.

p <- predict( m , iris_test ) 
table(p, iris_test$Species)

library(gmodels)
y <- CrossTable(iris_test$Species ,p)
sum(y$prop.tbl * diag(3))