#■  k-foldout 실습

setwd("/Users/macbook/Documents/itwill/R")
#install.packages("caret")
library(caret)
credit <- read.csv("credit.csv") # 독일 은행의 채무 불이행자를 예측
nrow(credit) #1000

# 10-fold CV
folds <- createFolds(credit$default, k = 10)
str(folds) #설명: 전체 10폴드 교차검증을 수행하기 위해 샘플링 된 인덱스가 생성됨

credit01_test <- credit[folds$Fold01, ]
credit01_train <- credit[-folds$Fold01, ]
nrow(credit01_test ) # 100
nrow(credit01_train) # 900

#전체 10폴드 교차검증을 수행하려면 이 단계는 10회 반복되어야한다.
#result <- lapply(1:3, function(x) x*2) #1, 2, 3 -> 1*2, 2*2, 3*2
#result

## Automating 10-fold CV for a C5.0 Decision Tree using lapply() ---- l

library(caret)
library(C50) #의사결정트리 모델 사용
#install.packages("irr")
library(irr)

credit <- read.csv("credit.csv", stringsAsFactors = TRUE)

set.seed(123)

folds <- createFolds(credit$default, k = 10)
str(folds)

cv_results <- lapply(folds, function(x) {
    credit_train <- credit[-x, ]
    credit_test <- credit[x, ]
    credit_model <- C5.0(default ~ ., data = credit_train)
    credit_pred <- predict(credit_model, credit_test)
    credit_actual <- credit_test$default
    kappa <- kappa2(data.frame(credit_actual, credit_pred))$value
    return(kappa) })

str(cv_results)
