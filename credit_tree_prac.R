setwd("/Users/macbook/Documents/itwill/R")
credit <- read.csv("credit.csv", header = TRUE,  stringsAsFactors = T)
str(credit)

table(credit$default)
prop.table(table(credit$default))

summary(credit$amount)


set.seed(123)
credit_shuffle <- credit[ sample(nrow(credit)), ]
nrow(credit_shuffle) #1000
nrow(credit)

train_num <- 0.9*1000
credit_train <- credit_shuffle[ 1:train_num, ]
credit_test <- credit_shuffle[ (train_num+1) : nrow(credit_shuffle), ]

nrow(credit_train) #900
nrow(credit_test) #1000

library(C50) 
str(credit_train)
ncol(credit_train)

credit_model <- C5.0( credit_train[ ,-17], credit_train$default)
credit_model

credit_result <- predict( credit_model, credit_test[ , -17])
credit_result
table(credit_result)
table(credit_test[  , 17])

table(credit_test[  , 17], credit_result)

library(gmodels)
CrossTable(x=credit_test[ ,17], y=credit_result)

diag(2)
print(sum(x$prop.tbl*diag(2)))


#모델 다시 세우고
credit_model2 <- C5.0( credit_train[ , -17], credit_train$default, trials=100 )

#예측 다시 세우고
credit_result2 <- predict( credit_model2, credit_test[ , -17])

#정확도 확인
x <- CrossTable(x=credit_test[ ,17], y=credit_result)
print(sum(x$prop.tbl*diag(2)))
