library(party) 
library(gmodels) 

setwd("/Users/macbook/Documents/itwill/R")
credit <- read.csv("credit.csv", header = TRUE,  stringsAsFactors = T)
View(credit)
str(credit) #check factor

colSums(is.na(credit)) #결측치가 다 0 이다

table(credit$default)

set.seed(659) #최적의 seed값
#set.seed(123)
credit_shuffle <-  credit[ sample(1000),  ]
nrow(credit_shuffle)

#6. 데이터를 9대 1로 나눕니다. (훈련 데이터: 9, 테스트 데이터 : 1 )

train_num <-   0.9 * 1000
credit_train <- credit_shuffle[ 1:train_num,   ] #1번부터 900번째행까지
credit_test  <- credit_shuffle[ (train_num+1) : nrow(credit_shuffle),   ]
#  901번부터 1000번까지는 테스트 데이터로 구성 

nrow(credit_train)  #900
nrow(credit_test)   #100

# 7. 의사결정트리 모델을 생성(party 패키지의 ctree함수 이용)
myformula <- default ~ .
credit_ctree <- ctree(myformula, data=credit_train)

predict(credit_ctree)
length(predict(credit_ctree))

table(predict(credit_ctree), credit_train$default)

test_pred <- predict(credit_ctree, newdata=credit_test)
table(test_pred, credit_test$default)

plot(credit_ctree)
plot(credit_ctree, type='simple')
