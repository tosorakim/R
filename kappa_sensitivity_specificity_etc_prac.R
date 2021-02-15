#■ 카파통계량, 정확도, 민감도(재현율), 특이도 를 출력하는 실습:

#1. 데이터를 로드한다.
setwd("/Users/macbook/Documents/itwill/R")
credit <- read.csv("credit.csv", stringsAsFactor=TRUE)
str(credit) 

#2. 데이터에 각 컬럼들을 이해한다. 

#라벨 컬럼 :  default  --->  yes : 대출금 상환 안함 
#no  : 대출금 상환 

prop.table( table(credit$default)  )
summary( credit$amount)

#3. 데이터가 명목형 데이터인지 확인해본다.

str(credit) 

#4. 데이터를 shuffle 시킨다.

set.seed(31)
credit_shuffle <-  credit[ sample( nrow(credit) ),  ]
credit_shuffle #잘 섞임

#5. 데이터를 9 대 1로 나눈다.

train_num <- round( 0.9 * nrow(credit_shuffle), 0) 

credit_train <- credit_shuffle[1:train_num ,  ]

credit_test  <- credit_shuffle[(train_num+1) : nrow(credit_shuffle),  ]
nrow(credit_train)
nrow(credit_test)

#6. C5.0 패키지와 훈련 데이터를 이용해서 모델을 생성한다.

library(C50)

credit_model <- C5.0( credit_train[ ,-17] , credit_train[  , 17] ) #17번이 라벨임
#credit_model #모델이 잘 만들어짐을 확인

#7. 위에서 만든 모델을 이용해서 테스트 데이터의 라벨을 예측한다.

credit_result <-  predict( credit_model, credit_test[  , -17] )

#8. 이원 교차표로 결과를 확인한다.

library(gmodels)

CrossTable( credit_test[   , 17], credit_result )



#■ 실제값과 예측값 대입

actual_type <- credit_test[  , 17]
predict_type <-  credit_result
positive_value <- 'yes'
negative_value <- 'no'

#■ 정확도

g <- CrossTable( actual_type, predict_type )

x <- sum(g$prop.tbl *diag(2))   # 정확도 확인하는 코드
x

#■ 카파통계량 

#install.packages("vcd")
library(vcd)

table( actual_type, predict_type)

Kappa( table( actual_type, predict_type)  ) 



#■ 민감도

head(credit_results)

#install.packages("caret")
library(caret)
sensitivity( predict_type, actual_type,
             positive=positive_value)

#■ 특이도
specificity(  predict_type, actual_type,
              negative=negative_value)  

#■ 정밀도
posPredValue( predict_type, actual_type,
              positive=positive_value) 

#■ 재현율 
sensitivity( predict_type, actual_type,
             positive=positive_value) 
