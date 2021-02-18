#■ 실습1. 독일 데이터로 부스팅 실습
#1. 데이터를 로드한다.
credit <- read.csv("credit.csv", stringsAsFactor=TRUE)
str(credit) 

#2. 데이터에 각 컬럼들을 이해한다. 

#라벨 컬럼 :  default  --->  yes : 대출금 상환 안함 
#no  : 대출금 상환 
prop.table( table(credit$default)  )
summary( credit$amount)

#3. 데이터가 명목형 데이터인지 확인해본다.
str(credit) 

#4. 데이터를 shuffle 시킨다.
set.seed(31)
credit_shuffle <-  credit[ sample( nrow(credit) ),  ]

#5. 데이터를 9 대 1로 나눈다.
train_num <- round( 0.9 * nrow(credit_shuffle), 0) 
credit_train <- credit_shuffle[1:train_num ,  ]
credit_test  <- credit_shuffle[(train_num+1) : nrow(credit_shuffle),  ]
nrow(credit_train) #900
nrow(credit_test) #100

#6. 부스팅으로 성능 높이기 
install.packages("adabag")
library(adabag)
set.seed(300)
m_adaboost <- boosting( default ~ . , data=credit_train )
p_adaboost <-  predict( m_adaboost,  credit_test )
head(p_adaboost$class)
p_adaboost$confusion
table( p_adaboost$class, credit_test$default) 


#7. 정확도 확인
library(gmodels)
g <- CrossTable( credit_test$default, p_adaboost$class ) #이원교차표로 실제값 확인
x <- sum(g$prop.tbl *diag(2))   # 정확도 확인하는 코드
x
