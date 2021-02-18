#1. 데이터를 로드한다.
data(iris)
head(iris)

#2. 데이터에 각 컬럼들을 이해한다. 
#라벨 컬럼 :  default  --->  yes : 대출금 상환 안함 
#no  : 대출금 상환 

prop.table( table(iris$Species)  )


#3. 데이터가 명목형 데이터인지 확인해본다.
str(iris) 

#4. 데이터를 shuffle 시킨다.
set.seed(31)
iris_shuffle <-  iris[ sample( nrow(iris) ),  ]

#5. 데이터를 9 대 1로 나눈다.
train_num <- round( 0.8 * nrow(iris_shuffle), 0) 
iris_train <- iris_shuffle[1:train_num ,  ]
iris_test  <- iris_shuffle[(train_num+1) : nrow(iris_shuffle),  ]
nrow(iris_train) # 120
nrow(iris_test)  # 130


#6. 부스팅으로 성능 높이기 
#install.packages("adabag")
library(adabag)
set.seed(300)  # 데이터를 복원추출하기 때문에 필요하다. 

head(iris)
head(iris_train)

#m_adaboost <- boosting( Species ~ . , data=iris_train )
m_adaboost <- boosting( Species ~ . , data=iris_train, boos=TRUE, mfinal=3 )

p_adaboost <-  predict( m_adaboost,  iris_test )

head(p_adaboost$class)
p_adaboost$confusion
table( p_adaboost$class, iris_test$Species) 

#7. 정확도 확인
library(gmodels)
g <- CrossTable( iris_test$Species, p_adaboost$class )
g

x <- sum(g$prop.tbl * diag(3))   # 정확도 확인하는 코드
x

?boosting
