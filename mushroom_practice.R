setwd("/Users/macbook/Documents/itwill/R")
mushroom <- read.csv("mushrooms.csv", header=T, stringsAsFactors=TRUE)
#True가 의미하는 바는? 문자형 데이터를 전부 팩터로 바꿔버리겠다.
View(mushroom)
str(mushroom) #을 해보면 모두 팩터로 바뀐것을 확인 할 수 있다

unique(mushroom$type)

par(family="AppleGothic")
r()


prop.table(table(mushroom$type))


mush_test <- mushroom[8123, ]
mush_test

write.csv( mush_test, "mush_test.csv",row.names=FALSE )
getwd()
nrow(mushroom)
mushrooms <- mushroom[ -8123, ]
nrow(mushrooms)

dim(mushrooms)
dim(mushrooms)[1]
dim(mushrooms)[2]


train_cnt <- round( 0.75*dim(mushrooms)[1] )
train_cnt


train_index <- sample( 1:dim(mushrooms)[1], train_cnt, replace=F)
mushrooms_train <- mushrooms[ train_index,  ]
mushrooms_test <- mushrooms[- train_index, ]

nrow(mushrooms_train) #6092
nrow(mushrooms_test) #2031
#str(mushrooms_train)

install.packages("e1071")
library(e1071) #모든 컬럼들
model1 <- naiveBayes(type~ . , data=mushrooms_train)
model1
result1 <- predict( model1, mushrooms_test[ , -1] ) #mushrooms_test[ ,-1]은 정답을 뺀 테스트 데이터 입니다

result1

library(gmodels)
CrossTable( mushrooms_test[ ,1], result1)


#정확도를 보고 싶다면 크로스테이블을 변수에 넣는다,
g2 <- CrossTable( mushrooms_test[ ,1], result1)
print(g2$prop.tb[1] + g2$prop.tb[4]) #정확도


model2 <- naiveBayes(type~ . , data=mushrooms_train, laplace=0.0004)
result2 <- predict( model2, mushrooms_test[ , -1] )
g3 <- CrossTable( mushrooms_test[ ,1], result2)
print(g3$prop.tb[1] + g3$prop.tb[4]) #정확도:0.9945


model2 <- naiveBayes(type~ . , data=mushrooms_train, laplace=0.00001)
result2 <- predict( model2, mushrooms_test[ , -1] )
g3 <- CrossTable( mushrooms_test[ ,1], result2)
print(g3$prop.tb[1] + g3$prop.tb[4]) #정확도

#laplace=0.00001 #[1] 0.9980305
#laplace=0.000001 #[1] 0.9980305
#laplace=0.00000000000001 #[1] 0.9980305

result3 <- predict( model2, mush_test[ ,-1] )
result3 #edible poisonous
