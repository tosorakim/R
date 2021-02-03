#OneR 패키지를 설치한다
library(OneR)
#데이터를 수집한다
setwd("/Users/macbook/Documents/itwill/R")
mushroom <- read.csv("mushrooms.csv", header=T, stringsAsFactors=TRUE)
nrow(mushroom) #8124
ncol(mushroom) #23
str(mushroom) #type이 ㄷ정답 lavels 컬럼이며 전부 factor 로 변환되어 있습니다

#하이퍼 파리미터를 지정후,
set.seed(11)

#테스트 데이터와 훈련 데이터를 나눈다
dim(mushroom) #8124   23
train_cnt <- round(0.75*dim(mushroom)[1])
train_index <- sample(1:dim(mushroom)[1], train_cnt, replace=F) #비복원추출

mushroom_train <- mushroom[train_index,  ]
mushroom_test  <- mushroom[-train_index, ]
nrow(mushroom)

#모델 생성한다
model1 <- OneR(type~. ,  data=mushroom_train)



model1

summary(model1)

#결과를 확인한다
result1 <- predict( model1, mushroom_test[   , -1] )



library(gmodels)



CrossTable( mushroom_test[ , 1],  result1)  #이원교차표


#정확도를 구한다
g_m <- CrossTable(mushroom_test[ ,1], result1)
print(sum(g_m$prop.tbl*diag(2))) #0.9862137

