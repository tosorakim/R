install.packages("C50")
library(C50)

setwd("/Users/macbook/Documents/itwill/R")
skin <- read.csv("skin.csv", header = TRUE, fileEncoding = "CP949", encoding = "UTF-8", stringsAsFactors = T)
head(skin)
nrow(skin) #30

skin_real_test_cust <- skin[30, ] #테스트용으로 따로 분리한다
skin2 <- skin[ 1:29, ] #29개의 데이터로 학습시켜서 의사결정트리를 생성한다
nrow(skin2) #29

skin_real_test_cust


skin2 <- skin2[ , -1] # 고객번호를 제외시킨다

set.seed(20)
skin2_shuffle <- skin2[sample(nrow(skin2)), ] # shuffle시킴

#7:3으로 train, test data를 만든다
train_num #20
train_num <- round(0.7 * nrow(skin2_shuffle), 0)
skin2_train <- skin2_shuffle[1:train_num, ]
skin2_test <- skin2_shuffle[(train_num+1) : nrow(skin2_shuffle), ]

nrow(skin2_train) # 20
nrow(skin2_test) # 9

#C50 패키지를 이용해서 분류 모델을 생성한다
#library(C50)
#skin_model <- C5.0(skin2_train[ , -6], skin2_train$cupon_react )
#skin_model
#plot(skin_model) ---- 안되네요

#skin2_result <- predict( skin_model, skin2_test[ , -6])

#이원 교차표로 결과를 확인한다
#library(gmodels)
#CrossTable( skin2_test[ , 6], skin2_result )

#정확도를 확인해본다
#library(gmodels)
#g2 <- CrossTable( skin2_test[ , 6], skin2_result )

#정확도를 높이기 위해 trials=10 를 skin_model2 변수에 넣어본다
library(C50)
skin_model2 <- C5.0(skin2_train[ , -6], skin2_train$cupon_react, trials = 10 )
skin_model2
#예측값을 skin_result2 변수에 다시 넣어주고
skin2_result2 <- predict( skin_model, skin2_test[ , -6])

#이원 교차표로 결과를 확인한다
library(gmodels)
g2 <- CrossTable( skin2_test[ , 6], skin2_result2 )
