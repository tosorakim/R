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


#6. C5.0 패키지와 훈련 데이터를 이용해서 모델을 생성한다.

library(C50)

credit_model <- C5.0( credit_train[ ,-17] , credit_train[  , 17] )

#7. 위에서 만든 모델을 이용해서 테스트 데이터의 라벨을 예측한다.

credit_result <-  predict( credit_model, credit_test[  , -17] )

#8. 이원 교차표로 결과를 확인한다.

library(gmodels)

CrossTable( credit_test[   , 17], credit_result )




#■ 실제값과 예측값 대입

credit_test_prob <- predict(credit_model, credit_test[   , -17], type = "prob")


credit_test_prob


# combine the results into a data frame

credit_results <- data.frame(actual_type =credit_test[  , 17],
                             predict_type = credit_result,
                             prob_yes = round(credit_test_prob[ , 2], 5),
                             prob_no = round(credit_test_prob[ , 1], 5))



credit_results


#3. 예측 데이터 프레임을 csv 로 저장합니다.

# uncomment this line to output the sms_results to CSV

write.csv(credit_results, "final_results.csv", row.names = FALSE)



#■ 실제값과 예측값 대입

actual_type <- credit_test[  , 17]
predict_type <-  credit_result
positive_value <- 'yes'
negative_value <- 'no'


#■ ROC 곡선 그리기 

#install.packages("ROCR")
library(ROCR)
head(credit_results) # 3번째 컬럼과 4번째컬럼의 확률을 확인한다.
pred <- prediction(predictions = credit_results$prob_yes,              
                   labels = credit_results$actual_type)
pred 

# ROC curves

perf <- performance(pred, measure = "tpr", x.measure = "fpr")

plot(perf, main = "ROC curve for SMS spam filter", col = "blue", lwd = 2)

# add a reference line to the graph
# 대각선 출력 

abline(a = 0, b = 1, lwd = 2, lty = 2)

# calculate AUC
perf.auc <- performance(pred, measure = "auc")
str(perf.auc)
unlist(perf.auc@y.values)
########인도분 코드########
# 정확도와 cutoff 출력하는 부분 
# perf <- performance(pred, measure = "tpr", x.measure = "fpr")
eval <- performance(pred,"acc")  # y 축을 정확도로 출력
eval  # 392개의 데이터 포인트 추출 
plot(eval)

#설명:  h는 수평선, v 가 수직선의 지점 

#Identifying the best cutoff  and Accuracy
slot(eval,"y.values") # 392개의 데이터 포인트를 출력 
max <- which.max(slot(eval,"y.values")[[1]])   # 392개의 데이터 포인트중에 max 값에 해당하는
# 인덱스 번호 출력 

max  # 61번째 인덱스 번호에 해당하는 데이터가 가장 큰 값

acc <- slot(eval,"y.values")[[1]][[max]]  #   y축에 정확도중에 61번째에 해당하는 값을 출력
cut <- slot(eval,"x.values")[[1]][[max]]  #   x축에 cutoff 값들중에서 61번째 해당하는 값을 출력
print(c(Accuracy=acc, Cutoff = cut))

#■ x축을 fpr하고, y축을 tpr하는 2차원 그래프에 cut-off지점을 시각화 하는 코드
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
max
tpr <- slot(perf,"y.values")[[1]][[max]]
fpr <- slot(perf,"x.values")[[1]][[max]]
print(c(tpr,fpr))
abline(h=0.37, v=0.028)
