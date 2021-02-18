
# 1. 데이터를 로드한다.
kyphosis <- read.csv("kyphosis.csv", stringsAsFactors = TRUE)
kyphosis


# 2. rpart 를 이용해서 의사 결정트리 모델을 생성한다.



library(rpart)

fit <- rpart(kyphosis ~ age + number + start, method="class", data=kyphosis)



# 3. 모델을 시각화 한다.



library(rattle)
library(rpart.plot)

fancyRpartPlot(fit)



# 4. 정확도를 확인한다.

result <- predict(fit , newdata = kyphosis)
result
sum(kyphosis$kyphosis == ifelse(result[,1]>0.5 , "absent" , "present"))/NROW(kyphosis)



# [1] 0.8395062


# 그럼 83% 로 나온다.



# 이 수치를 높이기 위해서  random forest 를 사용한다.



# 1. 랜덤 포레스트 모델을 만든다.
library(randomForest)
fit <- randomForest(kyphosis ~ age + number + start, mtry=3, ntree=10,  data=kyphosis) #ntry 갯수 늘리기
res2 <- predict(fit , newdata = kyphosis)
sum(res2 == kyphosis$kyphosis)/NROW(kyphosis)



# [1] 0.9876543  <--- 98% 로 정확도가 올라간다.
# 위에서 500개의 트리가 나오는데 이 트리들은 다 똑같은 트리는 아니고 우리는 모르게 미세하게 조금씩 파라미터가 다르다.
# 그 값들도 자동으로 함수에서 알아서 조정해준다.



#summary(fit)
#str(fit)
