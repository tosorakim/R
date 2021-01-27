#1. iris dataset 확인합니다
head(iris) #5개
nrow(iris)
unique(iris$Species) #종의 라벨

#2. 데이터를 min/max 정규화 합니다(0~1사이의 숫자로 다 변환)
normalize <- function(x) {
    return ( (x-min(x)) / (max(x) - min(x))  )
}

iris_n  <- as.data.frame(lapply(iris_unable[1:4],normalize))
#summary(iris_n)

#3. 셔플
set.seed(62)
iris_shuffle <- iris[sample(nrow(iris2)),]
iris_shuffle

#4. 훈련데이터와 테스트데이터를 8:2로 나눕니다.
library(caret)

#훈련데이터는 80%, 테스트 데이터는 20%로 설정
#데이터가 몇번째 데이터인지 데이터를 선별하기 위해서 partition함
train_num <- createDataPartition(iris$Species, p=0.8, list=FALSE) 
#train_num #partition된 값
#str(train_num) #train_num 타입

#5. train_num과 -train_num을 만들어줌(데이터셋 구분을 위해 train/test data)
train_data <- iris[train_num, ]
test_data <- iris[-train_num, ]

nrow(train_data) #120
#train_data(120개) 외 나머지 수들을 출력하기 위해서 -train_num 값을 출력
nrow(test_data) #30



#6. 라벨을 만들어줍니다
iris_train_label <- iris_unable[train_num,5] #컬럼의 갯수만큼 숫자를 넣어줍니다(head)
iris_test_label <- iris_unable[-train_num,5]

iris_test_label

#7. knn 적용(class 사용 필요)
library(class) 
#set.seed(1)
iris_knn <- knn(train=train_data, test=test_data,
                cl=iris_train_label, k=7 ) #k=7(하이퍼 파라미터)

#8. 결과확인
x <-  data.frame('실제'=iris_test_label,
                 '예측'=iris_knn)
table(x) 

#9. 이원교차표로 모델 성능 확인하기
library(gmodels)
g2 <- CrossTable(x=iris_test_label, y=iris_knn)
g2$prop.tbl
print( g2$prop.tb[1] + g2$prop.tb[5] + g2$prop.tb[9] )  # 정확도

