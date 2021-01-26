setwd("/Users/macbook/Documents/itwill/R")
wine <- read.csv("wine.csv")
nrow(wine)
summary(wine)

#Check data information
head(wine)
nrow(wine) #148
unique(wine$Type) #"t1" "t2" "t3"
ncol(wine) #컬럼 14개

#정규화
normalize <- function(x) {
    return ( (x-min(x)) / (max(x) - min(x))  )
}

wine_n  <- as.data.frame(lapply(wine[2:14],normalize)) #숫자형 데이터만 출력함
#wine[2:14]
#summary(wine[2:14])

#3.Shuffle
set.seed(62)
wine_shuffle <- wine[sample(nrow(wine_n)), ]
#sample(nrow(wine_n))
wine_shuffle

#4.데이터가 몇번째 데이터인지 데이터를 선별하기 위해서 partition함
library(caret)
set.seed(62)

train_num <- createDataPartition(wine_shuffle$Type, p=0.8, list=FALSE) 
#train_num #partition된 값
#str(train_num) #train_num 타입

nrow(train_data) #144
nrow(test_data) #27

#5.라벨지정
train_label<-wine[train_num, 1]
test_label<- wine[-train_num, 1]
test_label

length(train_label) #134
length(test_label) #34

table(train_label)
table(test_label)

#6. class를 이용한 knn
library(class)
knn_data <- knn(train=train_data, test=test_data, cl=train_label, k=7)

#7. 결과확인
x <-  data.frame('실제'=test_label, '예측'=knn_data)
table(x) 
