#1. 데이터를 로드한다
setwd("/Users/macbook/Documents/itwill/R")
wbcd <-  read.csv("wisc_bc_data.csv", header=T, stringsAsFactors=FALSE)
wbcd$diagnosis <- factor( wbcd$diagnosis,
                          levels= c("B","M"),
                          labels=c("Benign", "Maliganant") ) 
str(wbcd)
nrow(wbcd) #569

set.seed(1) 
sample(10) # 1부터 10까지의 숫자를 랜덤으로 섞어서 출력하는 코드
wbcd_shuffle <- wbcd[ sample(569),    ] # 설명:  wbcd[  행,  열 ]
wbcd_shuffle

wbcd2 <-  wbcd_shuffle[ , -1 ] #id삭제
str(wbcd2) 

normalize <-  function(x) {
    return  ( (x-min(x)) / ( max(x) - min(x) ) )
}

wbcd_n <- as.data.frame( lapply( wbcd2[ , 2:31], normalize)  )
nrow( wbcd_n ) # 569 

train_num <- round( 0.9 * nrow(wbcd_n), 0 ) #데이터를 9:1로 나눈다
train_num  # 512 

wbcd_train <- wbcd_n[ 1:train_num,  ]   
wbcd_test  <- wbcd_n[ (train_num+1) : nrow(wbcd_n),  ]  
nrow(wbcd_test)   # 57

wbcd_train_label <-  wbcd2[ 1:train_num,  1 ] 
wbcd_test_label <- wbcd2[ (train_num+1) : nrow(wbcd_n), 1  ] 
wbcd_test_label

# install.packages("class")
library(class)

result1 <- knn(train=wbcd_train, test=wbcd_test,   cl=wbcd_train_label, k=11)
result1

data.frame( result1, wbcd_test_label)
sum( result1 == wbcd_test_label ) #52

x <-  data.frame('실제'=wbcd_test_label, '예측'=result1)
table(x) 

actual_type <- wbcd_test_label  #테스트 데이터의 실제값
predict_type <- result1  #테스트 데이터의 예측값
positive_value <- 'Maliganant' # 관심범주(yes)
negative_value <- 'Benign'# 관심범주(no)

#■ 정확도
g <-CrossTable( actual_type, predict_type )

x <- sum(g$prop.tbl *diag(2)) #정확도 확인하는 코드
x #0.9122807

#■ 카파통계량
#install.packages("vcd")
library(vcd)

table(actual_type,predict_type)
Kappa(table(actual_type, predict_type)) #0.8224

#■ 민감도
#install.packages("caret")
library(caret)
sensitivity(predict_type, actual_type, positive=positive_value) #0.8148148

#■ 특이도
specificity(predict_type, actual_type, negative=negative_value) #1

#■ 정밀도
posPredValue(predict_type, actual_type, positive=positive_value) #1

#■ 재현율
sensitivity( predict_type, actual_type, positive=positive_value) #0.8148148


#F1 score 구하기 

library(MLmetrics)

F1_Score( actual_type,  predict_type, positive = positive_value)
