# 타이타닉 생존자 분류 

# 1. 데이터 로드 
setwd("/Users/macbook")
tat <- read.csv("tatanic.csv", stringsAsFactors = TRUE)
View(tat)
head(tat)

#survived : 생존=1, 죽음=0
#pclass : 승객 등급. 1등급=1, 2등급=2, 3등급=3
#sibsp : 함께 탑승한 형제 또는 배우자 수
#parch : 함께 탑승한 부모 또는 자녀 수
#ticket : 티켓 번호
#cabin : 선실 번호
#embarked : 탑승장소 S=Southhampton, C=Cherbourg, Q=Queenstown

#2.결측치 확인 

colSums( is.na(tat) ) 
?mean

tat$age[is.na(tat$age)] <- mean(tat$age,na.rm=TRUE)# null 값 제끼고 평균값 구해라

#3.이상치 확인 

library(outliers)

grubbs.flag <- function(x) {
    outliers <- NULL
    test <- x
    grubbs.result <- grubbs.test(test)
    pv <- grubbs.result$p.value
    while(pv < 0.05) {
        outliers <- c(outliers,as.numeric(strsplit(grubbs.result$alternative," ")[[1]][3]))
        test <- x[!x %in% outliers]
        grubbs.result <- grubbs.test(test)
        pv <- grubbs.result$p.value
    }
    return(data.frame(X=x,Outlier=(x %in% outliers)))
}

wisc <- read.csv("tatanic.csv")

for (i in c(2,3,5,6,8)){
    
    a = grubbs.flag(wisc[,colnames(wisc)[i]])
    b = a[a$Outlier==TRUE,"Outlier"]
    print ( paste( colnames(wisc)[i] , '--> ',  length(b) )  )
    
}


#4. 랜덤포레스트로 분류합니다.
library(caret)
library(C50)
library(irr)

nrow(tat) #891

set.seed(123)
tat_shuffle <- sample(1:891, 891)
tat_shuffle #랜덤 셔플링을하고,
tat2 <- tat[tat_shuffle,]
tat2

set.seed(123)
in_train <- createDataPartition(tat2$survived, p = 0.75, list = FALSE)
tat_train <- tat2[in_train, ] # 훈련 데이터 구성
tat_test <- tat2[-in_train, ] # 테스트 데이터 구성
nrow(tat_train #669)
nrow(tat_test) #222

m <- train( survived~ . ,
            data=tat_train, method="rf" ) #rf=랜덤포레스트

# 랜덤포레스트: 의사결정트리 + 앙상블 기법 
m # 튜닝한 결과를 확인할 수 있다.

p <- predict( m , tat_test )
p #0인지, 1인지 확률로 나오는중
round(p) #round로 둘러줘야 0.5이상인것은 1이 되게 함

table(round(p), tat_test$survived) #다맞춤. 대박

library(gmodels)
y <- CrossTable(tat_test$survived ,round(p) )
sum(y$prop.tbl * diag(2)) #100%
