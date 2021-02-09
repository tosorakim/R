#data set 을 불러옵니다
setwd("/Users/macbook/Documents/itwill/R")
academy <- read.csv("academy.csv", header = TRUE, fileEncoding = "CP949", encoding = "UTF-8")

#수학점수와 영어점수만 선택합니다
academy <- academy[  ,  c(3,4) ]
View(academy)

#k 값을 4로 주고 비지도학습 시켜 모델을 생성
km <- kmeans( academy,  4)
km


#시각화
library(factoextra)
fviz_cluster(km , data=academy,  stand=F) 
par(family="AppleGothic")

#특정집단만 추출
academy_seg <- cbind(academy[ , c(1,3,4)], km$cluster)
academy_seg[km$cluster==1, 1] #학생번호

