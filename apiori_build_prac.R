setwd("/Users/macbook/Documents/itwill/R")
build <- read.csv("building.csv" , header = TRUE, fileEncoding = "CP949", encoding = "UTF-8")
nrow(build)

build[is.na(build)] <- 0  
build
build[1] #건물번호
build <- build[-1] #건물번호 삭제
build

#행렬로 변환하기
library(arules)
trans <- as.matrix(build , "Transaction")
trans

rules1 <- apriori(trans , parameter = list(supp=0.2 , conf = 0.6 , target = "rules"))
rules1 

inspect(sort(rules1))
View(inspect(sort(rules1)))

#여러 규칙들중에서 보습학원 부분만 따로 검색했을때(신뢰도가 0.7이상)
rules2 <- subset(rules1 , subset = lhs %pin% '보습학원' & confidence > 0.7)
inspect(sort(rules2)) 

#여러 규칙들중에서 편의점 부분만 따로 검색했을때(신뢰도가 0.7이상)
rules3 <- subset(rules1 , subset = rhs %pin% '편의점' & confidence > 0.7)
rules3
inspect(sort(rules3)) 

#visualization
b2 <- t(as.matrix(build)) %*% as.matrix(build) 
library(sna)
library(rgl)
b2.w <- b2 - diag(diag(b2))
gplot(b2.w , displaylabel=T , vertex.cex=sqrt(diag(b2)) , vertex.col = "green" , edge.col="blue" , boxed.labels=F , arrowhead.cex = .3 , label.pos = 3 , edge.lwd = b2.w*2) 
par(family="AppleGothic")
