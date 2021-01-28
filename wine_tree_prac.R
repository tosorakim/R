setwd("/Users/macbook/Documents/itwill/R")
wine <- read.csv("wine.csv", header = TRUE,  stringsAsFactors = T)
nrow(wine) #178
head(wine)
summary(wine)
library(party)

wine$Type

table(wine$Type)
#t1 t2 t3 
#59 71 48 

#세원오빠 : seed 추가
set.seed(828)
ind <- sample( 2, nrow(wine), replace=T, prob=c(0.7,0.3) )
ind
ind==1
ind==2

trainData <- wine[ind==1,] #70%에 해당하는 데이터
testData <- wine[ind==2,] #30%에 해당하는 데이터

nrow(trainData) #132
nrow(testData) #46

myFormula <- Type ~ .
#myFormula = Type ~ Alcohol + Malic + Ash + Alcalinity + Magnesium + Phenols + Flavanoids + Nonflavanoids + Proanthocyanins + Color + Hue + Dilution + Proline

wine_ctree <- ctree(myFormula,data=trainData)
predict(wine_ctree)

table(predict(wine_ctree),trainData$Type)
#아래는 seed 설정안했을때 결과
#   t1 t2 t3
#t1 37  0  0
#t2  2 48  0
#t3  2  1 30

print(wine_ctree)
plot(wine_ctree,type="simple") 

testPred <- predict(wine_ctree, newdata=testData)
table(testPred,testData$Type)
#아래는 seed 설정안했을때 결과
#testPred t1 t2 t3
#      t1 16  1  0
#      t2  0 19  1
#      t3  2  2 17

#정민씨 : 이원교차표 확인
#library(gmodels)
#g<-CrossTable(testData[  , 1], testPred ) 
#g


#결오빠: 정확도 확인 추가
result <- predict(wine_ctree,testData[,-1])

g2 <- CrossTable(result,testData$Type)
sum(g2$prop.tbl*diag(3))
# [1] 1 정확도

plot(wine_ctree)
