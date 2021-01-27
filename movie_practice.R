setwd("/Users/macbook/Documents/itwill/R")
movie <- read.csv("movie.csv", header = TRUE, fileEncoding = "CP949", encoding = "UTF-8")

colnames(movie) <- c("age", "gender", "job", "marry", "friend", "m_type")
head(movie)

trainData <- movie[1:38, ]
testData <- movie[39, ]
View(testData)

library(e1071)

model2 <- naiveBayes(trainData[ ,1:5], trainData$m_type, laplace=0)
model2

result2 <- predict(model2, testData[ ,1:5])
result2

#문제 193번 테스트 데이터 만들기
testData2 <- data.frame(age='20대', gender='여', job='IT', marry='NO', friend='NO')
testData2

result3 <- predict(model2, testData2, laplace=0)
result3

#문제 194번 테스트 데이터 만들기
testData3 <- data.frame(age='20대', gender='남', job='학생', marry='NO', friend='NO')
testData3

result3 <- predict(model2, testData3, laplace=0)
result3
