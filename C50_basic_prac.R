library(caret)
library(C50)
library(irr)

credit <- read.csv("credit.csv", stringsAsFactors=TRUE)
set.seed(300)

# C5.0 의 하이퍼파라미터인 trials, winnow, model 의 27개의 조합에 대한 # 각각의 정확도를 구하는 작업

m <- train( default~ . , data=credit, method="C5.0")
m # 튜닝한 결과를 확인할 수 있다.

p <- predict( m , credit )
table(p, credit$default)