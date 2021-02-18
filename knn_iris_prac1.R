#■ knn의 k 값을 자동으로 알아내는 방법
colnames(iris)
levels(iris$Species)

set.seed(1)
train <- sample(1:150, 100) #무작위로 100개 추출 (학습데이터)
train_Set <- iris[train, ] #학습데이터 list형
test_Set <- iris[-train, ] #테스트 데이터 list형
nrow(train_Set) #100
nrow(test_Set) #50

## Do 5 repeats of 10-Fold CV for the iris data. We will fit
## a KNN model that eval‎uates 12 values of k and set the seed ## at each iteration. 

set.seed(123)
seeds <- vector(mode = "list", length = 51)
seeds

for(i in 1:50) seeds[[i]] <- sample.int(1000, 22)
seeds

## For the last model:

seeds[[51]] <- sample.int(1000, 1)
seeds

ctrl <- trainControl(method = "repeatedcv", repeats = 5, seeds = seeds)

set.seed(1)
mod <- train(Species ~ ., data = train_Set, method = "knn", tuneLength = 12, trControl = ctrl)
mod


test.pred <- predict(mod, newdata = test_Set)
test.pred

table(test.pred,test_Set$Species)

# 정확도 확인 

library(gmodels)
g <- CrossTable( test_Set$Species, test.pred )
x <- sum(g$prop.tbl *diag(3)) # 정확도 확인하는 코드
x