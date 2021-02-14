#실습코드: 
colnames(iris)
levels(iris$Species)
nrow(iris)#150
install.packages("kohonen")
library(kohonen)

train <- sample(1:150, 100) #무작위로 100개 추출 (학습데이터)
train_Set <-  list( x = as.matrix(iris[train,-5]), Species = as.factor(iris[train,5])) #학습데이터 list형
test_Set <- list(x = as.matrix(iris[-train,-5]), Species = as.factor(iris[-train,5])) #테스트 데이터 list형

train_Set

gr <- somgrid(xdim = 3, ydim = 5, topo = "hexagonal") #grid 갯수 및 모양 설정

#토폴로지(영어: topology, 문화어: 망구성방식)는 컴퓨터 네트워크의 요소들(링크, 노드 등)을 물리적으로 연결해 놓은 것

ss <- supersom(train_Set, gr, rlen = 200, alpha = c(0.05, 0.01)) #som 학습하기
ss
#실습시 나오는 som 함수 파라미터 설명
#https://www.rdocumentation.org/packages/kohonen/versions/3.0.10/topics/supersom

plot(ss, type="changes")

plot(ss, type="count", main="Node Counts")

plot(ss, type="dist.neighbours", main = "SOM neighbour distances")

plot(ss, type="codes")

test_Set$Species

test.pred <- predict(ss,  newdata = test_Set)
test.pred

test.pred$predictions$Species

table(test.pred$predictions$Species,test_Set$Species)
