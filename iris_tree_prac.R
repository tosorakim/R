#1. party 패키지를 설치한다
install.packages("party")
library(party)

#2. 내장되어있는 iris 데이터를 불러온다
data(iris)

#3. iris 데이터의 전체 행 수를 확인한다
nrow(iris) #150

#4. iris 데이터의 통계정보를 확인한다
summary(iris)

#5. iris 데이터의 라벨 컬럼의 종류와 건수를 확인한다
table(iris$Species)

#6. sample 함수를 사용해서 shuffle 도 하면서 데이터를 7:3으로 나눈다
#replace=T를 사용해서 복원추출한다
set.seed(11)
ind <- sample( 2, nrow(iris), replace=T, prob=c(0.7,0.3) )
#설명: 숫자 2는 2개로 데이터를 나누겠다는 뜻이다, nrow(iris) 로 전체 행 수를 적어주고, prob=c(0.7, 0.3)으로 7대 3으로 나누겠다고 지정한다 

ind==1
ind==2

#7. train, test data를 만들어줍니다
traindata <- iris[ind==1,] #70%에 해당하는 데이터
testdata <- iris[ind==2,] #30%에 해당하는 데이터

#정말 7:3이 맞는지 확인해봅니다
nrow(traindata) #120
nrow(testdata) #30

#8. 의사결정트리 모델 생성한다
myformula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width

iris_ctree <- ctree(myformula,data=traindata)

#9. 모델이 예측한 결과를 확인한다
predict(iris_ctree) #30개의 예측 결과들이 출력됨

#10. 예측결과와 실제 테스트 데이터의 정답과 비교해본다
table(predict(iris_ctree),traindata$Species) #6개 틀리고 다 맞췄음

#11. 모델을 프린트해서 스무고개 질문이 어떻게 되는지 확인한다
print(iris_ctree)    

#12. iris 데이터의 의사결정트리 모델을 시각화한다
plot(iris_ctree) #그래프 종류1
plot(iris_ctree,type="simple") #그래프 종류2
unique(iris$Species)

#13. 테스트 데이터 30개를 예측하고 잘 맞췄는지 확인한다
testpred <- predict(iris_ctree, newdata=testdata)
table(testpred,testdata$Species)
