#1. 데이터를 수집한다
setwd("/Users/macbook/Documents/itwill/R")
reg <- read.table("regression.txt", header = T)
reg

#2. 데이터를 산포도 그래프로 시각화한다
attach(reg)
plot(growth ~ tannin, data = reg, pch = 21, col = 'blue', bg = 'blue')

#3. 회귀분석을 해서 회귀계수인 기울기와 절편을 구합니다.
#lm: 회귀함수, growth 종속변수, tannin: 독립변수
model <- lm(growth ~ tannin, data = reg)
model

#4. 회귀직선을 abline으로 시각화 합니다
abline(model, col = 'red')

#5. 그래프 제목을 회귀 직선의 방정식으로 출력되게 합니다.
a<-model$coefficients[2] #기울기
b<-model$coefficients[1] #절편

par(family="AppleGothic")
title(paste('성장률=', a, 'x탄닌+', b))