#SDR 구하기(2021-02-03)
#1. 원본 데이터를 만든다
raw_data <- c(1,1,1,2,2,3,4,5,5,6,6,7,7,7,7)

#2. 원본 데이터를 A 속성으로 나누었을 때
a_t1 <- c(1,1,1,2,2,3,4,5,5)
a_t2 <- c(6,6,7,7,7,7)

#3. 원본 데이터를 B 속성으로 나누었을 때
b_t1 <- c(1,1,1,2,2,3,4)
b_t2 <- c(5,5,6,6,7,7,7,7)

#4. A 속성으로 나누었을 때 SDR
sdr_a <- sd(raw_data) - (length(a_t1)/length(raw_data)*sd(a_t1)
                         + length(a_t2)/length(raw_data)*sd(a_t2))
sdr_a #1.202815

#5. B 속성으로 나누었을 때 SDR
sdr_b <- sd(raw_data) - (length(b_t1)/length(raw_data)*sd(b_t1)
                         + length(b_t2)/length(raw_data)*sd(b_t2))
sdr_b #1.392751

#6. 둘중에 SDR 이 높은 것으로 분류한다

#7. B 속성으로 분류한 원본 데이터의 두 영역의 평균값을 각각 구해서 등급을 예측한다
mean(b_t1) #2
mean(b_t2) #6.25
