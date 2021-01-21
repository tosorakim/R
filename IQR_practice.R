setwd("/Users/macbook/Documents/itwill/R/9781788295864_Code/Chapter02")

usedcars<-read.csv("usedcars.csv")
View(usedcars)

#전체 건수 확인
nrow(usedcars)

#컬럼 개수 확인
ncol(usedcars)

#각 컬럼들의 데이터에 대한 통계 정보
summary(usedcars)

#중고차 가격에 대해서 히스토그램 그래프를 그리면,
hist(usedcars$price)

#breaks 옵션을 사용해서 간격을 조정해보면,
hist(usedcars$price, breaks=seq(0,26000,by=2000), col="grey", border="white")

#커스텀 옵션을 주면,
hist(usedcars$price, at=seq(0,24000,by=2000), breaks=seq(0,26000,by=2000), col=rainbow(11), density = 80, angle = 90, border="black")

text(x$mids, x$counts, labels=x$counts, adj=c(0.5,-0.5))


hist(usedcars$mileage, col=rainbow(11), density = 80, angle = 90, border="black")


a<-hist(usedcars$mileage, at=seq(0,160000,by=20000),breaks=seq(0,160000,by=20000),col=rainbow(20), density = 60, angle = 90, border="black" )
text(a$mids,a$counts+1,labels=a$counts,'%')


hist(usedcars$mileage, axes=F)

axis(1, at=seq(0,160000, 20000))

axis(2)



usedcars<-read.csv("usedcars.csv")

a<-boxplot(usedcars$price)
a$stats
#이상치 확인
a$out

quantile(usedcars$price)

IQR(usedcars$price)















