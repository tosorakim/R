#SNS의 글로 같은 성향을 가진 사람들 분류
setwd("/Users/macbook/Documents/itwill/R")
teens <-  read.csv("snsdata.csv")
table(teens$gender)
nrow(teens)

prop.table(table(teens$gender))

table(teens$gender, useNA="ifany")


teens$age <- ifelse(teens$age>=13 & teens$age <20, teens$age, NA)

colSums(is.na(teens))

#파생변수를 female로 생성한다
teens$female <- ifelse(teens$gender=="F" & !is.na(teens$gender),  1, 0)
#no_gender 파생변수를 생성한다(성별이 없는 사람들을 위해서)
teens$no_gender <- ifelse(is.na(teens$gender),1,0)

#기존 성별데이터에 NA값이 몇 개 있는지 확인
table(teens$gender, useNA="ifany")

#여성이면 1, 아니면 0
table(teens$female, useNA="ifany")
#no_gender이면 1, 아니면 0
table(teens$no_gender, useNA="ifany")


ave_age <- ave(teens$age, teens$gradyear,   FUN=function(x) mean(x, na.rm=TRUE) )
teens$age <- ifelse( is.na(teens$age), ave_age, teens$age)
aggregate(age~gradyear, data=teens, mean, na.rm=TRUE) #결측치를제거하면서 평균을 구해라

colSums(is.na(teens)) 

interests <- teens[5:40]
interests_z <- as.data.frame(lapply(interests, scale))


set.seed(2345)
teen_clusters <- kmeans(interests_z, 5)

teen_clusters 
teen_clusters$size #각 클라스 갯수
teen_clusters$centers 
teen_clusters$cluster

#각 군집별로 female(여성)이 몇 명인지 출력
result = cbind( data.frame(teens$female, teen_clusters$cluster ) )
attach(result)
tapply( teens.female, teen_clusters.cluster, sum, na.rm=TRUE )

#각 군집별로 female(여성)이 몇 명인지 출력
res <- cbind(teens, teen_clusters$cluster)
head(res)
attach(res)
tapply(female, teen_clusters$cluster, sum)

#각 군집별로 female(여성)이 몇 명인지 출력
teens$group <- teen_clusters$cluster
result1 <- teens[teens$female==1,]
table(result1$group)

#각 군집별로 female(여성)이 몇 명인지 출력
head(teens$female)
head(teen_clusters$cluster)
x<-cbind(teens$female, teen_clusters$cluster)
head(x)
tapply(x[,1], x[,2], sum)

#각 군집별로 female(여성)이 몇 명인지 출력
tapply(teens$gender=="F",teen_clusters$cluster, sum)
