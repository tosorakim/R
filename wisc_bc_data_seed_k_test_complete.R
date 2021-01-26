

wbcd <-  read.csv("wisc_bc_data.csv", header=T,  stringsAsFactors=FALSE)



wbcd$diagnosis <- factor(wbcd$diagnosis,
                         levels =c("B","M"),
                         labels = c("Benign","Maliganant"))

set.seed(1)
wbcd_shuffle <- wbcd[sample(nrow(wbcd)), ]


wbcd2 <- wbcd_shuffle[-1]



normalize <- function(x) {
    return ( (x-min(x)) / (max(x) - min(x))  )
}



wbcd_n  <- as.data.frame(lapply(wbcd2[2:31],normalize))


train_num<-round(0.9*nrow(wbcd_n),0)
wbcd_train<-wbcd_n[1:train_num,]
wbcd_test<-wbcd_n[(train_num+1):nrow(wbcd_n),]

wbcd_train_label <- wbcd2[1:train_num,1]
wbcd_test_label <- wbcd2[(train_num+1):nrow(wbcd_n),1]

wbcd_test_label 

library(class) 
set.seed(1)
result1 <- knn(train=wbcd_train, test=wbcd_test,   cl=wbcd_train_label, k=21)


x <-  data.frame('실제'=wbcd_test_label, '예측'=result1)
table(x) 

#이원교차표로 확인하기
library(gmodels)
g2 <- CrossTable(x=wbcd_test_label, y=result1)
print(g2$prop.tb[1]+g2$prop.tb[4]) #정확도
