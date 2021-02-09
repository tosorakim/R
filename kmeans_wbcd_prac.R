setwd("/Users/macbook/Documents/itwill/R")
wbcd <- read.csv("wisc_bc_data.csv", header=T, stringsAsFactors=FALSE)
#str(wbcd)
#plot(wbcd)
head(wbcd)
ncol(wbcd)

wbcd2 <- wbcd[ , 3:32]

km <- kmeans(wbcd2,  2) 
km
cbind(wbcd$diagnosis, km$cluster)

library(factoextra)
km <- kmeans(wbcd2, 2)
fviz_cluster( km, data = wbcd2, stand=F)

#준혁이가 만든 정답과 비교하는 코드:
wisc <- read.csv("wisc_bc_data.csv")
wisc$diagnosis <- factor(wisc$diagnosis,
                         level=c("B","M"),
                         labels = c(1,2))
#View(wisc)

wisc_km <- wisc[,c(-1,-2)]
#View(wisc_km)

km <- kmeans(wisc_km, 2)

cbind(wisc$diagnosis, km$cluster)
fviz_cluster(km, data = wisc_km)

library(gmodels)
CrossTable(wisc$diagnosis, km$cluster)
