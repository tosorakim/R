c <- c(10,9,1,4,10,1,7,10,3,10,1,1,6,7)
row <- c("APPLE","BACON","BANANA","CARROT","SAL","CHEESE","TOMATO")
col <- c("X","Y")
data <- matrix( c, nrow= 7, ncol=2, byrow=TRUE, dimnames=list(row,col))
data

plot(data)

library(stats)


km <- kmeans(data,  3) #3으로 군집화해서 시각화한다
km
cbind(data, km$cluster)

plot(round(km$center), col=km$center, pch=22,  bg=km$center, xlim=range(0:10),ylim=range(0:10))

par(new=T)  # 그래프 겹치기 

plot( data, col=km$cluster+1, xlim=range(0:10), ylim=range(0:10), pch=22, bg=km$cluster+1 )

library(factoextra)
km <- kmeans(data,3)
fviz_cluster( km, data = data, stand=F)

