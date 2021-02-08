setwd("/Users/macbook/Documents/itwill/R")
paper <- read.csv("paper1.csv" , header = TRUE, fileEncoding = "CP949", encoding = "UTF-8")
nrow(paper)

paper[is.na(paper)] <- 0 
paper 

rownames(paper) <- paper[,1] 
paper <- paper[-1]
paper2 <- as.matrix(paper) 
paper2

book_h <- read.csv("book_hour.csv" , header = TRUE, fileEncoding = "CP949", encoding = "UTF-8")
book_h

#visualization
library(sna)
x11()#창을 새로 하나 띄움

gplot(paper2 , displaylabels = T, boxed.labels = F , vertex.cex = sqrt(book_h[,2]) , vertex.col = "blue" , vertex.sides = 20 ,
      edge.lwd = paper2*2 , edge.col = "green" , label.pos = 3)
par(family="AppleGothic") #그래프의 한글
