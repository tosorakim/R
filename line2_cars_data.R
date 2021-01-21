library(data.table)

line2 <- function() {      # R 에서 함수 만드는 코드 
    
    fname <- file.choose()  #  윈도우 탐색기 여는 코드
    
    table <- read.csv(fname, header=T, stringsAsFactor=F, fileEncoding = "CP949", encoding = "UTF-8" )
    
    print(data.table(colnames(table))) # 컬럼명을 번호와 함께 출력하는 코드
    
    xcol_num <- as.numeric(readline('x축 컬럼 번호: ')) # 번호 물어보기
    ycol_num <- as.numeric(readline('y축 컬럼 번호: ')) # 번호 물어보기 
    
    xcol <- colnames(table[xcol_num])   # x축 컬럼명을 담는 코드
    ycol <- colnames(table[ycol_num])   # y축 컬럼명을 담는 코드 
    
    xcol2 <- table[,xcol] #  x 축 컬럼의 데이터를 xcol2 에 넣는다. 
    ycol2 <- table[,ycol] #  y 축 컬럼의 데이터를 ycol2 에 넣는다.
    
    y_max2 <- max(ycol2) -1
    y_max <- max(ycol2)  +3 

    plot( ycol2, type='o', col='blue', ylim=c(0,y_max), axes=FALSE, ann=FALSE )
    
    axis( 1,  at=1:5,  lab= xcol2 )
    axis(2)  # y 축 생성 
    legend( 2, y_max2,  table , col='blue', cex=0.8, pch=21, lty=1  ) 
    
}

line2()
