
my_func <- function() {  
  
  bar <- function() {      # R 에서 함수 만드는 코드 
    
    fname <- file.choose()  #  윈도우 탐색기 여는 코드
    
    table <- read.csv(fname, header=T, stringsAsFactor=F )
    
    print(data.table(colnames(table))) # 컬럼명을 번호와 함께 출력하는 코드
    
    xcol_num <- as.numeric(readline('x축 컬럼 번호: ')) # 번호 물어보기
    ycol_num <- as.numeric(readline('y축 컬럼 번호: ')) # 번호 물어보기 
    
    xcol <- colnames(table[xcol_num])   # x축 컬럼명을 담는 코드
    ycol <- colnames(table[ycol_num])   # y축 컬럼명을 담는 코드 
    
    xcol2 <- table[,xcol] # 예: emp[, "empno"] 
    ycol2 <- table[,ycol] #  y 축 컬럼의 데이터를 ycol2 에 넣는다. 
    y_max <- max(ycol2)  + 100
    View(table)
    barplot(ycol2, main=paste(xcol,'과',ycol,'의 막대 그래프'), names.arg=xcol2,
            col=c('Green Yellow'), density=80, ylim= c(0,y_max) , beside=T)
  }
  
  
  pie2 <- function() {      # R 에서 함수 만드는 코드 
    
    fname <- file.choose()  #  윈도우 탐색기 여는 코드
    
    table <- read.csv(fname, header=T, stringsAsFactor=F )
    
    print(data.table(colnames(table))) # 컬럼명을 번호와 함께 출력하는 코드
    
    xcol_num <- as.numeric(readline('라벨이 되는 컬럼 번호를 입력하세요: ')) # 번호 물어보기
    ycol_num <- as.numeric(readline('원형 그래프의 데이터가 될 컬럼 번호를 입력하세요: ')) # 번호 물어보기 
    
    xcol <- colnames(table[xcol_num])   # x축 컬럼명을 담는 코드
    ycol <- colnames(table[ycol_num])   # y축 컬럼명을 담는 코드 
    
    xcol2 <- table[,xcol] #  x 축 컬럼의 데이터를 xcol2 에 넣는다. 
    ycol2 <- table[,ycol] #  y 축 컬럼의 데이터를 ycol2 에 넣는다. 
    
    y_labels <- round( ycol2/ sum(ycol2) *100, 1)
    y_labels2 <-paste(xcol2, y_labels, '%')
    View(table)
    pie( ycol2 , main=paste(ycol ,'의 원형 그래프') , labels=y_labels2, col=rainbow(15) ) 
    
  }
  
  line2 <- function() {      # R 에서 함수 만드는 코드 
    
    fname <- file.choose()  #  윈도우 탐색기 여는 코드
    
    table <- read.csv(fname, header=T, stringsAsFactor=F )
    
    print(data.table(colnames(table))) # 컬럼명을 번호와 함께 출력하는 코드
    
    xcol_num <- as.numeric(readline('x축 컬럼 번호: ')) # 번호 물어보기
    ycol_num <- as.numeric(readline('y축 컬럼 번호: ')) # 번호 물어보기 
    
    xcol <- colnames(table[xcol_num])   # x축 컬럼명을 담는 코드
    ycol <- colnames(table[ycol_num])   # y축 컬럼명을 담는 코드 
    
    xcol2 <- table[,xcol] #  x 축 컬럼의 데이터를 xcol2 에 넣는다. 
    ycol2 <- table[,ycol] #  y 축 컬럼의 데이터를 ycol2 에 넣는다.
    
    y_max2 <- max(ycol2) -1
    y_max <- max(ycol2)  +3 
    View(table)
    plot( ycol2, type='o', col='blue', ylim=c(0,y_max), axes=FALSE, ann=FALSE )
    
    axis( 1,  at=1:5,  lab= xcol2 )
    axis(2)  # y 축 생성 
    legend( 2, y_max2,  table , col='blue', cex=0.8, pch=21, lty=1  ) 
    
  }



      my_hist <- function() {
           
        fname <- file.choose()
        table <- read.csv(fname, header=T, stringsAsFactor=F )

           print(data.table(colnames(table)))

       xcol_num <- as.numeric(readline('히스토그램 그래프를 그릴 컬럼을 선택하세요: '))
      
       xcol <- colnames(table[xcol_num])

           xcol2 <- table[,xcol]

          class1 <- sort(xcol2)
           hist(class1 , col="yellow", density=80,
                  main="히스토그램 정규분포 그래프" )
          par(new=T)
          plot( class1, dnorm( class1, mean=mean(class1), 
           sd=sd(class1)),type='l', axes=FALSE, ann=FALSE, 
           col="red") 
         View(table)

                           }


       
      my_box <- function() {
             
          fname <- file.choose()
          table <- read.csv(fname, header=T, stringsAsFactor=F )
          print(data.table(colnames(table)))
          xcol_num <- as.numeric(readline('박스 그래프를 그릴 컬럼번호: '))      
          xcol <- colnames(table[xcol_num])
          xcol2 <- table[,xcol]
          result <- boxplot(xcol2)
          out_value <-   length(result$out)
          box_label <- paste('아웃라이어의 갯수는:', out_value, '개 입니다')

          boxplot(xcol2, col="gray", density=80, main=box_label)

                           }


 v2 <- function() { 
     fname <- file.choose()  
     table <- read.csv(fname, header=T, stringsAsFactor=F )
     View(table) 
     print(summary(table))
    }

sanpodo <- function() {
  
  fname <- file.choose()
  table <- read.csv(fname, header=T, stringsAsFactor=F )
  
  print(data.table(colnames(table)))
  
  xcol_num <- as.numeric(readline('x축 컬럼 번호: '))
  ycol_num <- as.numeric(readline('y축 컬럼 번호: '))      
  
  xcol <- colnames(table[xcol_num])
  ycol <- colnames(table[ycol_num])
  xcol2 <- table[,xcol]
  ycol2 <- table[,ycol]

  xcol2[is.na(xcol2)]  <- 0
  ycol2[is.na(ycol2)]  <- 0
  cor_label <- cor(xcol2, ycol2)
  san_label <- paste('상관계수:', round(cor_label,2)  )

  View(table)
  plot(xcol2,ycol2,
       main= san_label,lwd=2, xlab=xcol,ylab=ycol,col='blue',pch=21,bg='blue')
}



pie3 <- function() {      # R 에서 함수 만드는 코드 

  fname <- file.choose()  #  윈도우 탐색기 여는 코드    
  table2 <- read.csv(fname, header=T, stringsAsFactor=F )
  print(data.table(colnames(table2))) # 컬럼명을 번호와 함께 출력하는 코드
  xcol_num2 <- as.numeric(readline('컬럼 번호: ')) # 번호 물어보기
  xcol_num <- colnames(table2[xcol_num2])   # x축 컬럼명을 담는 코드
  x <- table(table2[,xcol_num]) / length(table2[,xcol_num])
  x2 <- data.frame(x)
  names(x2) <- c("col1","col2")
  xcol1 <- x2$col1  # x축 컬럼명을 담는 코드
  xcol2 <- round(x2$col2,2)#  x 축 컬럼의 데이터를 xcol2 에 넣는다. 
  x_labels <- paste(xcol1,':',round( xcol2/ sum(xcol2) *100, 1),'%' )
  col_cnt <- length(unique(xcol2))
  View(table2)
  pie( xcol2 , main=paste('원형 그래프') , labels=x_labels, col=rainbow(col_cnt) ) 
  print(summary(table2))
}

 
  x1 <- menu( c('막대 그래프', '원형 그래프', '라인 그래프' ,'히스토그램 그래프', 
                  '박스 그래프', '테이블과 통계정보',
                 '산포도 그래프', '범주형 원형 그래프' ),
              title=" 숫자를 선택하세요 ~ 종료하려면 0번을 누르세요"  )

  switch( x1,
          gp1 = {   bar()   },
          gp2 = {   pie2()  },
          gp3 = {   line2() },
          gp4 = {   my_hist() },
          gp5 = {   my_box() },
          gp6 = {   v2()      },
          gp7 = { sanpodo()   },
          gp8 = {   pie3()    }   
  )
}   

my_func()
