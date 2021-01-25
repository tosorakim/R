
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
    
    barplot(ycol2, main=paste(xcol,'과',ycol,'의 막대 그래프'), names.arg=xcol2,
            col=c('Green Yellow'), density=80, ylim= c(0,y_max) , beside=T)
  }
  
  
  pie2 <- function() {      # R 에서 함수 만드는 코드 
    
    fname <- file.choose()  #  윈도우 탐색기 여는 코드
    
    table <- read.csv(fname, header=T, stringsAsFactor=F )
    
    print(data.table(colnames(table))) # 컬럼명을 번호와 함께 출력하는 코드
    
    xcol_num <- as.numeric(readline('x축 컬럼 번호: ')) # 번호 물어보기
    ycol_num <- as.numeric(readline('y축 컬럼 번호: ')) # 번호 물어보기 
    
    xcol <- colnames(table[xcol_num])   # x축 컬럼명을 담는 코드
    ycol <- colnames(table[ycol_num])   # y축 컬럼명을 담는 코드 
    
    xcol2 <- table[,xcol] #  x 축 컬럼의 데이터를 xcol2 에 넣는다. 
    ycol2 <- table[,ycol] #  y 축 컬럼의 데이터를 ycol2 에 넣는다. 
    
    y_labels <- round( ycol2/ sum(ycol2) *100, 1)
    y_labels2 <-paste(xcol2, y_labels, '%')
    
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
    y_max 
    plot( ycol2, type='o', col='blue', ylim=c(0,y_max), axes=FALSE, ann=FALSE )
    
    axis( 1,  at=1:5,  lab= xcol2 )
    axis(2)  # y 축 생성 
    legend( 2, y_max2,  table , col='blue', cex=0.8, pch=21, lty=1  ) 
    
  }



      my_hist <- function() {
           
        fname <- file.choose()
        table <- read.csv(fname, header=T, stringsAsFactor=F )

           print(data.table(colnames(table)))

       xcol_num <- as.numeric(readline('x축 컬럼 번호: '))
      
       xcol <- colnames(table[xcol_num])

           xcol2 <- table[,xcol]

          class1 <- sort(xcol2)
           hist(class1 , col="yellow", density=80,
                  main="히스토그램 정규분포 그래프" )
          par(new=T)
          plot( class1, dnorm( class1, mean=mean(class1), 
           sd=sd(class1)),type='l', axes=FALSE, ann=FALSE, 
           col="red") 

                           }


       
      my_box <- function() {
             
          fname <- file.choose()
          table <- read.csv(fname, header=T, stringsAsFactor=F )
          print(data.table(colnames(table)))
          xcol_num <- as.numeric(readline('x축 컬럼 번호: '))      
          xcol <- colnames(table[xcol_num])
          xcol2 <- table[,xcol]

          boxplot(xcol2, col="green", density=80 )

                           }


 v2 <- function() { 
     fname <- file.choose()  
     table <- read.csv(fname, header=T, stringsAsFactor=F )
     View(table) 
    }

 
  x1 <- menu( c('막대 그래프', '원형 그래프', '라인 그래프' ,'히스토그램 그래프', '박스 그래프', '테이블', '산포도 그래프' ),
              title=" 숫자를 선택하세요 ~ "  )

  switch( x1,
          gp1 = {   bar()   },
          gp2 = {   pie2()  },
          gp3 = {   line2() },
          gp4 = {   my_hist() },
          gp5 = {   my_box() },
          gp6 = {   v2()      }  
          gp7 = { sanpodo() }
  )
}   

my_func()
