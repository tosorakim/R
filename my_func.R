
my_func <- function() {
      
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

       plot(xcol2,ycol2,
       main=paste(xcol,'과',ycol,'의 산포도 그래프'),,lwd=2,             xlab=xcol,ylab=ycol,col='red',pch=21,bg='red')
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


      my_pie <- function() {
             
          fname <- file.choose()
          table <- read.csv(fname, header=T, stringsAsFactor=F )
          print(data.table(colnames(table)))
          xcol_num <- as.numeric(readline('x축 컬럼 번호: '))      
          xcol <- colnames(table[xcol_num])
          xcol2 <- table[,xcol]

          label2 <- round( prop.table( table(xcol2)) * 100, digits=1)
          label3 <- paste( names(label2), label2, '%')
           pie ( label2 , label=label3, col=c("Green","skyblue") )

                           }


         x1 <- menu( c('산포도 그래프','히스토그램 그래프','사분위수 그래프',
                       '원형그래프') ,
                       title='숫자를 선택하세요 ~' )  

        switch ( x1,  
                 san1 = {   sanpodo()                  } ,
                 san2 = {   my_hist()                  } ,
                 san3 = {   my_box()                   } ,
                 san4 = {   my_pie()                   }
                )

                           }

my_func()