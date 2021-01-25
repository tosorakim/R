my_func <- function() {  
    
    bar <- function() {    
        fname <- file.choose()
        table <- read.csv(fname, header=T, stringsAsFactor=F,  fileEncoding = "CP949", encoding = "UTF-8" )
        
        print(data.table(colnames(table)))
        
        xcol_num <- as.numeric(readline('x축 컬럼 번호: '))
        ycol_num <- as.numeric(readline('y축 컬럼 번호: '))      
        
        xcol <- colnames(table[xcol_num])
        ycol <- colnames(table[ycol_num])
        
        xcol2 <- table[,xcol]
        ycol2 <- table[,ycol]
        y_max <- max(ycol2)  + 100
        
        barplot(ycol2, main=paste(xcol,'과',ycol,'의 막대 그래프'), names.arg=xcol2,
                col=c('Green Yellow'), density=80, ylim= c(0,y_max), beside=T)
        par(family="AppleGothic")
    }
    
    
    pie2 <- function(){
        
        fname <- file.choose()
        
        table <- read.csv(fname, header=T, stringsAsFactor=F, fileEncoding = "CP949", encoding = "UTF-8")
        
        print(data.table(colnames(table)))
        
        xcol_num <- as.numeric(readline('x축 컬럼 번호: '))
        ycol_num <- as.numeric(readline('y축 컬럼 번호: '))
        
        xcol <- colnames(table[xcol_num])
        ycol <- colnames(table[ycol_num])
        
        xcol2 <- table[,xcol]
        ycol2 <- table[,ycol]
        
        y_labels <- round( ycol2/ sum(ycol2) *100, 1)
        y_labels2 <-paste(xcol2, y_labels, '%')
        
        pie( ycol2 , main=paste(ycol ,'의 원형 그래프') , labels=y_labels2, col=rainbow(15) )
        
    }
    
    x1 <- menu( c('막대 그래프', '원형 그래프'),
                title=" 숫자를 선택하세요 ~ "  )
    switch( x1,
            gp1 = {   bar()   },
            gp2 = {   pie2()  }
    )
}   

my_func()
par(family="AppleGothic")
