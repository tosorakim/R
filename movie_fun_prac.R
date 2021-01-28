movie_fun<-function(){
    library(e1071)
    setwd("/Users/macbook/Documents/itwill/R")
    movie<-read.csv("movie.csv", header=T, stringsAsFactors = TRUE,  fileEncoding = "CP949", encoding = "UTF-8")
    #head(movie)
    #nrow(movie) #39
    names(movie) <- c("age","gender","job","marrige","fiend","genre")
    train_data<-movie[1:38, ]
    #train_data
    
    model<-naiveBayes(train_data[ ,1:5] , train_data$genre)
    
    v_gender<-readline('성별을 입력하세요(여/남) : ')
    v_age<-readline('나이대를 입력하세요(20대/30대/40대): ')
    v_job<-readline('직업을 입력하세요(IT/디자이너/무직/언론/영업/자영업/학생/홍보마케팅): ')
    v_marrige<-readline('결혼여부를 입력하세요(YES/NO) :  ')
    v_friend<-readline('이성친구여부를 입력하세요(YES/NO) : ')
    test <- data.frame(age=v_age,gender=v_gender, job=v_job, marrige=v_marrige, friend=v_friend)
    result<-predict(model, test)
    
    print(result)
}

movie_fun()
