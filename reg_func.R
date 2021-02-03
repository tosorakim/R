#데이터를 수집한다
setwd("/Users/macbook/Documents/itwill/R")
reg <- read.table("regression.txt", header = T)
reg

reg_func <- function(x_num) {
    x = c(reg$growth)
    y = c(reg$tannin)
    a = cov(x,y) / var(x) #                기울기
    x_mean = mean(x)
    y_mean = mean(y)
    b = y_mean - a * x_mean #              절편 
    y_hat = b + a * x_num    #             y=b+ax (y:예상값, b:절편, a:기울기)
    print(y_hat)
}

reg_func(9)
