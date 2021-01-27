setwd("/Users/macbook/Documents/itwill/R")
skin <- read.csv("skin.csv", header = TRUE, fileEncoding = "CP949", encoding = "UTF-8", stringsAsFactors = T)
head(skin)
nrow(skin) #30

#정보획득량 구하기
install.packages("FSelector")
library(FSelector)

wg <- information.gain(cupon_react ~ . , skin, unit='log2')
print(wg)
