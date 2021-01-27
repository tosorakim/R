setwd("/Users/macbook/Documents/itwill/R")
fat <- read.csv("fatliver2.csv", header = TRUE, fileEncoding = "CP949", encoding = "UTF-8", stringsAsFactors = T)
head(fat)
nrow(fat) #510

#정보획득량 구하기
library(FSelector)
wg <- information.gain(FATLIVER ~ . , fat, unit='log2')
print(wg)
