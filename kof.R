setwd("/Users/macbook/Documents/itwill/R")
kof <- read.csv("KOFEXM.csv")
result <- aggregate(Channel~Event_Name,kof,length)
names(result) <- c("Event_Name", "Event_Name ¼ö")
result
