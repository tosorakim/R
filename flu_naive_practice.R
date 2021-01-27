setwd("/Users/macbook/Documents/itwill/R")
flu <- read.csv("flu.csv", header = TRUE, fileEncoding = "CP949", encoding = "UTF-8", stringsAsFactors = T)
head(flu)

#[data information]
#patient_id: 환자번호
#chills: 오한
#runny_nose: 콧물
#headache: 두통
#fever: 열
#flue: 독감(정답 라벨 컬럼)

nrow(flu)
trainData <- flu[1:7, ]
testData <- flu[8, ]
View(testData)

library(e1071)
View(trainData[ ,1:6])

model3 <-naiveBayes(trainData[ ,1:6], trainData$flue, laplace = 0)
model3

result3 <- predict(model3, testData[ ,1:6])
result3

#create as a function
naive_func <- function(){
    flu <- read.csv("flu.csv", header = TRUE, fileEncoding = "CP949", encoding = "UTF-8", stringsAsFactors = T)
    trainData <- flu[1:8, -1 ]
    model3 <-naiveBayes(trainData[ ,1:4], trainData$flue, laplace = 0)
    
    #Ask symptom
    v_chills <- readline('Do you have chills? (Y/N)')
    v_runny <- readline('Do you have a runny nose? (Y/N)')
    v_headache <- readline('Do you have a headache? (STRONG/MILD/NO)')
    v_fever <- readline('Do you have a fever? (Y/N)')
test <- data.frame(chills=v_chills, runny_nose=v_runny, headache=v_headache, fever=v_fever)

result <- predict(model3, test)
#print(result)
    if (result == 'Y'){
        print('You have to see a doctor')
    }
    else {
        print('It is safe')
    }
}

naive_func()


