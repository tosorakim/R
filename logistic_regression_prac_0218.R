# Logistic Regression
# Read data file
setwd("/Users/macbook/Documents/itwill/R")
mydata <- read.csv("binary.csv", header = TRUE)
str(mydata)

mydata$admit <- as.factor(mydata$admit)
mydata$rank <- as.factor(mydata$rank)

# Two-way table of factor variables
xtabs(~admit + rank, data = mydata)
nrow(mydata)

# Partition data - train (80%) & test (20%)
set.seed(1234)
ind <- sample(2, nrow(mydata), replace = T, prob = c(0.8, 0.2))
train <- mydata[ind==1,]
test <- mydata[ind==2,]

# Logistic regression model
mymodel <- glm(admit ~ gpa + rank, data = train, family = 'binomial')
summary(mymodel)

# Prediction
p1 <- predict(mymodel, train, type = 'response')
head(p1)
head(train)

# Misclassification error - train data
pred1 <- ifelse(p1>0.5, 1, 0)
tab1 <- table(Predicted = pred1, Actual = train$admit)
tab1
1 - sum(diag(tab1))/sum(tab1)

# Misclassification error - test data
p2 <- predict(mymodel, test, type = 'response')
pred2 <- ifelse(p2>0.5, 1, 0)
tab2 <- table(Predicted = pred2, Actual = test$admit)
tab2
1 - sum(diag(tab2))/sum(tab2)

# Goodness-of-fit test
with(mymodel, pchisq(null.deviance - deviance, df.null-df.residual, lower.tail = F))

##

###Caret Packages -- train() 
##0.1 Load data
binary <- read.csv("binary.csv")
binary$admit <- factor( binary$admit, labels=c("Y", "N"))
str(binary)

##0.2 Devide Db
set.seed(1)
library(caret)
k <- createDataPartition(binary$admit, p=0.80, list=F) #데이터를 나누는 작업을 먼저 진행 합니다.
trainset <- binary[k, ]
testset <- binary[-k, ]
nrow(trainset)    #321
nrow(testset)    #79


##3. svmLinear ---------------------------------------------
install.packages("kernlab")
library(kernlab) #caret을 이용한 자동튜닝을 진행합니다

ctrl <- trainControl(method="cv", number=10,
                     selectionFunction="oneSE") #k-fold 교차검증을합니다

grid <- expand.grid(C=c(1,2,3,4,5)) #하이퍼파라미터 C를 지정해줍니다

m <- train(admit~., data=trainset, method="svmLinear",
           metric="Accuracy",
           trControl=ctrl,
           tunegrid=grid)


m    #check model details


#predict
pred <- predict(m, testset)

#Definition of label column vector & Labeldata
actual_type <- testset$admit
predict_type <- pred
positive_val <- "Y"
negative_val <- "N"

#1) Accuracy
tab <- table(Predicted=pred, Actual=testset$admit)
tab
sum(diag(tab)) / sum(tab)    #testset_Accuracy

#2) Kappa statistics
#install.packages("vcd")
library(vcd)
table( actual_type, predict_type)
Kappa( table( actual_type, predict_type)  ) 

###
#2. 서포트 벡터 머신일때 (비선형으로) 실험


###Caret Packages -- train() 
##0.1 Load data
binary <- read.csv("binary.csv")
binary$admit <- factor( binary$admit, labels=c("Y", "N"))
str(binary)
##0.2 Devide Db

set.seed(5311) #시드값변경
library(caret)
k <- createDataPartition(binary$admit, p=0.80, list=F)
trainset <- binary[k, ]
testset <- binary[-k, ]
nrow(trainset)    #321
nrow(testset)    #79


##3. svmPoly ---------------------------------------------
#install.packages("kernlab")
library(kernlab)
library(caret)
ctrl <- trainControl(method="cv", number=10,  # k-hold 교차검정하겠다.
                     selectionFunction="oneSE")

grid <- expand.grid(C=c(1,2,3,4,5))  # 하이퍼파라미터 C 를 지정 

m <- train(admit~., data=trainset, method="svmPoly",
           metric="Accuracy",
           trControl=ctrl,
           tunegrid=grid)             #비선형


m    #check model details


#predict
pred <- predict(m, testset)

#Definition of label column vector & Labeldata
actual_type <- testset$admit
predict_type <- pred
positive_val <- "Y"
negative_val <- "N"

#1) Accuracy
tab <- table(Predicted=pred, Actual=testset$admit)
tab
sum(diag(tab)) / sum(tab)    #testset_Accuracy  

#2) Kappa statistics
install.packages("vcd")
library(vcd)
table( actual_type, predict_type)
Kappa( table( actual_type, predict_type)  ) 

