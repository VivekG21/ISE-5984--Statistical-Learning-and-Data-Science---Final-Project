
#Reading in libraries

library(data.table)
library(caret)
library(ggplot2)
library(ranger)
library(caTools)
library(dplyr) 
library(stringr)
library(corrplot)
library(Rtsne)
library(ROSE)
library(rpart)
library(Rborist)
library(xgboost)
library(neuralnet)
library(keras)
library(randomForest)
library(rpart.plot)
library(e1071)
library(smotefamily)
library(reshape2)
library(smotefamily)
library(mlbench)
library(tidyverse) 
library(reshape2) 
library(xgboost) 
library(Matrix) 
library(tictoc) 
library(PRROC)
library(smotefamily) 
library(gridExtra) 
library(gbm, quietly=TRUE)


memory.limit(size=56000)

# The data is read into R Studio and pre-processed for analysis
setwd("C:/ISE 5984")
data = read.csv("creditcard.csv")
head(data)
sum(is.na(data))
table(data$Class)
summary(data$Amount)
var(data$Amount)


str(data)





# Split raw data into training set and test set
set.seed(123)
split = sample.split(data$Class, SplitRatio = 0.70)
training_set = subset(data, split == TRUE)
test_set = subset(data, split == FALSE)


dim(training_set)
dim(test_set)
head(test_set)

table(training_set$Class)
table(test_set$Class)
# Apply ROSE sampling on training set to balance data set
set.seed(9560)
rose_train <- ROSE(Class ~ .,data  = training_set)$data 

sample <- sample_n(data, 100000)

# Apply Logistic Regression on Training Set 

glm.model <- glm(Class ~ ., data = training_set, family = "binomial")
glm.predict <- predict(glm.model,test_set, type = "response")
table(test_set$Class, glm.predict > 0.5)
summary(glm.model)
plot(glm.model)



# Remove the outliers from training set

outliers_remover <- function(a){
  df <- a
  aa<-c()
  count<-1
  for(i in 1:ncol(df)){
    if(is.numeric(df[,i])){
      Q3 <- quantile(df[,i], 0.75, na.rm = TRUE)
      Q1 <- quantile(df[,i], 0.25, na.rm = TRUE) 
      IQR <- Q3 - Q1  
      upper <- Q3 + 1.5 * IQR
      lower <- Q1 - 1.5 * IQR
      for(j in 1:nrow(df)){
        if(is.na(df[j,i]) == TRUE){
          next
        }
        else if(df[j,i] > upper | df[j,i] < lower){
          aa[count]<-j
          count<-count+1                  
        }
      }
    }
  }
  df<-df[-aa,]
}

rose_train <- outliers_remover(rose_train)
dim(rose_train)


decisionTree_model <- rpart(Class ~ . , data=training_set, method = 'class')
rpart.plot(decisionTree_model)
roseTree_model <- rpart(Class ~ . , rose_train, method = 'class')
rpart.plot(roseTree_model)
tree.predict <- predict(roseTree_model,test_set, type = "class")


# SVM Classification on Credit Card data training set
classifier = svm(formula = Class ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'radial')
classifier
summary(classifier)
y_pred = predict(classifier, newdata = test_set)
cm = table(test_set$Class, y_pred)
cm



# Constructing a Neural Network Model
library(neuralnet)
ANN_model =neuralnet (Class ~.,training_set,linear.output=FALSE)
plot(ANN_model)
predANN=compute(ANN_model,test_set)
resultANN=predANN$net.result
resultANN=ifelse(resultANN>0.5,1,0)


# Constructing a Gradient Boosting Model



model_boost <- gbm(Class ~ .
                   , distribution = "bernoulli"
                   , data = rbind(training_set, test_set)
                   , n.trees = 500
                   , interaction.depth = 3
                   , n.minobsinnode = 100
                   , shrinkage = 0.01
                   , bag.fraction = 0.5
                   , train.fraction = nrow(training_set) / (nrow(training_set) + nrow(test_set)))

model_gbm  
summary(model_gbm)

set.seed(42)

xgb_train <- sparse.model.matrix(Class ~ . -1, data = training_set)



set.seed(9560)
smote_train <- SMOTE(Class ~ .,data = trainingData)

gbm.iter = gbm.perf(model_gbm, method = "test")


plot(model_gbm)


# KNN Classification on Training Set Data


k <- 15

trControl <- trainControl(method  = "cv",
                          number  = 5)
set.seed(111)

s = function(seeds_list,k){
  seeds_list = lapply(seeds_list,"[",1:k)
  seeds_list[[length(seeds_list)+1]] = 999
  seeds_list
}

model <- train(Class ~.,data= training_set,method="knn",
                trControl = trainControl(method = 'LOOCV'),tuneGrid = expand.grid(k = 1:k))
model

# Plots of Credit Card Transaction Data
data$hour_of_day <- (data$Time/3600) %% 24 # convert to hours, then reduce mod 24

theme_set(theme_light()) # cleaner graphs

ggplot(data, aes(x = hour_of_day, fill = Class)) +
  geom_density(alpha = 0.7) + 
  scale_x_continuous(limits = c(0, 24), breaks = seq(0, 24, 1)) + 
  labs(title = "Hour of Day", 
       x = "Hour of Day", 
       y = "Density", 
       col = "Class") + 
  scale_fill_discrete(labels = c("Fraud", "Not Fraud"))

data$Time <- NULL

ggplot(data, aes(x = Amount, fill = Class)) +
  geom_density(alpha = 0.7) +
  scale_x_continuous(limits = c(0, 500), breaks = seq(0, 500, 50)) + 
  labs(title = "Transaction Amount", 
       x = "Amount", 
       y = "Density", 
       col = "Class") + 
  scale_fill_discrete(labels = c("Fraud", "Not Fraud"))
