#Logistic starts here
getwd()

framdata<-read.csv("FraminghamClean (1).csv")
head(framdata)

set.seed(2345)
str(framdata)


train_fram <- sample.int(n = nrow(framdata), size = floor(.80*nrow(framdata)), replace = FALSE)
framtrain <- framdata[train_fram, ]
framtest  <- framdata[-train_fram, ]

framtrain
cor(framtrain)

# LRfit1<-glm(formula=TenYearCHD ~ male + age	+ education +	currentSmoker +	cigsPerDay +	BPMeds	+ prevalentStroke	+ prevalentHyp +	diabetes	+ totChol	+ sysBP	+ diaBP	+ BMI	+ heartRate	+ glucose,data=framtrain, family=binomial())
# summary(LRfit1)

# LRfit2<-glm(formula=TenYearCHD ~ male + age	+ education +	cigsPerDay +	BPMeds	+ prevalentStroke	+ prevalentHyp + diabetes + totChol	+ sysBP	+ diaBP	+ BMI	+ heartRate + glucose,data=framtrain, family=binomial())
# summary(LRfit2)

# LRfit3<-glm(formula=TenYearCHD ~ male + age	+ education +	cigsPerDay +	BPMeds	+ prevalentStroke	+ prevalentHyp + totChol	+ sysBP	+ diaBP	+ BMI	+ heartRate + glucose,data=framtrain, family=binomial())
# summary(LRfit3)

# LRfit4<-glm(formula=TenYearCHD ~ male + age	+ education +	cigsPerDay +	BPMeds	+ prevalentStroke	+ prevalentHyp + totChol	+ sysBP	+ diaBP	+ heartRate + glucose,data=framtrain, family=binomial())
# summary(LRfit4) 
# 
# LRfit5<-glm(formula=TenYearCHD ~ male + age	+ education +	cigsPerDay +	BPMeds	+ prevalentStroke	+ prevalentHyp + totChol	+ sysBP	+ diaBP	+ glucose,data=framtrain, family=binomial())
# summary(LRfit5) 
# 
# LRfit6<-glm(formula=TenYearCHD ~ male + age	+ education +	cigsPerDay + prevalentStroke	+ prevalentHyp + totChol	+ sysBP	+ diaBP	+ glucose,data=framtrain, family=binomial())
# summary(LRfit6) 

# LRfit7<-glm(formula=TenYearCHD ~ male + age	+	cigsPerDay + prevalentStroke	+ prevalentHyp + totChol	+ sysBP	+ diaBP	+ glucose,data=framtrain, family=binomial())
# summary(LRfit7) 

# LRfit8<-glm(formula=TenYearCHD ~ male + age	+	cigsPerDay + prevalentStroke	+ prevalentHyp + totChol	+ sysBP	+ glucose,data=framtrain, family=binomial())
# summary(LRfit8) 

#LRfit9<-glm(formula=TenYearCHD ~ male + age	+	cigsPerDay + prevalentStroke + totChol	+ sysBP	+ glucose,data=framtrain, family=binomial())
#summary(LRfit9) 

LRfit10<-glm(formula=TenYearCHD ~ male + age	+	cigsPerDay + prevalentStroke 	+ sysBP	+ glucose,data=framtrain, family=binomial())
summary(LRfit10)


library(ROSE)
framtest$predict_final<-ifelse(predict(LRfit10,framtest, type="response")>0.4, 1, 0)

confusion_matrix <- table(framtest$predict_final,framtest$TenYearCHD, dnn=c("Predicted","Actual"))
confusion_matrix
overall_accuracy <- (confusion_matrix[1,1]+confusion_matrix[2,2])/sum(confusion_matrix)
overall_accuracy
sensitivity_final <- confusion_matrix[2,2]/sum(confusion_matrix[,2])
specificity_final <- confusion_matrix[1,1]/sum(confusion_matrix[,1])
sensitivity_final
specificity_final

framtest$predict_final1<-ifelse(predict(LRfit10,framtest, type="response")>0.15, 1, 0)
confusion_matrix1 <- table(framtest$predict_final1,framtest$TenYearCHD, dnn=c("Predicted","Actual"))
confusion_matrix1
overall_accuracy1 <- (confusion_matrix1[1,1]+confusion_matrix1[2,2])/sum(confusion_matrix1)
overall_accuracy1
sensitivity_final1 <- confusion_matrix1[2,2]/sum(confusion_matrix1[,2])
specificity_final1 <- confusion_matrix1[1,1]/sum(confusion_matrix1[,1])
sensitivity_final1
specificity_final1

#Logistic ends here

#Random forest starts here

rm(list=ls())

heart<-read.csv("FraminghamClean.csv")

head(heart)

#Categorical variables as factor

heart$male<-as.factor(heart$male)
heart$age<-as.factor(heart$age)
heart$education<-as.factor(heart$education)
heart$currentSmoker<-as.factor(heart$currentSmoker)
heart$cigsPerDay<-as.factor(heart$cigsPerDay)
heart$BPMeds<-as.factor(heart$BPMeds)
heart$prevalentStroke<-as.factor(heart$prevalentStroke)
heart$prevalentHyp<-as.factor(heart$prevalentHyp)
heart$diabetes<-as.factor(heart$diabetes)
heart$TenYearCHD<-as.factor(heart$TenYearCHD)

# Splitting into training and test data


#Model 1
set.seed(2345)

train_ind <- sample.int(n = nrow(heart), size = floor(.8*nrow(heart)), replace = FALSE)

#train and test
heart_train <- heart[train_ind, ]
heart_test  <- heart[-train_ind, ]

#Random Forest
install.packages("randomForest")    
library(randomForest)

rf <- randomForest(TenYearCHD ~ male+age+education+currentSmoker+cigsPerDay+BPMeds+prevalentStroke+prevalentHyp+diabetes+totChol+sysBP+diaBP+BMI+heartRate+glucose, data=heart_train)

rf_predict <- predict(rf,heart_test,type="response")     
confusion_matrix <- table(rf_predict,heart_test$TenYearCHD, dnn=c("Predicted","Actual"))
confusion_matrix



RF_accuracy <- (confusion_matrix[1,1]+confusion_matrix[2,2])/sum(confusion_matrix)
RF_accuracy

sensitivity <- confusion_matrix[2,2]/sum(confusion_matrix[ ,2])
specificity <- confusion_matrix[1,1]/sum(confusion_matrix[ ,1])
sensitivity
specificity

#Model 1 results - Accuracy: 83.88%; Sensitivity: 9.68%; Specificity: 97.49%
# We can check which variables are most important in driving the predictions using the importance function
importance(rf, type=2)

# MeanDecreaseGini
# male                   14.147944
# age                   162.760112
# education              28.118579
# currentSmoker           7.149898
# cigsPerDay             59.151379
# BPMeds                  4.297322
# prevalentStroke         2.016643
# prevalentHyp           12.447085
# diabetes                4.824826
# totChol                75.218154
# sysBP                  88.713660
# diaBP                  76.274267
# BMI                    77.147422
# heartRate              60.812657
# glucose                75.746562

#Model2 based on the importance
set.seed(2345)

train_ind <- sample.int(n = nrow(heart), size = floor(.8*nrow(heart)), replace = FALSE)

#train and test
heart_train <- heart[train_ind, ]
heart_test  <- heart[-train_ind, ]

#Random Forest
install.packages("randomForest")    
library(randomForest)

rf <- randomForest(TenYearCHD ~ age+sysBP+BMI+totChol+glucose+diaBP+heartRate+cigsPerDay, data=heart_train)

rf_predict <- predict(rf,heart_test,type="response")     
confusion_matrix <- table(rf_predict,heart_test$TenYearCHD, dnn=c("Predicted","Actual"))
confusion_matrix



RF_accuracy <- (confusion_matrix[1,1]+confusion_matrix[2,2])/sum(confusion_matrix)
RF_accuracy

sensitivity <- confusion_matrix[2,2]/sum(confusion_matrix[ ,2])
specificity <- confusion_matrix[1,1]/sum(confusion_matrix[ ,1])
sensitivity
specificity

#Model3 removing correlated features
rf <- randomForest(TenYearCHD ~ age+sysBP+BMI+totChol+glucose+heartRate+cigsPerDay, data=heart_train)

rf_predict <- predict(rf,heart_test,type="response")     
confusion_matrix <- table(rf_predict,heart_test$TenYearCHD, dnn=c("Predicted","Actual"))
confusion_matrix



RF_accuracy <- (confusion_matrix[1,1]+confusion_matrix[2,2])/sum(confusion_matrix)
RF_accuracy

sensitivity <- confusion_matrix[2,2]/sum(confusion_matrix[ ,2])
specificity <- confusion_matrix[1,1]/sum(confusion_matrix[ ,1])
sensitivity
specificity



#Confusion matrix for best model
TClass <- factor(c(0, 0, 1, 1))
PClass <- factor(c(0, 1, 0, 1))
Y      <- c(619, 22, 73, 18)
df <- data.frame(TClass, PClass, Y)

library(ggplot2)
ggplot(data =  df, mapping = aes(x = PClass, y = TClass)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_bw() + theme(legend.position = "none")
#Random forest ends here


#KNN starts here
install.packages("tidyverse")
library(tidyverse)
library(fastDummies)
library(class)


setwd("D:/1-UWM/1-Class/OTM 714-Supply Chain Analytics/Assignment/05112022-Final Project")
knn <- read.csv("FraminghamClean.csv")
str(knn)



# Separate the "X" variables from the class variable
knn_x <- knn[, 1:15]
knn_x <- knn[, c(3,5,7)]
colnames(knn_x)
knn_outcome <- knn[, 16]

# Scale the X variables for distance calculations
scaled_knn_x <- scale(knn_x)



# Split into 75% training and 25% testing
train75 <- sample.int(n = nrow(scaled_knn_x),size = floor(.75 * nrow(scaled_knn_x)),replace = FALSE)



# Selects all the rows that correspond with the indices select.
knn_train <- scaled_knn_x[train75, ]
knn_test  <- scaled_knn_x[-train75, ]
knn_train_outcome <- knn_outcome[train75]
knn_test_outcome <- knn_outcome[-train75]



# Run the KNN algorithm.
knn_predict <- knn(knn_train, knn_test, knn_train_outcome, k=7)
knn_confusion <- table(knn_predict, knn_test_outcome)
knn_confusion


# Calculate overall accuracy
knn_accuracy <- (knn_confusion[1, 1] + knn_confusion[2, 2]) / sum(knn_confusion)
knn_accuracy


# Sensitivity
knn_sensitivity <- knn_confusion[2, 2] / sum(knn_confusion[, 2])
knn_sensitivity



# Specificity
knn_specificity <- knn_confusion[1, 1] / sum(knn_confusion[, 1])
knn_specificity



# Balanced Accuracy
knn_balanced_accuracy <- (knn_sensitivity + knn_specificity) / 2
knn_balanced_accuracy
#K-Nearest Neighbors End


