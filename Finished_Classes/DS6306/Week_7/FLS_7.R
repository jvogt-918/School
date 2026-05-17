#FLS7
library(dplyr)
library(tm) #text mining library provides the stopwords() function
library(tidyr)
library(plyr)
library(jsonlite)
library(dplyr)
library(tidyverse)
library(e1071)
library(caret)

na_age <- titanic_train %>% filter(is.na(Age))
summary(na_age$Pclass)

titanic = titanic_train
#replacing na with 28 (median)
titanic$Age[is.na(titanic$Age)] <- 28

#Making survived and pclass into a categorical value
titanic$Survived <- factor(titanic$Survived)
titanic$Pclass <- factor(titanic$Pclass)

#creating 30 year old passenger
p1_30yo <- data.frame(Age = 30, Pclass = factor(1))
p2_30yo <- data.frame(Age = 30, Pclass = factor(2))
p3_30yo <- data.frame(Age = 30, Pclass = factor(3))

#NaiveBayes
model = naiveBayes(Survived~ Age + Pclass,data = titanic)

#predicting each pasenger
prediction_p1 <- predict(model, p1_30yo, type = "raw")
prediction_p2 <- predict(model, p2_30yo, type = "raw")
prediction_p3 <- predict(model, p3_30yo, type = "raw")
print(prediction_p3)
print(prediction_p2)
print(prediction_p1)

#Splitting 70% - 30%
titanicClean = titanic %>% filter(!is.na(Age) & !is.na(Pclass))
set.seed(4)
trainIndices = sample(seq(1:length(titanicClean$Age)),round(.7*length(titanicClean$Age)))
trainTitanic = titanicClean[trainIndices,]
testTitanic = titanicClean[-trainIndices,]

model <- naiveBayes(Survived~ Age + Pclass, data = trainTitanic)
predict(model, testTitanic, type = "raw")

confusionMatrix(table(predict(model,testTitanic),testTitanic$Survived))

#Same but with new seed
titanicClean = titanic %>% filter(!is.na(Age) & !is.na(Pclass))
set.seed(67)
trainIndices = sample(seq(1:length(titanicClean$Age)),round(.7*length(titanicClean$Age)))
trainTitanic = titanicClean[trainIndices,]
testTitanic = titanicClean[-trainIndices,]

model <- naiveBayes(Survived~ Age + Pclass, data = trainTitanic)
predict(model, testTitanic, type = "raw")

confusionMatrix(table(predict(model,testTitanic),testTitanic$Survived))

#loop it to repeat with 100 different seeds
#creating empty sets to summarize latter
hundo = 100
accuracies <- numeric(hundo)
sensitivities <- numeric(hundo)
specificities <- numeric(hundo)

for (i in 1:hundo) {
  set.seed(i)
  trainIndices = sample(seq(1:length(titanicClean$Age)),round(.7*length(titanicClean$Age)))
  trainTitanic = titanicClean[trainIndices,]
  testTitanic = titanicClean[-trainIndices,]
  model <- naiveBayes(Survived~ Age + Pclass, data = trainTitanic)
  comatrix <- confusionMatrix(table(predict(model,testTitanic),testTitanic$Survived))
  accuracies[i] <- comatrix$overall['Accuracy']
  sensitivities[i] <- comatrix$byClass['Sensitivity']
  specificities[i] <- comatrix$byClass['Specificity']
}

mean_accucary = mean(accuracies)
mean_sensitivies = mean(sensitivities)
mean_specificites= mean(specificities)

#adding sex to the train set
titanicClean = titanic %>% filter(!is.na(Age) & !is.na(Pclass) & !is.na(Sex))
set.seed(4)
trainIndices = sample(seq(1:length(titanicClean$Age)),round(.7*length(titanicClean$Age)))
trainTitanic = titanicClean[trainIndices,]
testTitanic = titanicClean[-trainIndices,]

model <- naiveBayes(Survived~ Age + Pclass + Sex, data = trainTitanic)

confusionMatrix(table(predict(model,testTitanic),testTitanic$Survived))

#looping it 100 times to see if sex changes tthings up
#loop it to repeat with 100 different seeds
#creating empty sets to summarize latter
hundo = 100
accuracies_2 <- numeric(hundo)
sensitivities_2 <- numeric(hundo)
specificities_2 <- numeric(hundo)

for (i in 1:hundo) {
  set.seed(i)
  trainIndices = sample(seq(1:length(titanicClean$Age)),round(.7*length(titanicClean$Age)))
  trainTitanic = titanicClean[trainIndices,]
  testTitanic = titanicClean[-trainIndices,]
  model <- naiveBayes(Survived~ Age + Pclass + Sex + Fare, data = trainTitanic)
  comatrix <- confusionMatrix(table(predict(model,testTitanic),testTitanic$Survived))
  accuracies_2[i] <- comatrix$overall['Accuracy']
  sensitivities_2[i] <- comatrix$byClass['Sensitivity']
  specificities_2[i] <- comatrix$byClass['Specificity']
}

mean_accucary_2 = mean(accuracies_2)
mean_sensitivies_2 = mean(sensitivities_2)
mean_specificites_2 = mean(specificities_2)
mean_accucary_2
mean_sensitivies_2
mean_specificites_2

#Part 2

# NB Loop for average of many training / test partition

iterations = 500

masterAcc = matrix(nrow = iterations)
setSen = matrix(nrow = iterations)
setSpec = matrix(nrow = iterations)
virSen = matrix(nrow = iterations)
virSpec = matrix(nrow = iterations)
verSen = matrix(nrow = iterations)
verSpec = matrix(nrow = iterations)


splitPerc = .7 #Training / Test split Percentage

for(j in 1:iterations)
{
  
  trainIndices = sample(1:dim(iris)[1],round(splitPerc * dim(iris)[1]))
  train = iris[trainIndices,]
  test = iris[-trainIndices,]
  
  model = naiveBayes(train[,c(1,2)],as.factor(train$Species),laplace = 1)
  table(predict(model,test[,c(1,2)]),as.factor(test$Species))
  CM = confusionMatrix(table(predict(model,test[,c(1,2)]),as.factor(test$Species)))
  masterAcc[j] = CM$overall[1]
  setSen[j] = CM$byClass[1,1]
  setSpec[j] = CM$byClass[1,2]
  verSen[j] = CM$byClass[2,1]
  verSpec[j] = CM$byClass[2,2]
  virSen[j] = CM$byClass[3,1]
  virSpec[j] = CM$byClass[3,2]
}

MeanAcc = colMeans(masterAcc)
MeansetSen = colMeans(setSen)
MeansetSpec = colMeans(setSpec)
MeanverSen = colMeans(verSen)
MeanverSpec = colMeans(verSpec)
MeanvirSen = colMeans(virSen)
MeanvirSpec = colMeans(virSpec)
0
MeanSpec <- NULL



#FLS LIVE Break out
#100 Patient, TCell, Cortisol
#NB
Readmit
model = naiveBayes(Readmitted~ TCell_Count + Cortisol_Level,data = Readmit)

#predicting each patient
p1_30yo <- data.frame(TCell_Count = 100, Cortisol_Level = 10)
prediction_p1 <- predict(model, p1_30yo, type = "raw")
prediction_p1

confusionMatrix(table(predict(model,testTitanic),testTitanic$Survived))
confusionMatrix(table(predict(model, Readmit),Readmit$Readmitted), mode = "everything")

