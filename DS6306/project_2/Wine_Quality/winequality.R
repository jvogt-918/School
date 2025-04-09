#Wine Quality
#Objectives
##A. Build Model that minimizes the Mean Absolute Error on test set
##B. Analyze the data to uncover key factors that contribute to high-quality wine
#Deliverables
## RShiny - Provides two way plots that shows quality versus other features
## Rmarkdown that describes Analysis
## Predicted Qualites for the Test Set
## Git Repo with Everything
## Youtube

#Environment
##Packages
library(ggplot2)
library(tidyverse)
library(dplyr)
library(class)
library(caret)

##Factorclass##Factoring Quality
Wine_Train$quality <- as.factor(Wine_Train$quality)
levels(Wine_Train$quality)

#Excel Notes
## Low Volatile Acidity seems to have some correlation
##largest Correlations seem to be Volatile Acidity, Alcohol, and density

#Analysis

#ScatterPlots quality vs. errthang
##Didn't show much, probably skip the facet wrap until better values are id'd
ggplot(Wine_Train, aes(x = alcohol, y = pH)) +
  geom_point() +
  facet_wrap(~quality)

##Shows a slight trend upwards, really just helps identify a sweet spot
ggplot(Wine_Train, aes(x = quality, y = alcohol)) +
  geom_point(position = "jitter")

##Shows not much, really that most wines are between 3 - 3.5
ggplot(Wine_Train, aes(x = quality, y = pH)) +
  geom_point(position = "jitter")

#Shows some sweetspots
ggplot(Wine_Train, aes(x = quality, y = `residual sugar`)) +
  geom_point(position = "jitter")

#Nothing of note
ggplot(Wine_Train, aes(x = quality, y = `fixed acidity`)) +
  geom_point(position = "jitter")

#Nahthing
ggplot(Wine_Train, aes(x = quality, y = sulphates)) +
  geom_point(position = "jitter")

ggplot(Wine_Train, aes(x = quality, y = `alcohol`, color = `density`)) +
  geom_point(position = "jitter")

##### Checking averages ################

#Total in each Quality
Wine_Train %>%
  group_by(quality) %>%
  summarize(count = n())

#Averages for each Category
Wine_Train %>%
  group_by(quality) %>%
  summarize(
    avg_facid = mean(`fixed acidity`, na.rm = TRUE),
    avg_volacid = mean(`volatile acidity`, na.rm = TRUE),
    avg_citacid = mean(`citric acid`, na.rm = TRUE),
    avg_resugar = mean(`residual sugar`, na.rm = TRUE),
    avg_chloride = mean(chlorides, na.rm = TRUE),
    avg_fsd = mean(`free sulfur dioxide`, na.rm = TRUE),
    avg_tsd = mean(`total sulfur dioxide`, na.rm = TRUE),
    avg_density = mean(density, na.rm = TRUE),
    avg_pH = mean(pH, na.rm = TRUE),
    avg_sulphates = mean(sulphates, na.rm = TRUE),
    avg_alcohol = mean(alcohol, na.rm = TRUE)
  )

#Median for each Category
Wine_Train %>%
  group_by(quality) %>%
  summarize(
    avg_facid = median(`fixed acidity`, na.rm = TRUE),
    avg_volacid = median(`volatile acidity`, na.rm = TRUE),
    avg_citacid = median(`citric acid`, na.rm = TRUE),
    avg_resugar = median(`residual sugar`, na.rm = TRUE),
    avg_chloride = median(chlorides, na.rm = TRUE),
    avg_fsd = median(`free sulfur dioxide`, na.rm = TRUE),
    avg_tsd = median(`total sulfur dioxide`, na.rm = TRUE),
    avg_density = median(density, na.rm = TRUE),
    avg_pH = median(pH, na.rm = TRUE),
    avg_sulphates = median(sulphates, na.rm = TRUE),
    avg_alcohol = median(alcohol, na.rm = TRUE)
  )

#Histograms
ggplot(Wine_Train, aes(x = `volatile acidity`))+
  geom_histogram()

ggplot(Wine_Train, aes(x = density))+
  geom_histogram()

ggplot(Wine_Train, aes(x = alcohol))+
  geom_histogram()

ggplot(Wine_Train, aes(x = quality))+
  geom_histogram(stat = "count")




#KNN (NEED TO REEDIT)
#setting parameters of the Model
iterations_knn = 500
numks = 90
splitPerc_knn = .7

#Creating buckets to collect every iteration's Acc, Sen, Spec
masterAcc_knn = matrix(nrow = iterations_knn, ncol = numks)

#A loop to hypertune the K - Value
for(j in 1:iterations_knn)
{
  trainIndices_knn = sample(1:dim(Wine_Train)[1],round(splitPerc_knn * dim(Wine_Train)[1]))
  train_knn = Wine_Train[trainIndices_knn,]
  test_knn = Wine_Train[-trainIndices_knn,]
  for(i in 1:numks)
  {
    classifications_knn = knn(train_knn[,c("volatile acidity", "density", "alcohol")], test_knn[,c("volatile acidity", "density", "alcohol")], train_knn$quality, prob = TRUE, k = i)
    table(classifications_knn,test_knn$quality)
    CM_knn = confusionMatrix(table(classifications_knn,test_knn$quality))
    masterAcc_knn[j,i] = CM_knn$overall[1]
  }
}

#Collect Means of ALL Iterations
MeanAcc_knn = colMeans(masterAcc_knn)

#Plotting each Iterations Acc, Spec, and Sen Value to see which K Value is best to use
plot(seq(1,numks,1),MeanAcc_knn, type = "l", main = "Mean Accuracy of Each K Value",
     xlab = "K Value", ylab = "Accuracy") 
CM_knn$byClass



