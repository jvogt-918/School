#FLS_6
#titanic 
count(titanic)
str(titanic)

titanic_rmageNA <- titanic

#Removing NA from the datasets
titanic_rmageNA <- titanic_rmageNA[!is.na(titanic_rmageNA$Age),]
titanic_rmageNA$Cabin <- NULL  
titanic_rmageNA$Embarked <- NULL

#Creating a dataset of 600 and 291
titanic_600 <- sample(1:nrow(titanic_rmageNA), size = 600)
titanic_600 <- titanic_rmageNA[titanic_600, ]
titanic_600 

titanic_291 <- sample(1:nrow(titanic_rmageNA), size = 291)
titanic_291 <- titanic_rmageNA[titanic_291, ]
titanic_291

#Checking NA and Summaries of the data
#sum(is.na(titanic_291$Survived))
#sum(is.na(titanic_600$Survived))
#sum(is.na(titanic_rmageNA$Survived))
#str(titanic_600)
#sum(is.na(titanic_291))

#KNN and Confusion Matrix
classification <- knn(titanic_600[,c(3,6)],
    titanic_291[,c(3,6)],
    titanic_600$Survived,
    k = 3)

confusionMatrix(table(classification,titanic_291$Survived))

#Age and survival rate
 
titanic_age_class_survival <- titanic_rmageNA %>% select(Survived ,Pclass, Age)
tsac <- titanic_age_class_survival %>%
  group_by(Age,Pclass) %>%
  summarise(mean(Survived))

tsac %>% filter(Age == 30)

#Male and Female Models
tm <- titanic_rmageNA %>%
  filter(Sex == "male")
tf <- titanic_rmageNA %>%
  filter(Sex == "female")

#knn for male and female
knn(titanic_rmageNA[,c(3,6)],
    tm[,c(3,6)],
    titanic_rmageNA$Survived,
    k = 3)
knn(titanic_rmageNA[,c(3,6)],
    tf[,c(3,6)],
    titanic_rmageNA$Survived,
    k = 3)

#Confusion Matrix for both
cm <- knn(titanic_rmageNA[,c(3,6)],
    tm[,c(3,6)],
    titanic_rmageNA$Survived,
    k = 3)
cf <- knn(titanic_rmageNA[,c(3,6)],
    tf[,c(3,6)],
    titanic_rmageNA$Survived,
    k = 3)
confusionMatrix(table(cm,tm$Survived))
confusionMatrix(table(cf,tf$Survived))

###### AI Perplexity ###############

#Import and Partitioning the data 
# Load the dataset
titanic <- read.csv("https://raw.githubusercontent.com/BivinSadler/MSDS_6306_Doing-Data-Science/Master/Unit%206/titanic_train.csv")

# Set a seed for reproducibility
set.seed(123)

# Create a vector of random indices
train_indices <- sample(1:nrow(titanic), 600)

# Create the training and testing sets
train <- titanic[train_indices, ]
test <- titanic[-train_indices, ]

# Verify the sizes
nrow(train)
nrow(test)

##KNN Classification
# Load the class package for KNN
library(class)

# Handle missing Age values (impute with the median)
median_age <- median(train$Age, na.rm = TRUE)
train$Age[is.na(train$Age)] <- median_age
test$Age[is.na(test$Age)] <- median_age

# Prepare training and testing data for KNN
train_data <- data.frame(train$Age, train$Pclass)
test_data <- data.frame(test$Age, test$Pclass)
train_labels <- factor(train$Survived)

# Run KNN (k = 3. You can tune this)
knn_predictions <- knn(train = train_data, test = test_data, cl = train_labels, k = 3)

# Display the predictions
print(knn_predictions)

#Confusion Matrix and Statistics
# Create the confusion matrix
confusion_matrix <- table(Actual = test$Survived, Predicted = knn_predictions)
print(confusion_matrix)

# Calculate statistics
accuracy <- sum(diag(confusion_matrix)) / nrow(test)
misclassification_rate <- 1 - accuracy
sensitivity <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ]) # TP / (TP + FN)
specificity <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ]) # TN / (TN + FP)

# Print the statistics
print(paste("Accuracy:", accuracy))
print(paste("Misclassification Rate:", misclassification_rate))
print(paste("Sensitivity:", sensitivity))
print(paste("Specificity:", specificity))

#Survival Probability Prediction
# Function to predict survival probability for a given age and Pclass
predict_survival_probability <- function(age, pclass, train_data, train_labels, k = 3) {
  new_data <- data.frame(Age = age, Pclass = pclass)
  prediction <- knn(train = train_data, test = new_data, cl = train_labels, k = k, prob = TRUE)
  probabilities <- attributes(prediction)$prob
  if (prediction[1] == 1) {
    return(probabilities[1]) # Probability of survival
  } else {
    return(1 - probabilities[1]) # Probability of survival
  }
}

# Use the same training data prepared before
train_data <- data.frame(train$Age, train$Pclass)
train_labels <- factor(train$Survived)

# Example usage:
age <- 30
for (pclass in 1:3) {
  survival_probability <- predict_survival_probability(age, pclass, train_data, train_labels)
  print(paste("Age:", age, ", Pclass:", pclass, ", Survival Probability:", survival_probability))
}

#Seperate Models for mail and female set
# Split the data into male and female sets
train_male <- train[train$Sex == "male", ]
train_female <- train[train$Sex == "female", ]
test_male <- test[test$Sex == "male", ]
test_female <- test[test$Sex == "female", ]

# Function to train KNN and evaluate performance
train_and_evaluate_knn <- function(train_data, train_labels, test_data, test_actual) {
  # Train KNN model
  knn_predictions <- knn(train = train_data, test = test_data, cl = train_labels, k = 3)
  
  # Create confusion matrix
  confusion_matrix <- table(Actual = test_actual, Predicted = knn_predictions)
  
  # Calculate statistics
  accuracy <- sum(diag(confusion_matrix)) / nrow(test_data)
  misclassification_rate <- 1 - accuracy
  sensitivity <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  specificity <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])
  
  return(list(
    confusion_matrix = confusion_matrix,
    accuracy = accuracy,
    misclassification_rate = misclassification_rate,
    sensitivity = sensitivity,
    specificity = specificity
  ))
}

# Prepare data for male model
train_data_male <- data.frame(train_male$Age, train_male$Pclass)
train_labels_male <- factor(train_male$Survived)
test_data_male <- data.frame(test_male$Age, test_male$Pclass)
test_actual_male <- test_male$Survived

# Train and evaluate male model
male_results <- train_and_evaluate_knn(train_data_male, train_labels_male, test_data_male, test_actual_male)

# Prepare data for female model
train_data_female <- data.frame(train_female$Age, train_female$Pclass)
train_labels_female <- factor(train_female$Survived)
test_data_female <- data.frame(test_female$Age, test_female$Pclass)
test_actual_female <- test_female$Survived

# Train and evaluate female model
female_results <- train_and_evaluate_knn(train_data_female, train_labels_female, test_data_female, test_actual_female)

# Print the results
print("Male Model Results:")
print(male_results)
print("Female Model Results:")
print(female_results)

#Multinomial Cross Validation

iterations = 500
numks = 90
splitPerc = .95

masterAcc = matrix(nrow = iterations, ncol = numks)

for(j in 1:iterations)
{
  trainIndices = sample(1:dim(iris)[1],round(splitPerc * dim(iris)[1]))
  train = iris[trainIndices,]
  test = iris[-trainIndices,]
  for(i in 1:numks)
  {
    classifications = knn(train[,c(1,2)],test[,c(1,2)],train$Species, prob = TRUE, k = i)
    table(classifications,test$Species)
    CM = confusionMatrix(table(classifications,test$Species))
    masterAcc[j,i] = CM$overall[1]
  }
  
}

MeanAcc = colMeans(masterAcc)

plot(seq(1,numks,1),MeanAcc, type = "l", main = "Mean Accuracy of Each K Value",
     xlab = "K Value", ylab = "Accuracy") 

which.max(MeanAcc)
max(MeanAcc)



