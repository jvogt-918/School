# Load necessary library
library(ggplot2)
library(caret)
library(class)
library(e1071)

# Read the dataset (update file path if needed)
data <- read.csv(file.choose(), header = TRUE)

# Create the scatter plot
ggplot(data, aes(x = TCell_Count, y = Cortisol_Level, color = factor(Readmitted))) +
  geom_point(size = 3, alpha = 0.7) +  # Scatter plot points
  scale_color_manual(values = c("blue", "red"), labels = c("Not Readmitted", "Readmitted")) +
  labs(
    title = "Scatter Plot of TCell Count vs Cortisol Level",
    x = "TCell Count",
    y = "Cortisol Level",
    color = "Readmitted Status"
  ) +
  theme_minimal() +  # Clean theme
  theme(legend.position = "top")  # Move legend to top


##KNN

set.seed(3)
iterations = 10
numks = 20

masterAcc = matrix(nrow = iterations, ncol = numks)

for(j in 1:iterations)
{
  
  for(i in 1:numks)
  {
    CM = confusionMatrix(table(data[,3],knn.cv(data[,c(1,2)],data[,3],k = i)))
    masterAcc[j,i] = CM$overall[1]
    
  }
  
}

MeanAcc = colMeans(masterAcc)

plot(seq(1,numks,1),MeanAcc, type = "l")

which.max(MeanAcc)
max(MeanAcc)

#Final KNN Model k = 7
confusionMatrix(table(knn.cv(data[,c(1,2)],data[,3],k = 7),data[,3]))




#NB

model = naiveBayes(data[,c(1,2)],data$Readmitted)

CM = confusionMatrix(table(predict(model,data[,c(1,2)]),data$Readmitted), mode = "everything")
CM


