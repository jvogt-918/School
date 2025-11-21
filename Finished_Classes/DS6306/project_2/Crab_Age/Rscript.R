#Crab Age
#Objectives
##A. Build Regression Model that minimizes the Mean Absolute Error on Competition set
##B. Analyze the data to uncover key factors that contribute to high-quality wine
#Deliverables
## RShiny - that is useful in highlighting  and exploring relationshipos between crab age and recorded features
## Rmarkdown that describes Analysis
## Predicted Qualites for the Test Set
## Git Repo with Everything
## Youtube (7 Minutes)

#Environment
library(ggplot2)
library(tidyverse)
library(dplyr)
library(class)
library(caret)
library(GGally)
library(corrplot)

#Cleaning Data
##Remove ID
crab_age$id <- NULL

#Removing the zeros for Height and Diameter
clean_crab_age <- crab_age %>%
  filter(Height != 0, Diameter != 0)


#Averages
ageavg <- crab_age %>%
  group_by(Age) %>%
  summarize(
    avg_sex = mean(Sex, na.rm = TRUE),
    avg_length = mean(Length, na.rm = TRUE),
    avg_diameter = mean(Diameter, na.rm = TRUE),
    avg_height = mean(Height, na.rm = TRUE),
    avg_weight = mean(Weight, na.rm = TRUE),
    avg_shweight = mean(`Shucked Weight`, na.rm = TRUE),
    avg_visweight = mean(`Viscera Weight`, na.rm = TRUE),
    avg_shlweight = mean(`Shell Weight`, na.rm = TRUE),
  )

view(ageavg)

crab_age %>%
  group_by(Sex) %>%
  summarize(
    avg_sex = mean(Age, na.rm = TRUE),
    avg_length = mean(Length, na.rm = TRUE),
    avg_diameter = mean(Diameter, na.rm = TRUE),
    avg_height = mean(Height, na.rm = TRUE),
    avg_weight = mean(Weight, na.rm = TRUE),
    avg_shweight = mean(`Shucked Weight`, na.rm = TRUE),
    avg_visweight = mean(`Viscera Weight`, na.rm = TRUE),
    avg_shlweight = mean(`Shell Weight`, na.rm = TRUE),
  )





#Look at Boxplots, maybe get rid of weight, high correlations
crab_age %>% 
  ggpairs()

crab_age %>% filter(crab_age$Weight)
  ggpairs()

#BoxPlots
crab_age %>%
  ggplot(aes(y = Sex, x = Length)) +
  geom_boxplot()

crab_age %>%
  ggplot(aes(y = Sex, x = Diameter)) +
  geom_boxplot()

crab_age %>%
  ggplot(aes(y = Sex, x = Height)) +
  geom_boxplot()

crab_age %>%
  ggplot(aes(y = Sex, x = Weight)) +
  geom_boxplot()

crab_age %>%
  ggplot(aes(y = Sex, x = `Shucked Weight`)) +
  geom_boxplot()

crab_age %>%
  ggplot(aes(y = Sex, x = `Viscera Weight` )) +
  geom_boxplot() 

crab_age %>%
  ggplot(aes(y = Sex, x= `Shell Weight`)) +
  geom_boxplot()

crab_age %>%
  ggplot(aes(y = Sex, x= Age)) +
  geom_boxplot()

#Violin Map
crab_age %>% 
  ggplot(aes(y = Age,x = Sex)) +
  geom_violin()

corrplot.mixed(crab_age)

#Linear Regression Model 
##Model with Whole Data Set. 
crab_age_model <- lm(Age ~ Length + Diameter + Height + Weight + Sex, data = crab_age)

predictions <- predict(crab_age_model, newdata = crab_age)

#MAE (Mean Average Error)
mean(abs(crab_age$Age - predictions))

#Creating Train set and a Test set and a loop to

iterations_crab = 500

masterMAE_crab = matrix(nrow = iterations_crab)

splitPerc_crab = .7 #Training / Test split Percentage

for(j in 1:iterations_crab)
{
  #Creating Sample at a 70% and %30 Split
  trainIndices_crab = sample(1:dim(crab_age)[1],round(splitPerc_crab * dim(crab_age)[1]))
  train_crab = crab_age[trainIndices_crab,]
  test_crab = crab_age[-trainIndices_crab,]
  model_crab = lm(Age ~ Length + Diameter + Height + `Shell Weight` + `Viscera Weight` + `Shucked Weight` + Sex, data = train_crab)
  #Mean Average Error
  predictions_crab = predict(model_crab, newdata = test_crab)
  masterMAE_crab[j] = mean(abs(test_crab$Age - predictions_crab))
}

MeanMAE_crab = colMeans(masterMAE_crab)
MeanMAE_crab



crab_age %>% 
  ggplot(aes(y = Age, x = `Shucked Weight`)) +
  geom_point(position = "jitter") +
  geom_smooth(type = "lm")
