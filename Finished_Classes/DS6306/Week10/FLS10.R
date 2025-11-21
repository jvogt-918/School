##FLS Unit 10 
library(tidyverse)
library(reshape2)

view(cars)        

#Plotting MPG and Weight
cars %>%
  ggplot(aes(x = Weight, y = MPG)) +
  geom_point() +
  geom_smooth(method = "lm")


#Fitting Model 
fit = lm(MPG~Weight, data = cars)

summary(fit)

#Confidence Interval
confint(fit)

#Intercept and Weight
fit$coefficients[1]
fit$coefficients[2]

tstat = -0.0076613/0.0002577 #beta_0_hat / SE(beta_0_hat)
pvalue = (pt(tstat,7)) * 2 # Mult by 2 since 2 sided test
tstat
pvalue

#Question 2
cars %>% 
  ggplot(aes(x = Weight, y = MPG)) + 
  geom_point() + 
  ggtitle("cars: mpg v. weight") + 
  geom_smooth(method = "lm") + 
  xlim(0,6000)

# degree 1 model
fit = lm(MPG~Weight, data = cars)
summary(fit)

#degree 2 model
cars %>% ggplot(aes(x = Weight, y = MPG)) + geom_point()
cars_deg2 = cars %>% mutate(Weight2 = Weight^2)
fit_deg2 = lm(MPG~Weight+Weight2, cars_deg2)
summary(fit_deg2)
confint(fit_deg2)

preds = predict(fit_deg2)
cars %>% 
  ggplot(aes(x = Weight, y = MPG)) +
  geom_point() +
  geom_line(data = cars, aes( x = Weight, y = preds, col = "red"))

twok <- data.frame(Weight = 2000)
twok = twok %>% mutate(Weight2 = Weight^2)
predict(fit_deg2, newdata = twok)

#Cross Validation
Train0bs = sample(seq(1,dim(cars)[1]),round(.75*dim(cars)[1]),replace = FALSE)
carsTrain = cars[Train0bs,]
carsTrain
carsTest = cars[-Train0bs,]
carsTest
Model_fit = lm(MPG ~ Weight, data = carsTrain)
summary(Model_fit)
Model_Preds = predict(Model_fit, newdata = carsTest)
as.data.frame(Model_Preds)


#CV for deg 2
Train0bs_deg2 = sample(seq(1,dim(cars_deg2)[1]),round(.75*dim(cars_deg2)[1]),replace = FALSE)
carsTrain_deg2 = cars_deg2[Train0bs_deg2,]
carsTrain_deg2
carsTest_deg2 = cars_deg2[-Train0bs_deg2,]
carsTest_deg2
Model_fit_deg2 = lm(MPG ~ Weight, data = carsTrain_deg2)
summary(Model_fit_deg2)
Model_Preds_deg2 = predict(Model_fit_deg2, newdata = carsTest_deg2)
as.data.frame(Model_Preds_deg2)

MSPE = data.frame(Observed = carsTest$MPG, Predicted = Model_Preds)
MSPE$Resisdual = MSPE$Observed - MSPE$Predicted
MSPE$SquaredResidual = MSPE$Resisdual^2
MSPE
mean(MSPE$SquaredResidual)

MSPE_deg2 = data.frame(Observed = carsTest_deg2$MPG, Predicted = Model_Preds_deg2)
MSPE_deg2$Resisdual = MSPE_deg2$Observed - MSPE_deg2$Predicted
MSPE_deg2$SquaredResidual = MSPE_deg2$Resisdual^2
MSPE_deg2
mean(MSPE_deg2$SquaredResidual)


#question 3
#Predicting and replacing NA HP Values
fit_hp = lm(Horsepower~MPG, data = cars)
hp_pred <- data.frame(MPG = c(34.5, 23))
predict(fit_hp, newdata = hp_pred)
cars$Horsepower[351] <- 62
cars$Horsepower[371] <- 106

summary(fit_q3)
confint(fit_q3)

fit_q3 = lm(MPG~Horsepower, data = cars)
preds_q3 = predict(fit_q3)
mean

preds_q3 = predict(fit_q3_deg2)
cars %>% 
  ggplot(aes(x = Horsepower, y = MPG)) +
  geom_point() +
  geom_line(data = cars, aes( x = Horsepower, y = preds_q3, col = "red"))

q3_hp <- data.frame(Horsepower = 250, Horsepower2 = 250^2
                    )
predict(fit_q3, newdata = q3_hp)

cars_q3_deg2 = cars %>% mutate(Horsepower2 = Horsepower^2)
fit_q3_deg2 = lm(MPG~Horsepower + Horsepower2, cars_q3_deg2)
summary(fit_q3_deg2)
confint(fit_q3_deg2)

predict(fit_q3_deg2, newdata = q3_hp)
