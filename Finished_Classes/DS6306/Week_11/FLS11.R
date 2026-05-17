##FLS 11 
##Activity 1

install.packages("fpp")
library(fpp)

# 1. SES MODEL FOR AUS AIR 
data(ausair)

#returns a ts object
air = window(ausair, start = 1990, end = 2004)

# Always plot the data first! 
plot(air,ylab = "Airline Passegners", xlab = "Year", main = "Airline Passengers")

#fit 3 different simple exponential smoothing models ... how are they different?
# what does the h paramter do? 
fit1 = ses(air, initial = "simple",alpha = .2,h = 3)
fit2 = ses(air,initial = "simple",alpha = .6, h = 3)
fit3 = ses(air, h = 3) #defaults

# the forecast package has a nice accuracy funciton with various metrics just pass it the 
# the model and the data!  (This is the "training" data)
accuracy(fit1, ausair)
accuracy(fit2, ausair)
accuracy(fit3, ausair)

#Reset the plot
plot(air,ylab = "Airline Passegners", xlab = "Year", type = "o", xlim = c(1990, 2008),ylim = c(15,50), main = "Airline Passengers")

#Plot the estimated values from the models .. the "fitted" values are the training values.
lines(fitted(fit1), col = "blue", type = "o")
lines(fitted(fit2), col = "red", type = "o")
lines(fitted(fit3), col = "green", type = "o")

# the  $mean values are the forecasts.
lines(fit1$mean, col = "blue", type = "o")
lines(fit2$mean, col = "red", type = "o")
lines(fit3$mean, col = "green", type = "o")

# These are the actual values!  Compare visually with the forecasts!
air2008 = window(ausair, start = 1990, end = 2007)
points(air2008, type = "o")

# Compare the forecasts with the actual values with various fit metrics.  
accuracy(fit1, air2008)
accuracy(fit2, air2008)
accuracy(fit3, air2008)


## Activity 2
#Activity 2: Holt LInear
#2 Holt's Linear Trend Model for AUS AIR
fit1h = holt(air, alpha = .8, beta = .2, initial = "simple", h = 10)
fit2h = holt(air, alpha = .8, beta = .2, initial = "simple", exponential = TRUE, h = 10)

# Check out estiamted values of the "training" data from the first holt model 
fitted(fit1h)
# Check out the forecast value (h of them)
fit1h$mean

# Reset the Plot!
plot(air,ylab = "Airline Passegners", xlab = "Year", main = "Airline Passengers by year", type = "o", xlim = c(1990, 2015),ylim = c(15,80))
#Plot each models estimated values of the training data (Do these one by one to see the differences)
lines(fitted(fit1h),col = "blue", type= "o")
lines(fitted(fit2h), col = "red", type= "o")
#Plot each models forecasts (Do these one by one to see the differences)
lines(fit1h$mean, col = "blue", type= "o")
lines(fit2h$mean,col = "red", type= "o")

# Fit another model ... damped!  
fit3h = holt(air, alpha = .8, beta = .2, damped = TRUE, initial = "optimal", h = 10)
# Plot the fitted value (estimated from triaining data)
lines(fitted(fit3h), col = "darkgreen", type= "o")
# Plot the forecasts
lines(fit3h$mean,col = "darkgreen", type= "o")

# Fit another model ... what is the difference?  
fit4h = holt(air, alpha = .8, beta = .2, damped = TRUE, initial = "optimal", exponential = TRUE, h = 10)
# Plot the fitted value (estimated from triaining data)
lines(fitted(fit4h), col = "cyan", type= "o")
#Plot the forecasts
lines(fit4h$mean,col = "cyan", type= "o")

# with implicit Test set... it figures out by the time which are training and which are test. 
accuracy(fit1h, ausair)
accuracy(fit2h, ausair)
accuracy(fit3h, ausair)

#with explicit Test set ... (same output)
airTest = window(ausair, start = 2005)
accuracy(fit1h, airTest)
accuracy(fit2h, airTest)
accuracy(fit3h, airTest)

#Add the actual values to visually compare forecasts to actual values
air2008 = window(ausair, start = 1990, end = 2009)
points(air2008, type = "o")


##Activity 2 

#2 Holt's Linear Trend Model for AUS AIR
fit1h = holt(air, alpha = .8, beta = .2, initial = "simple", h = 5)
fit2h = holt(air, alpha = .8, beta = .2, initial = "simple", exponential = TRUE, h = 5)

# Check out estiamted values of the "training" data from the first holt model 
fitted(fit1h)
# Check out the forecast value (h of them)
fit1h$mean

# Reset the Plot!
plot(air,ylab = "Airline Passegners", xlab = "Year", type = "o", xlim = c(1990, 2009),ylim = c(15,60))
#Plot each models estimated values of the training data (Do these one by one to see the differences)
lines(fitted(fit1h),col = "blue", type= "o")
lines(fitted(fit2h), col = "red", type= "o")
#Plot each models forecasts (Do these one by one to see the differences)
lines(fit1h$mean, col = "blue", type= "o")
lines(fit2h$mean,col = "red", type= "o")

# Fit another model ... damped!  
fit3h = holt(air, alpha = .8, beta = .2, damped = TRUE, initial = "optimal", h = 5)
# Plot the fitted value (estimated from triaining data)
lines(fitted(fit3h), col = "darkgreen", type= "o")
# Plot the forecasts
lines(fit3h$mean,col = "darkgreen", type= "o")

# Fit another model ... what is the difference?  
fit4h = holt(air, alpha = .8, beta = .2, damped = TRUE, initial = "optimal", exponential = TRUE, h = 5)
# Plot the fitted value (estimated from triaining data)
lines(fitted(fit4h), col = "cyan", type= "o")
#Plot the forecasts
lines(fit4h$mean,col = "cyan", type= "o")

# with implicit Test set... it figures out by the time which are training and which are test. 
accuracy(fit1h, ausair)
accuracy(fit2h, ausair)
accuracy(fit3h, ausair)

#with explicit Test set ... (same output)
airTest = window(ausair, start = 2005)
accuracy(fit1h, airTest)
accuracy(fit2h, airTest)
accuracy(fit3h, airTest)

#Add the actual values to visually compare forecasts to actual values
air2008 = window(ausair, start = 1990, end = 2009)
points(air2008, type = "o")

##ACtivity 3
#3. Seasonal Trend

#Load the data
data("austourists")
# Read about the dataset!
?austourists


# Always plot the data first!
plot(austourists)

# returns a ts object.  
aust = window(austourists,start = 1999, end = 2004)

#fit an additive and multiplicative model
fit1s = hw(aust,seasonal = "additive",h = 40)
fit2s = hw(aust,seasonal = "multiplicative",h = 40)

#Plot the original data
plot(aust,ylab = "Australian Tourists", xlab = "Year",main = "Holt-Winter's with a Additive Parameter", type = "o", xlim = c(1999, 2014),ylim = c(15,60))
#add the fitted values from the model (of the training data)
lines(fitted(fit1s),col = "blue", type= "o")
lines(fitted(fit2s), col = "red", type= "o")

#Now add the forecasts (add these one at a time)
lines(fit1s$mean, col = "blue", type= "o")
lines(fit2s$mean,col = "red", type= "o")

#Compare the accuracy
accuracy(fit1s,austourists)
accuracy(fit2s,austourists)

#add the actual values to visually compare the forecasts to the actual values. 
points(austourists, type = "o")


#Activity four
install.packages("fpp2")
library(fpp2)
maxtemp

#SES
data(maxtemp)

#returns a ts object
mtemp = window(maxtemp, start = 1990, end = 2016)

#creating train and test sets
train_set_ses <- window(mtemp, end = 2014)
test_set_ses <- window(mtemp, start= 2015)

# Always plot the data first! 
plot(test_set_ses,ylab = "Temperature", xlab = "Year", main = "Temperature over the years")

#fitting model
fit_act4 = ses(mtemp, initial = "optimal",alpha = .9,h = 4)

# the forecast package has a nice accuracy funciton with various metrics just pass it the 
# the model and the data!  (This is the "training" data)
accuracy(fit_act4, maxtemp)

#Reset the plot
plot(mtemp,ylab = "Temperature", xlab = "Year", type = "o", xlim = c(1990, 2025),ylim = c(30,50), main = "Max Temperature over the Years")

#Plot the estimated values from the models .. the "fitted" values are the training values.
lines(fitted(fit_act4), col = "blue", type = "o")

# the  $mean values are the forecasts.
lines(fit_act4$mean, col = "blue", type = "o")

#get AIC and BIC
fit_act4$model

##Holt LInear
#2 Holt's Linear Trend Model for AUS AIR
fit_h_mtemp = holt(mtemp, alpha = .8, beta = .2, damped = TRUE, initial = "optimal", exponential = TRUE, h = 5)

# Check out estiamted values of the "training" data from the first holt model 
fitted(fit_h_mtemp)
# Check out the forecast value (h of them)
fit_h_mtemp$mean

# Reset the Plot!
plot(mtemp,ylab = "Temperature", xlab = "Year", type = "o", xlim = c(1990, 2025),ylim = c(30, 50), main = "Temperautre over the years")
#Plot each models estimated values of the training data (Do these one by one to see the differences)
lines(fitted(fit_h_mtemp),col = "blue", type= "o")

#Plot each models forecasts (Do these one by one to see the differences)
lines(fit_h_mtemp$mean, col = "blue", type= "o")

fit_h_mtemp$model

#RMSE
#SES RMSE
fitted_ses = fitted(fit_act4)
rmse_ses = sqrt(mean((mtemp - fitted_ses)^2))
print(paste("RMSE_ses:", rmse_ses))

#Holt RMSE
fitted_holt = fitted(fit_h_mtemp)
rmse_holt = sqrt(mean((mtemp - fitted_holt)^2))
print(paste("RMSE_holt", rmse_holt))
