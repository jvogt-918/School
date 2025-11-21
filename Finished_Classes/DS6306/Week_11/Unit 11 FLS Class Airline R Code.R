#Classic Airline Analysis with Seasonal HW

AP1957 = window(AirPassengers, end = c(1957,12))
plot(AP1957,ylab = "Airline Passegners", xlab = "Year", main = "Airline Passengers")

fit1s = hw(AP1957,seasonal = "additive",h = 36)
fit2s = hw(AP1957,seasonal = "multiplicative",h = 36)

plot(AP1957,ylab = "Airline Passengers", xlab = "Year", type = "o", xlim = c(1949, 1961),ylim = c(100,700))
lines(fit1s$mean, col = "blue", type= "o")
points(AirPassengers, type = "o")

plot(AP1957,ylab = "Airline Passengers", xlab = "Year", type = "o", xlim = c(1949, 1961),ylim = c(100,700))
lines(fit2s$mean,col = "red", type= "o")
points(AirPassengers, type = "o")

# The Test is the holdout data for the 3 years from 1958 to 1960
accuracy(fit1s,AirPassengers)
accuracy(fit2s,AirPassengers)



#Ensemble
fit3s = (fit1s$mean+fit2s$mean) / 2
accuracy(fit1s,AirPassengers)
accuracy(fit2s,AirPassengers)
accuracy(fit3s,AirPassengers)

plot(AP1957,ylab = "Airline Passengers", xlab = "Year", type = "o", xlim = c(1949, 1961),ylim = c(100,700))
points(AirPassengers, type = "o")
lines(fit3s,col = "green", type= "o")

