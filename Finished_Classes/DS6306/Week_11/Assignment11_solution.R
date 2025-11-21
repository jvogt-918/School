library("fpp2")
library("dygraphs")
library("xts")

maxtemp = fpp2::maxtemp

# Number 1: In this exercise, we will use Temperature data
autoplot(maxtemp, xlab="Year", ylab="Temperature (Celsius)")
ts<-window(maxtemp_ts, start=1990)
ts

autoplot(ts, xlab="Year", ylab="Temperature (Celsius)")


# Use SES
fit<-ses(ts, alpha = .05, h=5)

# Comparing Forecast Fit
plot(fit,ylab="Temperature, Celsius", xlab= "Year", main="Comparing forecast fit")
lines(fitted(fit), col="blue")
lines(fit$mean, col="blue", type="o")

# Finding AICc
fit$model


#Exta from Live Session: External Cross Validation
ts2<-window(maxtemp, start=1990, end = 2011)
fit2<-ses(ts2, alpha = .05, h=5)
accuracy(fit2,ts)

plot(fit2,ylab="Temperature, Celsius", xlab= "Year", main="Comparing forecast fit",ylim = c(36,47))
lines(fitted(fit2), col="blue")
lines(fit2$mean, col="blue", type="o")
points(maxtemp, type = "o")


# Use Holt Fit and damped
holtfit<- holt(ts, initial="optimal", h=5, damped = TRUE)

plot(holtfit, ylab="Temperature, Celsius", xlab= "Year", main="Comparing forecast fit")
lines(fitted(holtfit), col="blue", type="o")
lines(holtfit$mean, col="red")

# Finding AICc
holtfit$model

fit$model$bic
holtfit$model$bic
# The one with the Lowest AICc and BIC is SES

#External CV
ts2<-window(maxtemp, start=1990, end = 2011)
holtfit2<- holt(ts2, initial="optimal", h=5, damped = TRUE)
accuracy(holtfit2,ts)


accuracy(fit2,ts)# SES
accuracy(holtfit2,ts) #HW

# Holt Winters has a lower cross validation statistic (Test)

# Future Forecasts to 2021
plot(holtfit2,ylab="Temperature, Celsius", xlab= "Year", main="Comparing forecast fit",ylim = c(30,50))
lines(fitted(holtfit2), col="blue")
lines(holtfit2$mean, col="blue", type="o")
points(maxtemp, type = "o")









# You Try it... Evaluate a SES and HW model with AIC and BIC use this model to forecast the next 3 years
# Also evaluate external cross validation for 2011 to 2023
## Global Warming?

gT = read.csv(file.choose(), header = TRUE)
head(gT)
gT_ts = ts(gT$Actual,start = 1880, end = 2023)

# Number 1: In this exercise, we will use Temperature data
autoplot(gT_ts, xlab="Year", ylab="Temperature Diff)")
ts<-window(gT_ts, start=1880)
ts
autoplot(ts, xlab="Year", ylab="Temperature (Celsius)")


# Use SES
fit<-ses(ts, alpha = .05, h=3)

# Comparing Forecast Fit
plot(fit,ylab="Temperature, Celsius", xlab= "Year", main="Comparing forecast fit")
lines(fitted(fit), col="blue")
lines(fit$mean, col="blue", type="o")

# Finding AICc
fit$model

#Exta from Live Session: External Cross Validation
ts2<-window(gT_ts, start=1880, end = 2010)
fit2<-ses(ts2, alpha = .05, h=11)
accuracy(fit2,ts)

plot(fit2,ylab="Temperature, Celsius", xlab= "Year", main="Comparing forecast fit",ylim = c(-1,1))
lines(fitted(fit2), col="blue")
lines(fit2$mean, col="blue", type="o")
points(gT_ts, type = "o")


# Use Holt Fit and damped
holtfit<- holt(ts, initial="optimal", h=3, damped = FALSE)

plot(holtfit, ylab="Temperature, Celsius", xlab= "Year", main="Comparing forecast fit")
lines(fitted(holtfit), col="blue", type="o")
lines(holtfit$mean, col="red")

# Finding AICc
holtfit$model

# The one with the Lowest AICc and BIC is SES

#External CV
ts2<-window(gT_ts, start=1880, end = 2010)
holtfit2<- holt(ts2, initial="optimal", h=13, damped = FALSE)
accuracy(holtfit2,ts)

plot(ts, ylab="Temperature, Dif", xlab= "Year", main="Comparing forecast fit")
lines(fitted(holtfit2), col="blue", type="l", lwd = 2)
lines(holtfit2$mean, col="red")
lines(fit2$mean, col="green")
points(gT_ts, type = "l")

# Finding AICc
holtfit$model


#External CV
accuracy(fit2,ts)
accuracy(holtfit2,ts)

# Holt Winters has a lower cross validation statistic (Test)

plot(holtfit,ylab="Temperature, Celsius", xlab= "Year", main="Comparing forecast fit",ylim = c(-1,1.5))
lines(fitted(holtfit), col="blue")
lines(holtfit$mean, col="blue", type="o")
points(gT_ts, type = "o")







# Number 2: Harry Potter

#Ollivander
df<-read.csv(file.choose(), header=FALSE)

df$V1<-as.Date(df$V1, format="%m/%d/%Y")
df<-xts(df$V2, order.by=df$V1)

#Gregorovitch
df2<-read.csv(file.choose(), header=FALSE)

df2$V1<-as.Date(df2$V1, format="%m/%d/%Y")
df2<-xts(df2$V2, order.by=df2$V1)

wands<-cbind(df, df2)

dygraph(wands, main="Wand Maker Sales across the past decades", ylab="Wands Sold", xlab="Year") %>%
  dySeries(label = "Ollivander") %>%
  dySeries(label = "Gregorovitch") %>%
  dyOptions(stackedGraph = TRUE, colors = RColorBrewer::brewer.pal(n=3, "Dark2")) %>%
  dyRangeSelector(height = 100) %>%
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
  dyShading(from = "1995-1-1", to = "1999-1-1", color = "#ffd3d3")
