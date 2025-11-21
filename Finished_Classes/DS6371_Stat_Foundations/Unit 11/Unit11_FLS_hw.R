#Unit 11
install.packages("WDI")
library(tidyverse)
library(WDI)
WDIsearch("infant") # yields the indicator: SP.DYN.IMRT.IN
WDIsearch("GDP") # yields the indicator: NY.GDP.PCAP.CD
InfantMort = WDI(,"SP.DYN.IMRT.IN",start = 2019, end = 2019)
GDPs = WDI(,"NY.GDP.PCAP.CD",start = 2019, end = 2019)
InfantVGDP = inner_join(GDPs,InfantMort,"country")
InfantVGDP = InfantVGDP[,c(1,2,3,4,6)]
colnames(InfantVGDP) = c("iso2C","country","GDP","year","InfantMort")
InfantVGDP

#Post-Fix
InfantMort = WDI(,"SP.DYN.IMRT.IN",start = 2019, end = 2019)
GDPs = WDI(,"NY.GDP.PCAP.CD",start = 2019, end = 2019)
InfantVGDP = inner_join(GDPs,InfantMort, "country")
InfantVGDP = InfantVGDP %>% select("country","NY.GDP.PCAP.CD","SP.DYN.IMRT.IN")
colnames(InfantVGDP) = c("country","GDP","InfantMort")
InfantVGDP

ggplot(data = InfantVGDP, aes(x = GDP, y = InfantMort)) +
  geom_point() +
  ggtitle("Normal Plot (No Logs)")

InfantVGDP$log_gdp <- log(InfantVGDP$GDP)
InfantVGDP$log_IF <- log(InfantVGDP$InfantMort)

ggplot(data = InfantVGDP, aes(x = log_gdp, y = InfantMort)) +
  geom_point() +
  ggtitle("Normal Plot (GDP Logged)")

ggplot(data = InfantVGDP, aes(x = log_gdp, y = log_IF)) +
  geom_point() +
  ggtitle("Normal Plot (Both Logs)") +
  geom_smooth(method = "lm", se = TRUE, color = "red")


print(InfantVGDP[,c("country", "InfantMort")])
 
InfantVGDP[InfantVGDP$country == "United States",]

fitloglog = lm(log(InfantMort)~log(GDP), data = InfantVGDP)

summary(fitloglog)

confint(fitloglog)



#Unit 11 Homework
#Gathering Data
autism <- read.csv(file.choose())
autism

#transform the data
autism$log_prev <- log(autism$Prevalence)
autism$log_year <- log(autism$Year)

#Model
aut_model <- lm(Prevalence ~ Year, data = autism)
aut_log_lin_model <- lm(log_prev ~ Year, data = autism)

#Confidence and Prediction Model
aut_conf <- predict(aut_model, newdata = autism, interval = "confidence")
aut_pred <- predict(aut_model, newdata = autism, interval = "prediction")

aut_loglin_conf <- predict(aut_log_lin_model, newdata = autism, interval = "confidence")
aut_loglin_pred <- predict(aut_log_lin_model, newdata = autism, interval = "prediction")

#plotting the data
library(ggplot2)
aut_plot <- cbind(autism, aut_conf, aut_pred = aut_pred[,"upr"], aut_pred_lwr = aut_pred[,"lwr"])
aut_loglin_plot <- cbind(autism, aut_loglin_conf, aut_loglin_pred = aut_loglin_pred[,"upr"], aut_loglin_pred_lwr = aut_loglin_pred[,"lwr"])

ggplot(autism, aes(x= Year, y = Prevalence)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue") +
  geom_line(data = aut_plot, aes(y = fit), color = "blue") +
  geom_ribbon(data = aut_plot, aes(ymin = lwr, ymax = upr), alpha = 0.2, fill = "blue") +
  geom_ribbon(data = aut_plot, aes(ymin = aut_pred_lwr, ymax = aut_pred), alpha = 0.1, fill = "red") +
  ggtitle("Year vs. Autism Prevalence") +
  ylab("Prevalence per 10,000")

ggplot(autism, aes(x= Year, y = log_prev)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue") +
  geom_line(data = aut_loglin_plot, aes(y = fit), color = "blue") +
  geom_ribbon(data = aut_loglin_plot, aes(ymin = lwr, ymax = upr), alpha = 0.2, fill = "blue") +
  geom_ribbon(data = aut_loglin_plot, aes(ymin = aut_loglin_pred_lwr, ymax = aut_loglin_pred), alpha = 0.1, fill = "red") +
  ggtitle("Year vs. Log'd Autism Prevalence") +
  ylab("Prevalence per 10,000")

#Resdiuals and Residual Plot
autism$residuals <- resid(aut_model)
autism$fitted <- fitted(aut_model)

autism$loglin_resid <- resid(aut_loglin_plot)
autism$loglin_fit <- fitted(aut_loglin_plot)

ggplot(autism, aes(x = fitted, y = residuals)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", x = "Fitted Values", y = "Residuals")

ggplot(autism, aes(x = loglin_fit, y = loglin_resid)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", x = "Fitted Values", y = "Residuals")

#Histogram with Normal Curve
ggplot(autism, aes(x=residuals)) +
  geom_histogram(aes(y= ..density..), bins = 5, fill = "lightgray", color = "black", alpha = 0.7) +
  stat_function(
    fun = dnorm,
    args = list(mean = mean(autism$residuals), sd = sd(autism$residuals)),
    color = "blue",
    size = 1.2
  )

# Extract residuals from the model
autism$residuals <- resid(model)
df <- data.frame(residuals = autism$residuals)

ggplot(df, aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), bins = 5, fill = "lightgray", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(df$residuals), sd = sd(df$residuals)), color = "blue", size = 1.2) +
  labs(title = "Histogram of Residuals with Normal Curve", x = "Residuals", y = "Density")

autism$loglin_resid <- resid(aut_log_lin_model)
loglin_df <- data.frame(residuals = autism$loglin_resid)

ggplot(loglin_df, aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), bins = 6, fill = "lightgray", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(loglin_df$residuals), sd = sd(loglin_df$residuals)), color = "blue", size = 1.2) +
  labs(title = "Histogram of Residuals with Normal Curve from Log'd Values", x = "Residuals", y = "Density")

#summary of chosen model
summary(aut_log_lin_model)

#confidence interval of the model
confint(aut_log_lin_model)

#R-Squared
summary(aut_log_lin_model)$r.squared
