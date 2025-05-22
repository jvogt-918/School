#question 3
t.test(EducationData$Income2005 ~ EducationData$Educ)


library(ggplot2)
# Create histogram for each education group
ggplot(EducationData, aes(x = Income2005)) +
  geom_histogram(fill = "steelblue", color = "black") +
  facet_wrap(~ Educ, nrow = 1) +
  labs(
    title = "Histogram of 2005 Income by Education Group",
    x = "Income in 2005",
    y = "Count"
  ) +
  theme_minimal()

EducationData$log_Income2005 <- log(EducationData$Income2005)

EducationData

# Create histogram for each education group
ggplot(EducationData, aes(x = log_Income2005)) +
  geom_histogram(fill = "steelblue", color = "black") +
  facet_wrap(~ Educ, nrow = 1) +
  labs(
    title = "Histogram Log'd Income by Education Group",
    x = "Income in 2005",
    y = "Count"
  ) +
  theme_minimal()

t.test(EducationData$log_Income2005 ~ EducationData$Educ, var.equal = TRUE)


#Unit 3 HW

fired_samoan = c(34, 37, 37, 38, 41, 42, 43, 44, 44, 45, 45, 45, 46, 48, 49, 53, 53, 54, 54, 55, 56)
nonfired_samoan = c(27, 33, 36, 37, 38, 38, 39, 42, 42, 43, 43, 44, 44, 44, 45, 45, 45, 45, 46, 46, 47, 47, 48, 48, 49, 49, 51, 51, 52, 54)

t.test(x = fired_samoan, y = nonfired_samoan, conf.int = .95, var.equal = FALSE, alternative = "two.sided")


ggplot(EducationData, aes(x = Income2005)) +
  geom_histogram(fill = "steelblue", color = "black") +
  facet_wrap(~ Educ, nrow = 1) +
  labs(
    title = "Histogram Log'd Income by Education Group",
    x = "Income in 2005",
    y = "Count"
  ) +
  theme_minimal()
EducationData$log_Income2005 <- log(EducationData$Income2005)

ggplot(EducationData, aes(x = log_Income2005)) +
  geom_histogram(fill = "steelblue", color = "black") +
  facet_wrap(~ Educ, nrow = 1) +
  labs(
    title = "Histogram Log'd Income by Education Group",
    x = "Income in 2005",
    y = "Count"
  ) +
  theme_minimal()

tapply(EducationData$log_Income2005, EducationData$Educ, sd, na.rm=TRUE)

power.t.test(EducationData$log_Income2005 ~ EducationData$Educ, var.equal = FALSE)

power.t.test(EducationData$Income2005 ~ EducationData$Educ, var.equal = FALSE)
power.t.test(
  power = 0.8,
  delta = 33132.07,
  sd = 22687,
  sig.level = 0.05,
  type = "two.sample",
  alternative = "two.sided"
)

power.t.test(n = 250, delta = 0, sd = 25, sig.level = .05, type = "two.sample", alternative = "one.sided")

#ritalin
library(effectsize)
install.packages("effectsize")

sd_pooled(twitch$Placebo, twitch$Ritalin)
power.t.test(n = 41, delta = .094475, sd = .119, sig.level = .05, type = "two.sample", alternative = "two.sided")
power.t.test(n = 41, delta = .07, sd = .2, sig.level = .05, type = "two.sample", alternative = "two.sided")
 
#Sample Size Power Currrve
powerholder = c()
samplesizes = seq(10,200, length = 20)

for(i in 1:20)
{
  powerholder[i] = power.t.test(n = samplesizes[i], delta=.07, sd =.2, sig.level = .05, type = "two.sample", alternative = "two.sided")$power
}

plot(samplesizes, powerholder, type = "l",col = "blue", main = "POWER CURVE", ylab = "POWER", lwd = 3 )

install.packages("MKpower")
library(MKpower)
power.t.test(n = c(24, 23), delta = 3, 
             sd = sqrt((4.4395^2 + 5.2526^2)/2),
             sig.level = 0.05,
             type = "two.sample")

power.welch.t.test(n = c(24,23), delta = 3, 
                   sd1 = 4.5,
                   sig.level = 0.05)
power.welch.t.test()


#Effect Size
powerholder = c()
effectsizes = seq(1,5,length = 20)

for(i in 1:20)
{
  powerholder[i] = power.t.test(n = c(24,23),delta = effectsizes[i], sd = 4.5, sig.level = .05, type = "two.sample",alternative = "two.sided")$power
}

plot(effectsizes,powerholder,type = "l", col = "blue", main = "POWER CURVE", ylab = "POWER", lwd = 3)
